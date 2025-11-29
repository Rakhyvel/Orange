const std = @import("std");
const core_ = @import("../hierarchy/core.zig");
const Module_Iterator = @import("../util/dfs.zig").Dfs_Iterator(*Module);
const Dependency_Node = @import("../types/dependency_node.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Compiler_Context = @import("../hierarchy/compiler.zig");
const Interned_String_Set = @import("../ir/interned_string_set.zig");
const Module = @import("../hierarchy/module.zig").Module;
const Header_Emitter = @import("header_emitter.zig");
const Source_Emitter = @import("source_emitter.zig");
const Emitter = @import("emitter.zig");
const Test_Emitter = @import("test_emitter.zig");
const Typedef_Emitter = @import("typedef_emitter.zig");
const Type_Map = @import("../types/type_map.zig").Type_Map;
const Type_Set = @import("../types/type_set.zig");
const Canonical_Type_Fmt = @import("../types/canonical_type_fmt.zig");

/// Goes through each package and outputs a C/H file header pair for each module in each package.
///
/// Does not output packages that are not modified.
pub fn output_modules(compiler: *Compiler_Context) !void {
    // Start from root module, of each package, DFS through imports and generate
    for (compiler.packages.keys()) |package_name| {
        const package = compiler.lookup_package(package_name).?;
        const package_root_module = package.root.init_value().?.module.module;

        if (!package.modified.?) {
            continue;
        }

        const build_path = package.get_build_path(compiler.allocator());
        _ = std.fs.openDirAbsolute(build_path, .{}) catch {
            std.fs.makeDirAbsolute(build_path) catch unreachable;
        };

        const types_path = package.get_types_path(compiler.allocator());
        _ = std.fs.openDirAbsolute(types_path, .{}) catch {
            std.fs.makeDirAbsolute(types_path) catch unreachable;
        };

        try output_types(&package.type_set, types_path, compiler.allocator());

        var modules = std.array_list.Managed(*Module).init(compiler.allocator());
        defer modules.deinit();
        var dfs_iter: Module_Iterator = Module_Iterator.init(package_root_module, compiler.allocator());
        defer dfs_iter.deinit();
        while (dfs_iter.next()) |module| {
            if (std.mem.eql(u8, module.get_package_abs_path(), package.absolute_path)) {
                try output_module(module, &compiler.module_interned_strings, build_path, compiler.allocator());
                try modules.append(module);

                if (package.kind == .test_executable) {
                    try output_tests(module, &compiler.module_interned_strings, build_path, compiler.allocator());
                }
            }
        }

        switch (package.kind) {
            .test_executable => try output_testrunner(modules, build_path, compiler.allocator()),
            .executable => try output_start(package_root_module, &compiler.module_interned_strings, build_path, compiler.allocator()),
            else => {},
        }
    }
}

/// Takes in a statically correct module, writes it out to C source and header files
fn output_module(
    module: *Module,
    module_interned_strings: *const std.AutoArrayHashMap(u32, *Interned_String_Set),
    build_path: []const u8,
    allocator: std.mem.Allocator,
) !void {
    var output_h_file = try open_file(module.package_name, module.name(), ".h", build_path, allocator);
    defer output_h_file.close();
    var h_buffer = std.array_list.Managed(u8).init(allocator);
    defer h_buffer.deinit();
    var header_emitter = Header_Emitter.init(module, &h_buffer);
    header_emitter.generate() catch return error.CompileError;

    var output_c_file = try open_file(module.package_name, module.name(), ".c", build_path, allocator);
    defer output_c_file.close();
    var c_buffer = std.array_list.Managed(u8).init(allocator);
    defer c_buffer.deinit();
    var source_emitter = Source_Emitter.init(module, module_interned_strings, &c_buffer);
    source_emitter.generate() catch return error.CompileError;

    output_c_file.writeAll(c_buffer.items) catch return error.WriteFailed;
    output_h_file.writeAll(h_buffer.items) catch return error.WriteFailed;
}

fn output_types(
    type_set: *const Type_Set,
    types_path: []const u8,
    allocator: std.mem.Allocator,
) !void {
    // Output all typedefs
    for (type_set.types.items) |dag| {
        try output_type(dag, types_path, allocator);
    }
}

fn output_type(
    dep: *Dependency_Node,
    types_path: []const u8,
    allocator: std.mem.Allocator,
) !void {
    if (dep.visited) {
        // only visit a DAG node once
        return;
    }
    dep.mark_visited();

    // output any types that this type depends on
    for (dep.dependencies.items) |depen| {
        try output_type(depen, types_path, allocator);
    }

    if (dep.base.sizeof() == 0) {
        // Don't output zero-sized types
        return;
    }

    var output_filename = std.array_list.Managed(u8).init(allocator);
    defer output_filename.deinit();
    try output_filename.print("{f}.h", .{Canonical_Type_Fmt{ .type = dep.base }});

    const out_paths = [_][]const u8{ types_path, output_filename.items };
    const out_path = try std.fs.path.join(allocator, &out_paths);

    const output_type_h_file = std.fs.createFileAbsolute(out_path, .{}) catch unreachable;
    var type_h_buffer = std.array_list.Managed(u8).init(allocator);
    defer type_h_buffer.deinit();

    var test_emitter = Typedef_Emitter.init(dep, &type_h_buffer);
    test_emitter.generate() catch return error.CompileError;

    output_type_h_file.writeAll(type_h_buffer.items) catch return error.WriteFailed;
}

/// Takes in a statically correct module, writes the tests out to C source and header files
fn output_tests(
    module: *Module,
    module_interned_strings: *const std.AutoArrayHashMap(u32, *Interned_String_Set),
    build_path: []const u8,
    allocator: std.mem.Allocator,
) !void {
    var output_h_file = try open_file(module.package_name, module.name(), "-tests.h", build_path, allocator);
    defer output_h_file.close();
    var h_buffer = std.array_list.Managed(u8).init(allocator);
    defer h_buffer.deinit();

    var output_c_file = try open_file(module.package_name, module.name(), "-tests.c", build_path, allocator);
    defer output_c_file.close();
    var c_buffer = std.array_list.Managed(u8).init(allocator);
    defer c_buffer.deinit();

    var test_emitter = Test_Emitter.init(module, module_interned_strings, &c_buffer, &h_buffer);
    test_emitter.generate() catch return error.CompileError;

    output_c_file.writeAll(c_buffer.items) catch return error.WriteFailed;
    output_h_file.writeAll(h_buffer.items) catch return error.WriteFailed;
}

fn output_start(
    module: *Module,
    module_interned_strings: *const std.AutoArrayHashMap(u32, *Interned_String_Set),
    build_path: []const u8,
    allocator: std.mem.Allocator,
) !void {
    const paths = [_][]const u8{ build_path, "start.c" };
    const path = try std.fs.path.join(allocator, &paths);

    var start_file = std.fs.createFileAbsolute(path, .{}) catch unreachable;
    defer start_file.close();

    var buf = std.array_list.Managed(u8).init(allocator);
    defer buf.deinit();

    var source_emitter = Source_Emitter.init(
        module,
        module_interned_strings,
        &buf,
    );
    source_emitter.output_header_include() catch return error.CompileError;
    buf.print("#include <stdio.h>\n#include \"debug.inc\"\n\n", .{}) catch return error.CompileError;

    var contexts_used = Type_Map(void).init(allocator);
    defer contexts_used.deinit();
    for (module.cfgs.items) |@"test"| {
        contexts_used.put_many(@"test".symbol.type().function.contexts.items, void{}) catch return error.CompileError;
    }
    source_emitter.emitter.output_context_includes(&contexts_used) catch return error.CompileError;

    source_emitter.output_main_function() catch return error.CompileError;

    start_file.writeAll(buf.items) catch return error.WriteFailed;
}

fn output_testrunner(
    modules: std.array_list.Managed(*Module),
    build_path: []const u8,
    allocator: std.mem.Allocator,
) !void {
    const paths = [_][]const u8{ build_path, "test-runner.c" };
    const path = try std.fs.path.join(allocator, &paths);

    var testrunner_file = std.fs.createFileAbsolute(path, .{}) catch unreachable;
    defer testrunner_file.close();

    var buf = std.array_list.Managed(u8).init(allocator);
    defer buf.deinit();

    buf.print(
        \\/* Code generated using the Orange compiler http://ornglang.org */
        \\
        \\#include <stdio.h>
        \\#include <stdlib.h>
        \\#include <string.h>
        \\
        \\#include "debug.inc"
        \\
        \\
    , .{}) catch return error.CompileError;

    for (modules.items) |module| {
        try buf.print("#include \"{s}-{s}-tests.h\"\n", .{ module.package_name, module.name() });
    }

    var mod_0_emitter = Emitter.init(&buf);

    var contexts_used = Type_Map(void).init(allocator);
    defer contexts_used.deinit();
    for (modules.items) |module| {
        for (module.tests.items) |@"test"| {
            contexts_used.put_many(@"test".symbol.type().function.contexts.items, void{}) catch return error.CompileError;
        }
    }
    mod_0_emitter.output_context_includes(&contexts_used) catch return error.CompileError;

    buf.print(
        \\
        \\typedef 
    , .{}) catch return error.CompileError;
    mod_0_emitter.output_type(core_.test_result_type) catch return error.CompileError;
    buf.print(
        \\ test_result;
        \\
    , .{}) catch return error.CompileError;

    buf.print(
        \\
        \\int main(int argc, char *argv[])
        \\{{
        \\    const char *substr = NULL;
        \\    const char *prev_filename = NULL;
        \\    test_result res;
        \\    int failed_tests = 0;
        \\    
    , .{}) catch return error.CompileError;
    mod_0_emitter.output_context_defs(&contexts_used) catch return error.CompileError;
    buf.print(
        \\    if (argc >= 2)
        \\    {{
        \\        substr = argv[1];
        \\    }}
        \\
        \\    printf("[============]\n");
        \\
    , .{}) catch return error.CompileError;

    for (modules.items) |module| {
        var emitter = Emitter.init(&buf);
        for (module.tests.items) |@"test"| {
            const test_filename = @"test".symbol.scope.module.?.name();
            const test_name = @"test".symbol.decl.?.@"test".name.?.string.data;

            buf.print(
                \\    if (substr == NULL || strstr("{0s}.orng: {1s}", substr))
                \\    {{
                \\        if (prev_filename == NULL)
                \\        {{
                \\            prev_filename = "{0s}";
                \\        }}
                \\        else if (strcmp(prev_filename, "{0s}"))
                \\        {{
                \\            prev_filename = "{0s}";
                \\            printf("[------------]\n");
                \\        }}
                \\        printf("[ RUN        ] %s.orng: %s\n", "{0s}", "{1s}");
                \\        res = 
            , .{ test_filename, test_name }) catch return error.CompileError;
            emitter.output_symbol(@"test".symbol) catch return error.CompileError;
            buf.print("(", .{}) catch return error.CompileError;
            emitter.output_context_args(@"test".symbol.type().function.contexts.items) catch return error.CompileError;
            buf.print(");\n", .{}) catch return error.CompileError;
            buf.print(
                \\        if (res.tag == 0)
                \\        {{
                \\            printf("[         OK ]\n");
                \\        }}
                \\        else
                \\        {{
                \\            orange_debug__dump_error_trace("unhandled error");
                \\            printf("[     FAILED ]\n");
                \\            failed_tests += 1;
                \\        }}
                \\    }}
                \\
            , .{}) catch return error.CompileError;
        }
    }
    buf.print(
        \\    printf("[============]\n");
        \\    return failed_tests;
        \\}}
        \\
    , .{}) catch return error.CompileError;

    testrunner_file.writeAll(buf.items) catch return error.WriteFailed;
}

fn open_file(package_name: []const u8, module_name: []const u8, ext: []const u8, build_path: []const u8, allocator: std.mem.Allocator) !std.fs.File {
    var output_filename = std.array_list.Managed(u8).init(allocator);
    defer output_filename.deinit();
    try output_filename.print("{s}-{s}{s}", .{ package_name, module_name, ext });

    const out_paths = [_][]const u8{ build_path, output_filename.items };
    const out_path = try std.fs.path.join(allocator, &out_paths);

    const output_file = std.fs.createFileAbsolute(out_path, .{}) catch unreachable;
    return output_file;
}
