const std = @import("std");
const AST = @import("../ast/ast.zig").AST;
const Compiler_Context = @import("../hierarchy/compiler.zig");
const core_ = @import("../hierarchy/core.zig");
const Interpreter_Context = @import("../interpretation/interpreter.zig");
const lval_ = @import("../ir/lval.zig");
const repo_ = @import("../util/repo.zig");
const String = @import("../zig-string/zig-string.zig").String;

const Error: type = error{ LexerError, ParseError, CompileError, FileNotFound, OutOfMemory };

const Package_Find_Result = struct { package_adrs: i64, package_dirname: []const u8 };

/// Implements the Package::find method at build-time. Takes in a string representing the name of
/// the package in the Orange cache, and returns an AST representing the package.
pub fn package_find(compiler: *Compiler_Context, interpreter: *Interpreter_Context, current_module_path: []const u8, package_src: *AST) Error!Package_Find_Result {
    // Construct the path to the package's `build.orng` file
    const package_absolute_path = switch (package_src.pos().?) {
        0 => try package_find_relative(current_module_path, package_src.enum_value.init.?, compiler.allocator()),
        1 => try package_find_git(compiler, package_src.enum_value.init.?),
        else => unreachable,
    };

    const package_build_paths = [_][]const u8{ package_absolute_path, "build.orng" };
    const package_build_path = std.fs.path.join(compiler.allocator(), &package_build_paths) catch unreachable;

    const path_buffer = compiler.allocator().alloc(u8, std.fs.max_path_bytes) catch unreachable;
    const package_build_absolute_path = std.fs.cwd().realpath(package_build_path, path_buffer) catch return error.CompileError;

    // Compile the package's `build.orng` file
    const build_cfg = try compiler.compile_build_file(package_build_absolute_path);
    interpreter.load_module(build_cfg.symbol.scope.module.?);

    // Allocate space for the package to be placed
    const package_len: usize = @intCast(core_.package_type.sizeof().?);
    const adrs: i64 = @intCast(try interpreter.alloc(@intCast(package_len), 8));
    const retval_place = lval_.L_Value.create_raw_address_lval(adrs, compiler.allocator());

    // Jump to the `build()` fn
    try interpreter.call(build_cfg.symbol, retval_place, std.array_list.Managed(*lval_.L_Value).init(compiler.allocator()));
    try interpreter.run();

    return .{ .package_adrs = adrs, .package_dirname = package_absolute_path };
}

fn package_find_relative(current_module_path: []const u8, package_src: *AST, allocator: std.mem.Allocator) Error![]const u8 {
    const package_path = package_src.string.data;

    const current_package = std.fs.path.dirname(current_module_path).?;
    const required_package_paths = [_][]const u8{ current_package, package_path };
    const required_package_path = std.fs.path.join(allocator, &required_package_paths) catch unreachable;

    const package_buffer = allocator.alloc(u8, std.fs.max_path_bytes) catch unreachable;
    const retval = std.fs.cwd().realpath(required_package_path, package_buffer) catch return error.CompileError;
    return retval;
}

/// Implements `core::Package::find` for the git variant
fn package_find_git(compiler: *Compiler_Context, package_src: *AST) Error![]const u8 {
    const git_source_type = compiler.get_core_type("Git_Source");
    const url = package_src.get_field(git_source_type, "url");
    const subdir = package_src.get_field(git_source_type, "subdir");

    try repo_.git_clone(url.string.data, compiler.allocator());

    const repo_dir = repo_.get_packages_dir(compiler.allocator());
    var dirs = std.array_list.Managed([]const u8).init(compiler.allocator());
    defer dirs.deinit();
    dirs.append(repo_dir) catch unreachable;
    dirs.append(std.fs.path.basename(url.string.data)) catch unreachable;
    if (subdir.string.data.len > 0) {
        // This has to be done so that package absolute paths have one canonical representation, even on Windows
        var subdir_string = String.init_with_contents(compiler.allocator(), subdir.string.data) catch unreachable;
        const os_sep: [1]u8 = [1]u8{std.fs.path.sep};
        _ = subdir_string.replace("/", &os_sep) catch unreachable;
        dirs.append((subdir_string.toOwned() catch unreachable).?) catch unreachable;
    }
    return std.fs.path.join(compiler.allocator(), dirs.items) catch unreachable;
}
