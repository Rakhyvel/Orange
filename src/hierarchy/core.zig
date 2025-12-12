///! This file is responsible for interacting with the core library. The core library is a library that the compiler
/// knows through this file, but are not as ubiquitous as to be placed in the prelude. It includes traits such as
/// Allocator, Writer, Reader.
const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const module_ = @import("../hierarchy/module.zig");
const prelude_ = @import("prelude.zig");
const repo_ = @import("../util/repo.zig");
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const Token = @import("../lexer/token.zig");
const UID_Gen = @import("../util/uid_gen.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

// TODO: Think about how to remove these public variables. They're only variable because they need to be constructed later.
// Maybe a map?
pub var package_type: *Type_AST = undefined;
pub var package_source_type: *Type_AST = undefined;
pub var range_type: *Type_AST = undefined;
pub var test_result_type: *Type_AST = undefined;
pub var test_type: *Type_AST = undefined;

// Traits
pub var into_iterator_trait: *Symbol = undefined;

// Contexts
pub var allocating_context: *Type_AST = undefined;
pub var io_context: *Type_AST = undefined;
pub var args_context: *Type_AST = undefined;
pub var file_io_context: *Type_AST = undefined;

var core: ?*Scope = null;
pub var core_symbol: ?*Symbol = null;
pub var core_package_name: []const u8 = undefined;
pub fn get_core_scope(compiler: *Compiler_Context) !*Scope {
    if (core == null) {
        try create_core(compiler);
    }
    return core.?;
}

pub fn deinit() void {
    core = null;
}

fn create_core(compiler: *Compiler_Context) !void {
    // Create core scope
    var uid_gen = UID_Gen.init();

    repo_.ensure_packages_dir_exists(compiler.allocator());
    core_package_name = repo_.get_repo_dir("core", compiler.allocator());
    _ = std.fs.openDirAbsolute(core_package_name, .{}) catch {
        std.fs.makeDirAbsolute(core_package_name) catch unreachable;
    };

    var core_module_abs_path = std.array_list.Managed(u8).init(compiler.allocator());
    try core_module_abs_path.print("{s}{c}core.orng", .{ core_package_name, std.fs.path.sep });

    const module = try module_.Module.init(try core_module_abs_path.toOwnedSlice(), compiler.allocator());
    core = Scope.init(compiler.prelude, &uid_gen, compiler.allocator());
    core_symbol = Symbol.init(
        compiler.prelude,
        "core",
        ast_.AST.create_module(
            Token.init_simple("core"),
            core.?,
            module,
            compiler.allocator(),
        ),
        .module,
        .local,
        compiler.allocator(),
    );
    try compiler.prelude.put_symbol(core_symbol.?, &compiler.errors);
    core.?.module = module;

    const contents: []const u8 = @embedFile("core.orng");
    try module_.Module.fill_contents(
        contents,
        "core.orng",
        null,
        core.?,
        module,
        core_symbol.?,
        false,
        compiler,
    );

    const module_scope = compiler.module_scope(module.absolute_path).?;
    package_type = module_scope.lookup("Package", .{}).found.init_typedef().?;
    package_source_type = module_scope.lookup("Package_Source", .{}).found.init_typedef().?;
    range_type = module_scope.lookup("Range", .{}).found.init_typedef().?;
    test_result_type = module_scope.lookup("Test_Result", .{}).found.init_typedef().?;
    const args = std.array_list.Managed(*Type_AST).init(compiler.allocator());
    test_type = Type_AST.create_function(
        test_result_type.token(),
        args,
        test_result_type,
        std.array_list.Managed(*Type_AST).init(compiler.allocator()),
        compiler.allocator(),
    );

    into_iterator_trait = module_scope.lookup("Into_Iterator", .{}).found;

    allocating_context = module_scope.lookup("Allocating", .{}).found.init_typedef().?;
    io_context = module_scope.lookup("IO", .{}).found.init_typedef().?;
    args_context = module_scope.lookup("Args", .{}).found.init_typedef().?;
    file_io_context = module_scope.lookup("File_IO", .{}).found.init_typedef().?;

    _ = module_scope.lookup("Requirement", .{}).found.init_typedef().?;
}
