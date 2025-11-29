const std = @import("std");
const Module = @import("../hierarchy/module.zig").Module;
const CFG = @import("../ir/cfg.zig");
const Compiler_Context = @import("compiler.zig");
const Symbol = @import("../symbol/symbol.zig");
const repo_ = @import("../util/repo.zig");
const Module_Hash = @import("module_hash.zig");
const Type_Set = @import("../types/type_set.zig");

const Package = @This();

name: []const u8,
absolute_path: []const u8,
output_absolute_path: []const u8,
root: *Symbol,
local_modules: std.array_list.Managed(*Module),
/// Maps the names of required packages, relative to this package, to the symbol of their root module
requirements: std.StringArrayHashMap(*Symbol),
include_directories: std.StringArrayHashMap(void),
library_directories: std.StringArrayHashMap(void),
libraries: std.StringArrayHashMap(void),
kind: Package_Kind,
visited: bool,
module_hash: Module_Hash,
/// The C-equivalence type-closure for this package, built from the types used in all the package's modules
type_set: Type_Set,
modified: ?bool,

pub const Package_Kind = enum {
    executable,
    static_library,
    shared_library,
    test_executable,
};

pub const Package_Iterator_Node = struct {
    compiler: *Compiler_Context,
    package: *Package,

    pub fn init(compiler: *Compiler_Context, package_abs_path: []const u8) Package_Iterator_Node {
        const package = compiler.lookup_package(package_abs_path).?;
        return Package_Iterator_Node{ .compiler = compiler, .package = package };
    }

    pub fn get_adjacent(self: *Package_Iterator_Node) []Package_Iterator_Node {
        var list = std.array_list.Managed(Package_Iterator_Node).init(self.compiler.allocator());

        for (self.package.requirements.keys()) |requirement_name| {
            const requirement_root_module_symbol = self.package.requirements.get(requirement_name);
            const requirement_root_module = requirement_root_module_symbol.?.init_value().?.module.module;
            const requirement_root_abs_path = requirement_root_module.get_package_abs_path();
            list.append(Package_Iterator_Node.init(self.compiler, requirement_root_abs_path)) catch unreachable;
        }

        return list.toOwnedSlice() catch unreachable;
    }

    pub fn free_adjacent(self: *Package_Iterator_Node, adj: []Package_Iterator_Node) void {
        self.compiler.allocator().free(adj);
    }
};

pub fn new(allocator: std.mem.Allocator, package_absolute_path: []const u8, kind: Package_Kind) *Package {
    const package = allocator.create(Package) catch unreachable;
    package.root = undefined; // filled in later
    package.output_absolute_path = undefined; // filled in when the output binary is created
    package.requirements = std.StringArrayHashMap(*Symbol).init(allocator);
    package.include_directories = std.StringArrayHashMap(void).init(allocator);
    package.library_directories = std.StringArrayHashMap(void).init(allocator);
    package.libraries = std.StringArrayHashMap(void).init(allocator);
    package.local_modules = std.array_list.Managed(*Module).init(allocator);
    package.visited = false;
    package.name = std.fs.path.basename(package_absolute_path);
    package.absolute_path = package_absolute_path;
    package.kind = kind;
    package.module_hash = Module_Hash.init(package_absolute_path, allocator) catch unreachable;
    package.type_set = Type_Set.init(allocator);
    package.modified = null;

    package.set_executable_name(allocator) catch unreachable;

    return package;
}

pub fn get_build_path(self: *const Package, allocator: std.mem.Allocator) []const u8 {
    const package_root_module = self.root.init_value().?.module.module;
    const package_path = package_root_module.get_package_abs_path();
    const build_paths = [_][]const u8{ package_path, "build" };
    return std.fs.path.join(allocator, &build_paths) catch unreachable;
}

pub fn get_types_path(self: *const Package, allocator: std.mem.Allocator) []const u8 {
    const package_root_module = self.root.init_value().?.module.module;
    const package_path = package_root_module.get_package_abs_path();
    const types_paths = [_][]const u8{ package_path, "build", "types" };
    return std.fs.path.join(allocator, &types_paths) catch unreachable;
}

pub fn get_build_module_absolute_path(self: *const Package, allocator: std.mem.Allocator) []const u8 {
    const package_root_module = self.root.init_value().?.module.module;
    const package_path = package_root_module.get_package_abs_path();
    const build_paths = [_][]const u8{ package_path, "build.orng" };
    return std.fs.path.join(allocator, &build_paths) catch unreachable;
}

pub fn entry(self: *const Package) ?*CFG {
    return self.root.init_value().?.module.module.entry;
}

pub fn get_build_module(self: *const Package, compiler: *Compiler_Context) ?*Module {
    const build_module_absolute_path: []const u8 = self.get_build_module_absolute_path(compiler.allocator());
    const build_module_symbol: *Symbol = compiler.lookup_module(build_module_absolute_path) orelse return null;
    return build_module_symbol.init_value().?.module.module;
}

fn get_required_package(self: *Package, requirement_name: []const u8, packages: std.StringArrayHashMap(*Package)) *Package {
    const requirement_root_module_symbol: ?*Symbol = self.requirements.get(requirement_name);
    const requirement_root_module: *Module = requirement_root_module_symbol.?.init_value().?.module.module;
    const requirement_root_abs_path: []const u8 = requirement_root_module.get_package_abs_path();
    const required_package: *Package = packages.get(requirement_root_abs_path).?;
    return required_package;
}

/// A package is modified if:
/// - Any of its modules are modified
/// - Any of its dependencies are modified
pub fn determine_if_package_modified(self: *Package, packages: std.StringArrayHashMap(*Package), compiler: *Compiler_Context) !void {
    if (self.modified != null) {
        return;
    }
    self.modified = false;

    // Check if any requirements are modified
    for (self.requirements.keys()) |requirement_name| {
        const required_package = self.get_required_package(requirement_name, packages);
        try required_package.determine_if_package_modified(packages, compiler);
        self.modified = required_package.modified.? or self.modified.?;
    }

    // Check if the build module was modified, if it was, entire package will be rebuilt
    if (self.get_build_module(compiler)) |build_module| {
        try build_module.determine_if_module_modified(compiler);
        try build_module.update_module_hash(&self.module_hash, compiler.allocator());
        self.modified = build_module.modified.? or self.modified.?;
    }

    // Check if any modules are modified
    for (self.local_modules.items) |local_module| {
        try local_module.determine_if_module_modified(compiler);
        try local_module.update_module_hash(&self.module_hash, compiler.allocator());
        self.modified = local_module.modified.? or self.modified.?;
    }

    // Check if the output exists - if it doesn't, package is modified
    self.modified = !file_exists(self.output_absolute_path) or self.modified.?;
}

pub fn collect_package_types(self: *Package) void {
    for (self.local_modules.items) |module| {
        module.collect_module_types(&self.type_set);
    }
}

fn file_exists(abs_path: []const u8) bool {
    var file = std.fs.openFileAbsolute(abs_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return false,
    };
    defer file.close();
    return true;
}

/// Compiles a package. Assumes C files have already been emitted.
pub fn output(self: *Package, packages: std.StringArrayHashMap(*Package), allocator: std.mem.Allocator) !void {
    if (self.visited) {
        return;
    }
    self.visited = true;

    for (self.requirements.keys()) |requirement_name| {
        const required_package = self.get_required_package(requirement_name, packages);
        try required_package.output(packages, allocator);
    }

    const obj_files = try self.compile_obj_files(packages, allocator);
    self.module_hash.output_new_json(allocator);

    if (self.modified.?) {
        if (self.kind == .static_library) {
            try self.ar(obj_files, allocator);
        } else {
            try self.link_executable(obj_files, packages, allocator);
        }
    }
}

/// Compiles the C files to object files for this package's local modules and entry point
fn compile_obj_files(self: *Package, packages: std.StringArrayHashMap(*Package), allocator: std.mem.Allocator) !std.StringArrayHashMap(void) {
    var obj_files = std.StringArrayHashMap(void).init(allocator);

    try self.compile_local_modules(&obj_files, packages, allocator);
    try self.compile_entry_point(&obj_files, packages, allocator);

    return obj_files;
}

/// Compiles the C files to object files for this package's local modules
fn compile_local_modules(self: *Package, obj_files: *std.StringArrayHashMap(void), packages: std.StringArrayHashMap(*Package), allocator: std.mem.Allocator) !void {
    for (self.local_modules.items) |local_module| {
        if (!std.mem.eql(u8, local_module.get_package_abs_path(), self.absolute_path)) {
            continue;
        }

        const o_file = self.add_module_obj_files(obj_files, local_module, allocator);

        if (!local_module.modified.?) {
            // No need to re-compile!
            continue;
        }

        try self.compile_module_sources(o_file, packages, local_module, allocator);
    }
}

/// Appends the object files for a module to a set of object files
/// TODO: Should de-couple this from Module so that we can compile arbitrary C files
fn add_module_obj_files(self: *Package, obj_files: *std.StringArrayHashMap(void), local_module: *Module, allocator: std.mem.Allocator) std.array_list.Managed(u8) {
    // Append the o filename to the obj files list, even if this isn't compiled!
    var o_file = std.array_list.Managed(u8).init(allocator);
    o_file.print("{s}{c}build{c}{s}.o", .{
        self.absolute_path,
        std.fs.path.sep,
        std.fs.path.sep,
        local_module.name(),
    }) catch unreachable;
    obj_files.put(o_file.items, void{}) catch unreachable;
    local_module.modified = !file_exists(o_file.items) or local_module.modified.?;

    if (self.kind == .test_executable) {
        var test_o_file = std.array_list.Managed(u8).init(allocator);
        test_o_file.print("{s}{c}build{c}{s}-tests.o", .{
            self.absolute_path,
            std.fs.path.sep,
            std.fs.path.sep,
            local_module.name(),
        }) catch unreachable;
        obj_files.put(test_o_file.items, void{}) catch unreachable;
        local_module.modified = !file_exists(test_o_file.items) or local_module.modified.?;
    }
    return o_file;
}

/// Compiles the sources for a module
/// TODO: Should de-couple this from Module so that we can compile arbitrary C files
fn compile_module_sources(self: *Package, o_file: std.array_list.Managed(u8), packages: std.StringArrayHashMap(*Package), local_module: *Module, allocator: std.mem.Allocator) !void {
    var c_file = std.array_list.Managed(u8).init(allocator);
    defer c_file.deinit();
    c_file.print("{s}{c}build{c}{s}-{s}.c", .{
        self.absolute_path,
        std.fs.path.sep,
        std.fs.path.sep,
        self.name,
        local_module.name(),
    }) catch unreachable;
    try self.cc(c_file.items, o_file.items, packages, allocator);

    if (self.kind == .test_executable) {
        var test_c_file = std.array_list.Managed(u8).init(allocator);
        test_c_file.print("{s}{c}build{c}{s}-{s}-tests.c", .{
            self.absolute_path,
            std.fs.path.sep,
            std.fs.path.sep,
            self.name,
            local_module.name(),
        }) catch unreachable;

        var test_o_file = std.array_list.Managed(u8).init(allocator);
        test_o_file.print("{s}-tests.o", .{local_module.name()}) catch unreachable;

        try self.cc(test_c_file.items, test_o_file.items, packages, allocator);
    }
}

/// Compiles the C file for this package's entry point
fn compile_entry_point(self: *Package, obj_files: *std.StringArrayHashMap(void), packages: std.StringArrayHashMap(*Package), allocator: std.mem.Allocator) !void {
    if (!self.modified.?) {
        return;
    }

    switch (self.kind) {
        .executable => try self.compile_executable_entry_point(obj_files, packages, allocator),
        .test_executable => try self.compile_test_runner_entry_point(obj_files, packages, allocator),
        else => {},
    }
}

/// Compiles the `start` C file to an object file
fn compile_executable_entry_point(self: *Package, obj_files: *std.StringArrayHashMap(void), packages: std.StringArrayHashMap(*Package), allocator: std.mem.Allocator) !void {
    var start_o_file = std.array_list.Managed(u8).init(allocator);
    start_o_file.print("{s}{c}build{c}start.o", .{
        self.absolute_path,
        std.fs.path.sep,
        std.fs.path.sep,
    }) catch unreachable;
    obj_files.put(start_o_file.items, void{}) catch unreachable;

    var c_file = std.array_list.Managed(u8).init(allocator);
    c_file.print("{s}{c}build{c}start.c", .{
        self.absolute_path,
        std.fs.path.sep,
        std.fs.path.sep,
    }) catch unreachable;

    try self.cc(c_file.items, start_o_file.items, packages, allocator);
}

/// Compiles the test-runner C file to an object file
fn compile_test_runner_entry_point(self: *Package, obj_files: *std.StringArrayHashMap(void), packages: std.StringArrayHashMap(*Package), allocator: std.mem.Allocator) !void {
    var test_runner_o_file = std.array_list.Managed(u8).init(allocator);
    test_runner_o_file.print("{s}{c}build{c}test-runner.o", .{
        self.absolute_path,
        std.fs.path.sep,
        std.fs.path.sep,
    }) catch unreachable;
    obj_files.put(test_runner_o_file.items, void{}) catch unreachable;

    var c_file = std.array_list.Managed(u8).init(allocator);
    c_file.print("{s}{c}build{c}test-runner.c", .{
        self.absolute_path,
        std.fs.path.sep,
        std.fs.path.sep,
    }) catch unreachable;

    try self.cc(c_file.items, test_runner_o_file.items, packages, allocator);
}

pub fn append_include_dir(self: *Package, packages: std.StringArrayHashMap(*Package), include_dirs: *std.StringArrayHashMap(void)) void {
    for (self.requirements.keys()) |requirement_name| {
        const required_package = self.get_required_package(requirement_name, packages);
        required_package.append_include_dir(packages, &self.include_directories);
    }
    for (self.include_directories.keys()) |dir| {
        include_dirs.put(dir, void{}) catch unreachable;
    }
}

fn cc(
    self: *Package,
    c_file: []const u8,
    o_file: []const u8,
    packages: std.StringArrayHashMap(*Package),
    allocator: std.mem.Allocator,
) !void {
    const cc_cmd = try self.construct_obj_cc_cmd(c_file, o_file, packages, allocator);
    try self.invoke_cmd(cc_cmd, allocator);
}

/// Constructs the command to call zig cc to compile a modules .o file
fn construct_obj_cc_cmd(
    self: *Package,
    c_file: []const u8,
    o_file: []const u8,
    packages: std.StringArrayHashMap(*Package),
    allocator: std.mem.Allocator,
) !std.array_list.Managed([]const u8) {
    var env_map = std.process.getEnvMap(allocator) catch unreachable;
    defer env_map.deinit();

    // Base cc command
    var cc_cmd = std.array_list.Managed([]const u8).init(allocator);
    cc_cmd.appendSlice(&[_][]const u8{ "zig", "cc" }) catch unreachable;

    // Add input/output
    cc_cmd.append("-c") catch unreachable;
    cc_cmd.append(c_file) catch unreachable;

    cc_cmd.append("-o") catch unreachable;
    cc_cmd.append(o_file) catch unreachable;

    const inc_path = repo_.get_inc_dir(allocator);
    var std_include_path = std.array_list.Managed(u8).init(allocator);
    std_include_path.print("-I{s}", .{inc_path}) catch unreachable;

    // Add basic flags
    cc_cmd.appendSlice(&[_][]const u8{
        "-std=c11",
        "-O3",
        "-g",
        std_include_path.items,
    }) catch unreachable;

    if (true) {
        cc_cmd.appendSlice(&[_][]const u8{
            // "-Werror", // TODO: Requires us to know when interned strings are unused
            "-Wall",
            "-Wextra",
            "-Wpedantic",
            "-pedantic-errors",
            "-Wconversion",
            "-Wno-pointer-sign", // C `char *` isnt' the same as `uint8_t *`, but like it is
            "-Wsign-conversion",
            "-Wfloat-conversion",
            "-Wcast-qual",
            // "-Wlogical-op", // only GCC
            "-Wshadow",
            "-Wformat=2",
            "-Wmisleading-indentation",
            "-Wstrict-prototypes",
            "-Wmissing-prototypes",
            "-Winit-self",
            // "-Wjump-misses-init", // only GCC
            "-Wdeclaration-after-statement",
            "-Wbad-function-cast",
            // "-Wc11-c2x-compat", // For C11
            "-Wcast-align",
            // "-fsanitize=undefined,address", // TODO: Needs some kind of linking
        }) catch unreachable;
    }

    self.append_requirements_includes(&cc_cmd, packages, allocator);
    self.append_self_includes(&cc_cmd, allocator);

    return cc_cmd;
}

/// Appends the include directories of the packages that this packages requires to the cc command
fn append_requirements_includes(
    self: *Package,
    cc_cmd: *std.array_list.Managed([]const u8),
    packages: std.StringArrayHashMap(*Package),
    allocator: std.mem.Allocator,
) void {
    for (self.requirements.keys()) |requirement_name| {
        const required_package = self.get_required_package(requirement_name, packages);

        var requirement_include_path = std.array_list.Managed(u8).init(allocator);
        requirement_include_path.print("-I{s}{c}build", .{ required_package.root.init_value().?.module.module.get_package_abs_path(), std.fs.path.sep }) catch unreachable;
        cc_cmd.append(requirement_include_path.items) catch unreachable;
    }
}

/// Appends the include directories that this package specified in `build.orng` to the cc command
fn append_self_includes(
    self: *Package,
    cc_cmd: *std.array_list.Managed([]const u8),
    allocator: std.mem.Allocator,
) void {
    for (self.include_directories.keys()) |include_dir| {
        var requirement_include_path = std.array_list.Managed(u8).init(allocator);
        requirement_include_path.print("-I{s}", .{include_dir}) catch unreachable;
        cc_cmd.append(requirement_include_path.items) catch unreachable;
    }
}

fn ar(self: *Package, obj_files: std.StringArrayHashMap(void), allocator: std.mem.Allocator) !void {
    // Base command
    var cmd = std.array_list.Managed([]const u8).init(allocator);
    cmd.appendSlice(&[_][]const u8{
        "zig",
        "ar",
        "rcs",
    }) catch unreachable;

    // Add the lib name
    cmd.append(self.output_absolute_path) catch unreachable;

    // Add all the object files
    cmd.appendSlice(obj_files.keys()) catch unreachable;

    try self.invoke_cmd(cmd, allocator);
}

pub fn set_executable_name(self: *Package, allocator: std.mem.Allocator) !void {
    var output_absolute_path = std.array_list.Managed(u8).init(allocator);
    output_absolute_path.print("{s}{c}build{c}", .{
        self.absolute_path,
        std.fs.path.sep,
        std.fs.path.sep,
    }) catch unreachable;
    switch (self.kind) {
        .executable => try output_absolute_path.print("{s}.exe", .{self.name}),
        .test_executable => try output_absolute_path.print("{s}-test.exe", .{self.name}),
        .static_library => try output_absolute_path.print("{s}.lib", .{self.name}),
        .shared_library => try output_absolute_path.print("{s}.so", .{self.name}),
    }
    self.output_absolute_path = output_absolute_path.items;
}

/// Runs the command to link the object files of a package into an executable
fn link_executable(self: *Package, obj_files: std.StringArrayHashMap(void), packages: std.StringArrayHashMap(*Package), allocator: std.mem.Allocator) !void {
    const cmd = try self.construct_exe_cc_cmd(obj_files, packages, allocator);
    try self.invoke_cmd(cmd, allocator);
}

fn invoke_cmd(self: *Package, cmd: std.array_list.Managed([]const u8), allocator: std.mem.Allocator) !void {
    var cwd_string = std.array_list.Managed(u8).init(allocator);
    cwd_string.print("{s}{c}build", .{ self.absolute_path, std.fs.path.sep }) catch unreachable;

    print_cmd(&cmd);
    const run_res = std.process.Child.run(.{
        .allocator = allocator,
        .argv = cmd.items,
        .cwd = cwd_string.items,
        .max_output_bytes = 1024 * 1024,
    }) catch |err| switch (err) {
        else => std.debug.panic("compile error: on cmd invoke: {}", .{err}),
    };

    var retcode: u8 = 0;
    switch (run_res.term) {
        .Exited => |c| {
            retcode = c;
        },
        else => {
            std.debug.panic("{s}\n", .{run_res.stderr});
        },
    }

    if (retcode != 0) {
        std.debug.panic("err:{s}\n", .{run_res.stderr});
    }
}

/// Constructs the command to call zig cc to compile an executable from a list of object files
fn construct_exe_cc_cmd(
    self: *Package,
    obj_files: std.StringArrayHashMap(void),
    packages: std.StringArrayHashMap(*Package),
    allocator: std.mem.Allocator,
) !std.array_list.Managed([]const u8) {
    var env_map = std.process.getEnvMap(allocator) catch unreachable;
    defer env_map.deinit();

    // Base command
    var cmd = std.array_list.Managed([]const u8).init(allocator);
    cmd.appendSlice(&[_][]const u8{ "zig", "cc" }) catch unreachable;

    // Add all the object files
    cmd.appendSlice(obj_files.keys()) catch unreachable;

    for (self.requirements.keys()) |requirement_name| {
        self.append_requirement_link(&cmd, packages, requirement_name, allocator);
    }

    // Add the output name
    cmd.append("-o") catch unreachable;
    cmd.append(self.output_absolute_path) catch unreachable;

    return cmd;
}

/// Appends the flags to link against a required package
fn append_requirement_link(
    self: *Package,
    cmd: *std.array_list.Managed([]const u8),
    packages: std.StringArrayHashMap(*Package),
    requirement_name: []const u8,
    allocator: std.mem.Allocator,
) void {
    const required_package = self.get_required_package(requirement_name, packages);

    try self.append_library_dirs(cmd, required_package, allocator);
    try self.append_library_flags(cmd, required_package, allocator);
}

fn append_library_dirs(
    self: *Package,
    cmd: *std.array_list.Managed([]const u8),
    required_package: *Package,
    allocator: std.mem.Allocator,
) !void {
    var requirement_library_path = std.array_list.Managed(u8).init(allocator);
    requirement_library_path.print("{s}{c}build", .{ required_package.root.init_value().?.module.module.get_package_abs_path(), std.fs.path.sep }) catch unreachable;
    cmd.append("-L") catch unreachable;
    cmd.append(requirement_library_path.items) catch unreachable;

    for (self.library_directories.keys()) |lib_dir| {
        cmd.append("-L") catch unreachable;
        cmd.append(lib_dir) catch unreachable;
    }
}

fn append_library_flags(
    self: *Package,
    cmd: *std.array_list.Managed([]const u8),
    required_package: *Package,
    allocator: std.mem.Allocator,
) !void {
    var requirement_library = std.array_list.Managed(u8).init(allocator);
    requirement_library.print("-l{s}", .{required_package.name}) catch unreachable;
    cmd.append(requirement_library.items) catch unreachable;

    for (self.libraries.keys()) |lib| {
        cmd.append("-l") catch unreachable;
        cmd.append(lib) catch unreachable;
    }
}

const debug: bool = true;
fn print_cmd(cmd: *const std.array_list.Managed([]const u8)) void {
    if (debug) {
        for (cmd.items) |item| {
            std.debug.print("{s} ", .{item});
        }
        std.debug.print("\n", .{});
    }
}
