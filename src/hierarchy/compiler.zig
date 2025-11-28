const std = @import("std");
const CFG = @import("../ir/cfg.zig");
const CFG_Store = @import("../ir/cfg_store.zig");
const core_ = @import("core.zig");
const errs_ = @import("../util/errors.zig");
const Interned_String_Set = @import("../ir/interned_string_set.zig");
const Module = @import("../hierarchy/module.zig").Module;
const Module_Iterator = @import("../util/dfs.zig").Dfs_Iterator(*Module);
const Package = @import("../hierarchy/package.zig");
const Package_Iterator = @import("../util/dfs.zig").Dfs_Iterator(Package_Iterator_Node);
const Package_Iterator_Node = @import("../hierarchy/package.zig").Package_Iterator_Node;
const poison_ = @import("../ast/poison.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const repo_ = @import("../util/repo.zig");
const Symbol = @import("../symbol/symbol.zig");
const Scope = @import("../symbol/scope.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

const Validate_Module = @import("../semantic/module_validate.zig");
const Validate_Scope = @import("../semantic/scope_validate.zig");
const Validate_Symbol = @import("../semantic/symbol_validate.zig");
const Validate_Type = @import("../types/type_validate.zig");
const Validate_Pattern = @import("../semantic/pattern_validate.zig");
const Typecheck = @import("../semantic/typecheck.zig");

const Self = @This();

const Error: type = error{
    LexerError,
    ParseError,
    CompileError,
    FileNotFound,
    OutOfMemory,
};

arena: std.heap.ArenaAllocator,

/// List of errors found during compilation
errors: errs_.Errors,
stderr: ?std.fs.File,

prelude: *Scope,

core: ?*Scope, // Null when compiling prelude and core module itself!

/// Maps module absolute paths to their symbol
modules: std.StringArrayHashMap(*Symbol),

cfg_store: CFG_Store,

validate_module: Validate_Module,
validate_scope: Validate_Scope,
validate_symbol: Validate_Symbol,
validate_type: Validate_Type,
validate_pattern: Validate_Pattern,
typecheck: Typecheck,

/// Maps module absolute paths to the interned string set for that module
module_interned_strings: std.AutoArrayHashMap(u32, *Interned_String_Set),

// Maps package names to their root module
packages: std.StringArrayHashMap(*Package),

/// Throws an error if the prelude could not be compiled
pub fn init(stderr: ?std.fs.File, alloc: std.mem.Allocator) Error!*Self {
    var retval: *Self = try alloc.create(Self);
    retval.arena = std.heap.ArenaAllocator.init(alloc);

    repo_.ensure_inc_dir_exists(retval.allocator());

    poison_.init_structures(retval.allocator());
    retval.errors = errs_.Errors.init(retval.allocator());
    retval.module_interned_strings = std.AutoArrayHashMap(u32, *Interned_String_Set).init(retval.allocator());
    retval.modules = std.StringArrayHashMap(*Symbol).init(retval.allocator());
    retval.packages = std.StringArrayHashMap(*Package).init(retval.allocator());
    retval.core = null;
    retval.stderr = stderr;

    retval.cfg_store = CFG_Store.init(retval);

    retval.validate_module = Validate_Module.init(retval);
    retval.validate_scope = Validate_Scope.init(retval);
    retval.validate_symbol = Validate_Symbol.init(retval);
    retval.validate_type = Validate_Type.init(retval);
    retval.validate_pattern = Validate_Pattern.init(retval);
    retval.typecheck = Typecheck.init(retval);

    retval.prelude = try prelude_.get_prelude_scope(retval);
    retval.core = core_.get_core_scope(retval) catch |e| {
        if (stderr == null) return e;
        var writer = stderr.?.writer(&.{}).interface;
        retval.errors.print_errors(&writer, .{});
        return e;
    };

    try retval.register_package(core_.core_package_name, .static_library);
    const core_module_symbol = retval.lookup_module(retval.core.?.module.?.absolute_path).?;
    retval.set_package_root(core_.core_package_name, core_module_symbol);

    return retval;
}

pub fn deinit(self: *Self) void {
    if (self.stderr) |stderr| {
        var writer = stderr.writer(&.{});
        const writer_intfc = &writer.interface;
        self.errors.print_errors(writer_intfc, .{});
    }
    poison_.deinit();
    self.arena.deinit();
    self.arena.child_allocator.destroy(self);
}

pub fn allocator(self: *Self) std.mem.Allocator {
    return self.arena.allocator();
}

pub fn compile_build_file(self: *Self, absolute_path: []const u8) Error!*CFG {
    const build_module = try self.compile_module(absolute_path, "build", false);
    return build_module.init_value().?.scope().?.lookup("build", .{}).found.cfg.?;
}

/// Compiles a module from a file
///
/// TODO: I think what needs to happen to allow for circular imports is compile the module first, collect any other
///       imports and symbols, and stop just before access decoration. Only after all symbols are defined for all
///       modules should accesses be resolved.
pub fn compile_module(
    self: *Self,
    absolute_path: []const u8,
    entry_name: ?[]const u8,
    fuzz_tokens: bool,
) Error!*Symbol {
    if (self.modules.get(absolute_path)) |module| {
        return module;
    }

    const module = Module.load_from_filename(absolute_path, entry_name, fuzz_tokens, self) catch |err| {
        switch (err) {
            // Always print these errors for fuzz testing
            error.LexerError,
            error.ParseError,
            error.FileNotFound,
            error.OutOfMemory,
            => {
                return err;
            },

            // Only print these errors if NOT fuzz testing
            error.CompileError,
            => if (!fuzz_tokens) {
                return err;
            } else {
                return err;
            },
        }
    };

    return self.lookup_module(module.absolute_path).?;
}

pub fn lookup_module(self: *Self, absolute_path: []const u8) ?*Symbol {
    return self.modules.get(absolute_path);
}

pub fn register_module(self: *Self, absolute_path: []const u8, module: *Symbol) !void {
    try self.modules.put(absolute_path, module);
}

pub fn module_scope(self: *Self, absolute_path: []const u8) ?*Scope {
    const module_symbol = self.lookup_module(absolute_path) orelse return null;
    return module_symbol.init_value().?.scope();
}

pub fn register_interned_string_set(self: *Self, module_uid: u32) !void {
    const interned_strings = try self.allocator().create(Interned_String_Set);
    interned_strings.* = Interned_String_Set.init(module_uid, self.allocator());
    try self.module_interned_strings.put(module_uid, interned_strings);
}

pub fn get_core_symbol(self: *Self, name: []const u8) *Symbol {
    const prelude_abs_path = self.core.?.module.?.absolute_path;
    return self.module_scope(prelude_abs_path).?.lookup(name, .{}).found;
}

pub fn get_core_type(self: *Self, name: []const u8) *Type_AST {
    return self.get_core_symbol(name).init_typedef().?;
}

pub fn lookup_interned_string_set(self: *Self, module_uid: u32) ?*Interned_String_Set {
    return self.module_interned_strings.get(module_uid);
}

pub fn lookup_package(self: *Self, package_absolute_path: []const u8) ?*Package {
    std.debug.assert(std.fs.path.isAbsolute(package_absolute_path));
    return self.packages.get(package_absolute_path);
}

pub fn lookup_package_root_module(self: *Self, package_absolute_path: []const u8, requirement_name: []const u8) ?*Symbol {
    const package = self.lookup_package(package_absolute_path);
    if (package == null) {
        return null;
    }
    return package.?.requirements.get(requirement_name);
}

pub fn register_package(self: *Self, package_absolute_path: []const u8, kind: Package.Package_Kind) !void {
    if (self.lookup_package(package_absolute_path) == null) {
        const package = Package.new(self.allocator(), package_absolute_path, kind);
        try self.packages.put(package_absolute_path, package);
        if (!std.mem.eql(u8, package_absolute_path, core_.core_package_name)) {
            try self.make_package_requirement_link(package_absolute_path, "core", core_.core_package_name);
        }
    }
}

pub fn make_package_requirement_link(self: *Self, package_absolute_path: []const u8, requirement_name: []const u8, requirement_absolute_path: []const u8) !void {
    const package = self.lookup_package(package_absolute_path).?;
    const requirement = self.lookup_package(requirement_absolute_path).?;
    try package.requirements.put(requirement_name, requirement.root);
}

pub fn set_package_root(self: *Self, package_absolute_path: []const u8, root: *Symbol) void {
    const package = self.lookup_package(package_absolute_path).?;
    package.root = root;
}

pub fn propagate_include_directories(self: *Self, root_package_absolute_path: []const u8) void {
    const package = self.lookup_package(root_package_absolute_path).?;
    package.append_include_dir(self.packages, &package.include_directories);
}

pub fn set_package_kind(self: *Self, package_absolute_path: []const u8, kind: Package.Package_Kind) !void {
    const package = self.lookup_package(package_absolute_path).?;
    package.kind = kind;
    try package.set_executable_name(self.allocator());
}

pub fn clean_package(self: *Self, package_absolute_path: []const u8) void {
    var dfs_iter: Package_Iterator = Package_Iterator.init(Package_Iterator_Node.init(self, package_absolute_path), self.allocator());
    defer dfs_iter.deinit();
    while (dfs_iter.next()) |package_node| {
        const build_path = package_node.package.get_build_path(self.allocator());
        std.fs.deleteTreeAbsolute(build_path) catch |err| switch (err) {
            error.FileNotFound => {}, // This is ok!
            else => std.debug.panic("{}\n", .{err}),
        };
    }
}

pub fn collect_package_local_modules(self: *Self) !void {
    for (self.packages.keys()) |package_name| {
        const package = self.lookup_package(package_name).?;
        const module = package.root.init_value().?.module.module;

        var dfs_iter: Module_Iterator = Module_Iterator.init(module, self.allocator());
        defer dfs_iter.deinit();
        while (dfs_iter.next()) |next_module| {
            if (std.mem.eql(u8, next_module.name(), "core") and !std.mem.eql(u8, package.name, "core")) {
                continue;
            }
            try package.local_modules.append(next_module);
        }
    }
}

pub fn collect_types(self: *Self) void {
    for (self.packages.keys()) |package_name| {
        const package = self.lookup_package(package_name).?;
        package.collect_package_types();
    }
}

pub fn determine_if_modified(self: *Self, root_package_absolute_path: []const u8) !void {
    const package = self.lookup_package(root_package_absolute_path).?;
    try package.determine_if_package_modified(self.packages, self);
}

pub fn compile(self: *Self, root_package_absolute_path: []const u8) !void {
    const package = self.lookup_package(root_package_absolute_path).?;
    try package.output(self.packages, self.allocator());
}
