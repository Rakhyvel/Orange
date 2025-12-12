const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const CFG = @import("../ir/cfg.zig");
const Cfg_Iterator = @import("../util/dfs.zig").Dfs_Iterator(*CFG);
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const Instruction = @import("../ir/instruction.zig");
const pipeline_ = @import("../util/pipeline.zig");
const Span = @import("../util/span.zig");
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const Module_Hash = @import("module_hash.zig");
const Token = @import("../lexer/token.zig");
const Type_Set = @import("../types/type_set.zig");
const UID_Gen = @import("../util/uid_gen.zig");

// Front-end pipeline steps
const Read_File = @import("../lexer/read_file.zig");
const Hash = @import("../lexer/hash.zig");
const Split_Lines = @import("../lexer/split_lines.zig");
const Tokenize = @import("../lexer/tokenize.zig");
const Special_Identifier_Literals = @import("../lexer/special_identifier_literal.zig");
const Apply_Layout = @import("../lexer/apply_layout.zig");
const Parse = @import("../parser/parse.zig");
const Apply_Ast_Walk = @import("../ast/walker.zig").Apply_Ast_Walk;
const Apply_Flat_Ast_Walk = @import("../ast/walker.zig").Apply_Flat_Ast_Walk;

// AST walks
const Expand = @import("../ast/expand.zig");
const Import = @import("../ast/import.zig");
const Cinclude = @import("../ast/cinclude.zig");
const Symbol_Tree = @import("../ast/symbol-tree.zig");
const Decorate = @import("../ast/decorate.zig");
const Type_Decorate = @import("../ast/type_decorate.zig");

pub const Module_Errors = error{ OutOfMemory, LexerError, ParseError, CompileError, FileNotFound };
pub const Module_UID: type = u32;

var module_uids: Module_UID = 0;

/// This structure represents a module being compiled. Each module corresponds to a single .c/.h pair. A program has
/// many modules.
///
/// Each module has a list of instructions, and a table of symbols/cfgs that refer to their corresponding instructions.
pub const Module = struct {
    // A unique identifier for this Orange module
    uid: Module_UID,

    /// Unique Identifier generator for symbols in this module, so that symbols are given predictable, stable UIDs
    uid_gen: UID_Gen,

    // Absolute path of the module
    absolute_path: []const u8,

    // The name of the package this module belongs to
    package_name: []const u8,

    /// Set of local modules that are imported
    local_imported_modules: std.AutoArrayHashMap(*Module, void),

    // Set of C headers to include
    cincludes: std.array_list.Managed(*ast_.AST),

    /// List of instructions for this module. Used by the interpreter, so that instructions are indexable by a random
    /// access instruction pointer.
    instructions: std.array_list.Managed(*Instruction),

    /// List of CFGs defined in this module. Used by codegen to generate functions and method definitions.
    cfgs: std.array_list.Managed(*CFG),

    /// Main function. Used by codegen to jump to the main function. May be null for modules that don't contain the
    /// entry point of the package.
    /// TODO: Isn't this more of a property of the package than the module?
    entry: ?*CFG,

    /// List of all traits defined in this module. Used by codegen to output the vtable structs definitions
    traits: std.array_hash_map.AutoArrayHashMap(*ast_.AST, void),

    /// List of all impls defined in this module. Used by codegen to output the vtable implementations.
    impls: std.array_list.Managed(*ast_.AST),

    /// List of all enums defined in this module. Used by codegen to output the variant_name function implementations.
    enums: std.array_hash_map.AutoArrayHashMap(*ast_.AST, void),

    /// List of all tests defined in this module. Used by codegen to output the vtable implementations.
    tests: std.array_list.Managed(*CFG),

    /// The types used by this module alone
    type_set: Type_Set,

    /// Allocator for the module
    allocator: std.mem.Allocator,

    /// The hash of the contents, used to track changes
    hash: u64,

    modified: ?bool,

    pub fn init(absolute_path: []const u8, allocator: std.mem.Allocator) !*Module {
        var retval = try allocator.create(Module);
        retval.uid = module_uids;
        retval.uid_gen = UID_Gen.init();
        module_uids += 1;
        std.debug.assert(std.fs.path.isAbsolute(absolute_path));
        retval.absolute_path = absolute_path;
        retval.package_name = std.fs.path.basename(std.fs.path.dirname(absolute_path).?);
        retval.allocator = allocator;
        retval.local_imported_modules = std.AutoArrayHashMap(*Module, void).init(allocator);
        retval.cincludes = std.array_list.Managed(*ast_.AST).init(allocator);
        retval.instructions = std.array_list.Managed(*Instruction).init(allocator);
        retval.traits = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(allocator);
        retval.impls = std.array_list.Managed(*ast_.AST).init(allocator);
        retval.enums = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(allocator);
        retval.tests = std.array_list.Managed(*CFG).init(allocator);
        retval.cfgs = std.array_list.Managed(*CFG).init(allocator);
        retval.type_set = Type_Set.init(allocator);
        retval.entry = null;
        retval.modified = null;

        return retval;
    }

    // TODO: Move to compiler context
    pub fn load_from_filename(
        absolute_path: []const u8,
        entry_name: ?[]const u8,
        fuzz_tokens: bool,
        compiler: *Compiler_Context,
    ) Module_Errors!*Module {
        // Check to see if the file exists
        {
            var file = std.fs.openFileAbsolute(absolute_path, .{}) catch |err| switch (err) {
                error.FileNotFound => return error.FileNotFound,
                else => return error.CompileError,
            };
            defer file.close();
        }

        // Create the symbol for this module
        const module = try Module.init(absolute_path, compiler.allocator());
        var file_root = Scope.init(compiler.prelude, &module.uid_gen, compiler.allocator());
        const symbol = Symbol.init(
            compiler.prelude,
            module.name(),
            ast_.AST.create_module(
                Token.init_simple(module.name()),
                file_root,
                module,
                compiler.allocator(),
            ),
            .module,
            .local,
            compiler.allocator(),
        );
        try compiler.prelude.put_symbol(symbol, &compiler.errors);
        file_root.module = module;

        try fill_contents_from_file(absolute_path, entry_name, file_root, module, symbol, fuzz_tokens, compiler);

        return module;
    }

    pub fn name(self: *const Module) []const u8 {
        const basename = std.fs.path.basename(self.absolute_path);
        var i: usize = 0;
        while (i < basename.len and basename[i] != '.') : (i += 1) {}
        return basename[0..i];
    }

    fn fill_contents_from_file(
        in_name: []const u8,
        entry_name: ?[]const u8,
        file_root: *Scope,
        module: *Module,
        module_symbol: *Symbol,
        fuzz_tokens: bool,
        compiler: *Compiler_Context,
    ) Module_Errors!void {
        const read_file = Read_File.init(compiler.allocator());
        const contents = try read_file.run(in_name);
        return fill_contents(contents, in_name, entry_name, file_root, module, module_symbol, fuzz_tokens, compiler);
    }

    pub fn fill_contents(
        contents: []const u8,
        in_name: []const u8,
        entry_name: ?[]const u8,
        file_root: *Scope,
        module: *Module,
        module_symbol: *Symbol,
        fuzz_tokens: bool,
        compiler: *Compiler_Context,
    ) Module_Errors!void {
        try compiler.register_interned_string_set(module.uid);
        try compiler.register_module(module.absolute_path, module_symbol);

        if (compiler.core != null) {
            // Can be null if you're compiling the core module itself!
            try module.local_imported_modules.put(compiler.core.?.module.?, void{});
            const core_import_symbol = Symbol.init(
                file_root,
                "core",
                ast_.AST.create_module(
                    Token.init_simple("core"),
                    compiler.core.?,
                    module,
                    compiler.allocator(),
                ),
                .{ .import = .{ .real_name = "core", .real_symbol = compiler.modules.get(compiler.core.?.module.?.absolute_path).? } },
                .local,
                compiler.allocator(),
            );
            try file_root.put_symbol(core_import_symbol, &compiler.errors);
        }

        // Setup and run the front-end pipeline
        _ = try pipeline_.run(contents, .{
            Hash.init(&module.hash),
            Split_Lines.init(&compiler.errors, compiler.allocator()),
            Tokenize.init(in_name, 0, 1, &compiler.errors, fuzz_tokens, compiler.allocator()),
            Special_Identifier_Literals.init(),
            Apply_Layout.init(),
            Parse.init(.top_level, &compiler.errors, compiler.allocator()),
            Apply_Ast_Walk(Expand).init(Expand.new(&compiler.errors, compiler.allocator())),
            Apply_Flat_Ast_Walk(Import).init(Import.new(compiler, module.get_package_abs_path(), &module.local_imported_modules)),
            Apply_Flat_Ast_Walk(Cinclude).init(Cinclude.new(&module.cincludes)),
            Apply_Ast_Walk(Symbol_Tree).init(Symbol_Tree.new(file_root, &compiler.errors, compiler.allocator())),
            Apply_Ast_Walk(Decorate).init(Decorate.new(file_root, compiler)),
            Apply_Ast_Walk(Type_Decorate).init(Type_Decorate.new(compiler)),
        });

        // Perform checks and collections on the module
        try compiler.validate_module.validate_module(module);
        compiler.module_scope(module.absolute_path).?.collect_traits_and_impls_and_enums(&module.traits, &module.impls, &module.enums);
        try module.add_all_cfgs(entry_name, compiler);
        if (module.entry) |entry| {
            entry.assert_needed_at_runtime();
        }
        try module.collect_impls_cfgs(compiler);
        try module.collect_tests(compiler);
        module.collect_local_types();
    }

    fn add_all_cfgs(self: *Module, entry_name: ?[]const u8, compiler: *Compiler_Context) Module_Errors!void {
        var found_entry = false;
        const need_entry = entry_name != null;
        for (compiler.module_scope(self.absolute_path).?.symbols.keys()) |key| {
            const symbol: *Symbol = compiler.module_scope(self.absolute_path).?.symbols.get(key).?;
            if (symbol.kind != .@"fn") {
                continue;
            }
            if (symbol.decl.?.num_generic_params() > 0) {
                continue;
            }

            // Instruction translation
            const interned_strings = compiler.lookup_interned_string_set(self.uid).?;
            const cfg = try compiler.cfg_store.get_cfg(symbol, interned_strings);
            self.collect_cfgs(cfg);

            if (need_entry and std.mem.eql(u8, key, entry_name.?)) {
                self.entry = cfg;
                found_entry = true;

                // Check for entry context requirements
                const args_ = @import("../semantic/args.zig");
                try args_.validate_requested_contexts(cfg.symbol.type().function.contexts.items, &compiler.errors);
            }
        }
        if (need_entry and !found_entry) {
            compiler.errors.add_error(errs_.Error{ .basic = .{
                .span = Span.phony,
                .msg = "no main function found",
            } });
            // Cannot find main function, fatal, do not procede
            return error.CompileError;
        }
    }

    /// This allows us to pick up anon and inner CFGs that wouldn't be exposed to the module's scope
    fn collect_cfgs(self: *Module, cfg: *CFG) void {
        var cfg_dfs_iter = Cfg_Iterator.init(cfg, self.allocator);
        while (cfg_dfs_iter.next()) |next_cfg| {
            if (next_cfg.symbol.decl.?.num_generic_params() > 0) continue;
            if (next_cfg.symbol.decl.?.* == .method_decl and next_cfg.symbol.decl.?.method_decl.impl.?.num_generic_params() > 0) continue;
            next_cfg.collect_generated_symbvers();
            _ = next_cfg.emplace_cfg(self.uid, &self.cfgs, &self.instructions);
        }
    }

    fn collect_local_types(self: *Module) void {
        for (self.traits.keys()) |trait| {
            for (trait.trait.method_decls.items) |decl| {
                const @"type" = decl.method_decl.c_type.?.expand_identifier();
                if (@"type".refers_to_self()) continue;
                _ = self.type_set.add_type(decl.method_decl.c_type.?.expand_identifier());
                _ = self.type_set.add_type(decl.method_decl.ret_type.expand_identifier());
            }
        }
        for (self.cfgs.items) |cfg| {
            cfg.collect_cfg_types(&self.type_set);
        }
    }

    /// Puts all the types used in this module into the given type set
    pub fn collect_module_types(self: *Module, type_set: *Type_Set) void {
        type_set.union_from(&self.type_set);
    }

    fn collect_impls_cfgs(self: *Module, compiler: *Compiler_Context) Module_Errors!void {
        for (self.impls.items) |impl| {
            if (impl.impl.instantiations.pairs.items.len == 0) {
                try self.collect_impl_cfgs(impl, compiler);
            } else {
                for (impl.impl.instantiations.pairs.items) |pair| {
                    try self.collect_impl_cfgs(pair.value, compiler);
                }
            }
        }
    }

    fn collect_impl_cfgs(self: *Module, impl: *ast_.AST, compiler: *Compiler_Context) Module_Errors!void {
        for (impl.impl.method_defs.items) |def| {
            const symbol = def.symbol().?;
            const interned_strings = compiler.lookup_interned_string_set(self.uid).?;
            const cfg = try compiler.cfg_store.get_cfg(symbol, interned_strings);
            cfg.assert_needed_at_runtime();
            self.collect_cfgs(cfg);
        }
    }

    fn collect_tests(self: *Module, compiler: *Compiler_Context) Module_Errors!void {
        var test_asts = std.array_list.Managed(*ast_.AST).init(compiler.allocator());
        compiler.module_scope(self.absolute_path).?.collect_tests(&test_asts);

        for (test_asts.items) |test_ast| {
            const symbol = test_ast.symbol().?;
            const interned_strings = compiler.lookup_interned_string_set(self.uid).?;
            const cfg = try compiler.cfg_store.get_cfg(symbol, interned_strings);
            try self.tests.append(cfg);
            cfg.assert_needed_at_runtime();
            self.collect_cfgs(cfg);
        }
    }

    pub fn get_package_abs_path(self: *Module) []const u8 {
        return std.fs.path.dirname(self.absolute_path).?;
    }

    pub fn pop_cfg(self: *Module, idx: i64) void {
        _ = self.cfgs.orderedRemove(@as(usize, @intCast(idx)));
    }

    pub fn get_adjacent(self: *Module) []*Module {
        return self.local_imported_modules.keys();
    }

    pub fn print_instructions(self: *Module) void {
        for (self.instructions.items) |instr| {
            std.debug.print("{}", .{instr});
        }
    }

    /// A module is modified if:
    /// - Its source hash differs from what is stored in the package's json file
    /// - Any of the module it imports have been modified
    pub fn determine_if_module_modified(self: *Module, compiler: *Compiler_Context) !void {
        if (self.modified != null) {
            return;
        }

        const package_abs_path = self.get_package_abs_path();
        const module_hashes = compiler.lookup_package(package_abs_path).?.module_hash;
        const old_hash = module_hashes.get_module_stored_hash(self.name());
        const local_module_number_string = try std.fmt.allocPrint(compiler.allocator(), "{X}", .{self.hash});
        defer compiler.allocator().free(local_module_number_string);
        self.modified = (old_hash == null) or !std.mem.eql(u8, local_module_number_string, old_hash.?);

        for (self.local_imported_modules.keys()) |imported_module| {
            try imported_module.determine_if_module_modified(compiler);
            self.modified = imported_module.modified.? or self.modified.?;
        }
    }

    pub fn update_module_hash(self: *const Module, module_hash: *Module_Hash, allocator: std.mem.Allocator) !void {
        const local_module_number_string = try std.fmt.allocPrint(allocator, "{X}", .{self.hash});
        module_hash.set_module_hash(self.name(), local_module_number_string, allocator);
    }
};
