const std = @import("std");
const anon_name_ = @import("../util/anon_name.zig");
const ast_ = @import("../ast/ast.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const Decorate = @import("../ast/decorate.zig");
const errs_ = @import("../util/errors.zig");
const Symbol = @import("symbol.zig");
const Symbol_Tree = @import("../ast/symbol-tree.zig");
const module_ = @import("../hierarchy/module.zig");
const unification_ = @import("../types/unification.zig");
const UID_Gen = @import("../util/uid_gen.zig");
const walker_ = @import("../ast/walker.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

const Self = @This();

const Lookup_Result = union(enum) { found_but_rt, found_but_fn, not_found, found: *Symbol };

parent: ?*Self,
children: std.array_list.Managed(*Self),
symbols: std.StringArrayHashMap(*Symbol),
traits: std.array_hash_map.AutoArrayHashMap(*ast_.AST, void), // Set of all `trait`s in this scope. Added to in the `decorate` phase.
impls: std.array_list.Managed(*ast_.AST), // List of all `impl`s in this scope Added to in the `decorate` phase.
enums: std.array_hash_map.AutoArrayHashMap(*ast_.AST, void), // List of all `enum`s in this scope Added to in the `decorate` phase. This is so we can generate the variant_name functions.
tests: std.array_list.Managed(*ast_.AST), // List of all `test`s in this scope Added to in the `decorate` phase.
module: ?*module_.Module, // Enclosing module
uid: usize,
uid_gen: *UID_Gen,

function_depth: usize = 0,
inner_function: ?*Symbol = null,

pub fn init(parent: ?*Self, uid_gen: *UID_Gen, allocator: std.mem.Allocator) *Self {
    var retval = allocator.create(Self) catch unreachable;
    retval.parent = parent;
    retval.children = std.array_list.Managed(*Self).init(allocator);
    retval.symbols = std.StringArrayHashMap(*Symbol).init(allocator);
    retval.traits = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(allocator);
    retval.impls = std.array_list.Managed(*ast_.AST).init(allocator);
    retval.enums = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(allocator);
    retval.tests = std.array_list.Managed(*ast_.AST).init(allocator);
    retval.uid = uid_gen.uid();
    retval.uid_gen = uid_gen;
    if (parent) |_parent| {
        _parent.children.append(retval) catch unreachable;
        retval.function_depth = _parent.function_depth;
        retval.inner_function = _parent.inner_function;
        retval.module = _parent.module;
    } else {
        retval.function_depth = 0;
        retval.inner_function = null;
        retval.module = null;
    }
    return retval;
}

pub const Lookup_Flags = struct {
    crossed_boundary: bool = false,
    allow_modules: bool = false,
    look_for_kind: ?Symbol.Kind = null,
};

pub fn lookup(self: *Self, name: []const u8, flags: Lookup_Flags) Lookup_Result {
    const log: bool = false;
    if (log) {
        const found = self.symbols.get(name) != null;
        std.debug.print("searching for: {s} {}({})\n", .{ name, found, flags });
        self.pprint();
    }

    if (self.symbols.get(name)) |symbol| {
        if (!flags.allow_modules and symbol.kind == .module) {
            // search parents
        } else if (flags.look_for_kind != null and @intFromEnum(flags.look_for_kind.?) != @intFromEnum(symbol.kind)) {
            // search parents
        } else if (flags.crossed_boundary and (symbol.kind == .mut or symbol.kind == .let)) {
            // Found the symbol, but it's non-const and we've crossed an inner-function boundary
            return .found_but_fn;
        } else {
            // Found the symbol just fine
            if (log) {
                std.debug.print("Found: {s} {*}\n", .{ name, symbol });
            }
            return Lookup_Result{ .found = symbol };
        }
    }

    if (self.parent) |parent| {
        var new_flags = flags;
        new_flags.crossed_boundary = parent.function_depth < self.function_depth or flags.crossed_boundary;
        const res = parent.lookup(name, new_flags);
        return res;
    } else {
        // Did not find the symbol at all
        return .not_found;
    }
}

pub fn num_visible_contexts(self: *Self) usize {
    var result: usize = 0;

    for (self.symbols.keys()) |symbol_name| {
        const symbol = self.symbols.get(symbol_name).?;
        if (symbol.kind == .let) {
            var symbol_type = symbol.type();
            if (symbol_type.* == .addr_of) {
                symbol_type = symbol_type.child();
            }
            if (symbol_type.* == .context_type or std.mem.startsWith(u8, symbol.name, "context_value__")) {
                result += 1;
            }
        }
    }

    const parent_contrib = if (self.parent) |parent|
        parent.num_visible_contexts()
    else
        0;

    return result + parent_contrib;
}

pub fn context_lookup(self: *Self, context_type: *Type_AST, ctx: *Compiler_Context) !?*Symbol {
    for (self.symbols.keys()) |symbol_name| {
        const symbol = self.symbols.get(symbol_name).?;
        if (symbol.kind == .let) {
            var symbol_type = symbol.type();
            if (symbol_type.* == .addr_of and context_type.* != .addr_of) {
                symbol_type = symbol_type.child();
            }
            try walker_.walk_type(symbol_type, Decorate.new(ctx)); // TODO: Needed?
            if (context_type.types_match(symbol_type)) {
                return symbol;
            }
        }
    } else if (self.parent) |parent| {
        return parent.context_lookup(context_type, ctx);
    } else {
        return null;
    }
}

const Impl_Trait_Lookup_Result = struct { count: u8, ast: ?*ast_.AST };

/// Returns the number of impls found for a given type-trait pair, and the impl ast. The impl is unique if count == 1.
pub fn impl_trait_lookup(self: *Self, for_type: *Type_AST, trait: *Symbol, ctx: *Compiler_Context) error{ CompileError, OutOfMemory }!Impl_Trait_Lookup_Result {
    if (false) {
        std.debug.print("searching {} for impls of {s} for {f}\n", .{ self.impls.items.len, trait.name, for_type.* });
        Tree_Writer.print_type_tree(for_type);
        self.pprint();
    }
    var retval: Impl_Trait_Lookup_Result = .{ .count = 0, .ast = null };

    // Type param with the constraint, return positive count with null ast if traits match
    if ((for_type.* == .identifier or for_type.* == .access) and for_type.symbol() != null and for_type.symbol().?.decl.?.* == .type_param_decl) {
        const type_param_decl = for_type.symbol().?.decl.?;
        for (type_param_decl.type_param_decl.constraints.items) |constraint| {
            const trait_symbol = try Decorate.symbol(constraint, ctx);
            if (trait_symbol == trait) {
                return .{ .count = 1, .ast = null };
            }
        }
    }

    // As-trait ascription, return positive count with null ast if traits match
    if (for_type.* == .as_trait and for_type.rhs().symbol().? == trait) {
        return self.impl_trait_lookup(for_type.lhs(), trait, ctx);
    }

    // Go through the scope's list of implementations, check to see if the types and traits match
    for (self.impls.items) |impl| {
        const traits_match = impl.impl.trait.?.symbol() == trait;
        if (!traits_match) continue;

        var subst = unification_.Substitutions.init(std.heap.page_allocator);
        defer subst.deinit();
        unification_.unify(impl.impl._type, for_type, &subst, .{}) catch {
            continue;
        };
        if (for_type.* == .identifier and for_type.symbol().?.decl.?.* == .type_param_decl) {
            const sat_res = impl.impl._type.satisfies_all_constraints(for_type.symbol().?.decl.?.type_param_decl.constraints.items, self, ctx) catch continue;
            if (sat_res != .satisfies) continue;
        }

        retval.count += 1;
        retval.ast = retval.ast orelse impl;
    }

    // Go through imports
    for (self.symbols.keys()) |symbol_name| {
        const symbol = self.symbols.get(symbol_name).?;
        if (symbol.kind == .import) {
            var res_symbol: *Symbol = symbol.kind.import.real_symbol orelse self.parent.?.lookup(symbol.kind.import.real_name, .{ .allow_modules = true }).found;

            const module_scope = res_symbol.init_value().?.scope().?;
            const parent_res = try module_scope.impl_trait_lookup(for_type, trait, ctx);
            if (parent_res.count > 0) {
                retval.count += parent_res.count;
                retval.ast = retval.ast orelse parent_res.ast;
                return parent_res;
            }
        }
    }

    if (self.parent != null) {
        // Did not match in this scope. Try parent scope
        const parent_res = try self.parent.?.impl_trait_lookup(for_type, trait, ctx);
        retval.count += parent_res.count;
        retval.ast = retval.ast orelse parent_res.ast;
        return retval;
    } else {
        // Not found, parent scope is null
        return retval;
    }
}

/// Looks up the impl's decl/method_decl ast for a given type, with a given name
pub fn lookup_impl_member(self: *Self, for_type: *Type_AST, name: []const u8, compiler: *Compiler_Context) !?*ast_.AST {
    if (false) {
        std.debug.print("searching {} impls for {f}::{s}\n", .{ self.impls.items.len, for_type.*, name });
        Tree_Writer.print_type_tree(for_type);
        self.pprint();
    }

    if (for_type.* == .identifier and for_type.symbol() != null and for_type.symbol().?.decl.?.* == .type_param_decl) {
        const type_param_decl = for_type.symbol().?.decl.?;
        for (type_param_decl.type_param_decl.constraints.items) |constraint| {
            const trait_decl = switch (constraint.*) {
                .generic_apply => constraint.lhs().symbol().?.decl.?,
                else => constraint.symbol().?.decl.?,
            };
            if (try self.lookup_member_in_trait(trait_decl, for_type, name, compiler)) |res| return res;
            for (trait_decl.trait.super_traits.items) |super_trait| {
                if (try self.lookup_member_in_trait(super_trait.symbol().?.decl.?, for_type, name, compiler)) |res| return res;
            }
        }
    }

    if (try self.lookup_impl_member_impls(for_type, name, compiler)) |res| return res;
    if (try self.lookup_impl_member_super_impls(for_type, name, compiler)) |res| return res;
    if (try self.lookup_impl_member_imports(for_type, name, compiler)) |res| return res;
    if (self.parent) |p| return p.lookup_impl_member(for_type, name, compiler);
    return null;
}

/// Searches a trait declaration for a declaration. Clones and substitutes any references to `Self` with `for_type`.
pub fn lookup_member_in_trait(self: *Self, trait_decl: *ast_.AST, for_type: *Type_AST, name: []const u8, compiler: *Compiler_Context) !?*ast_.AST {
    // TODO: (for next release) De-duplicate this
    for (trait_decl.trait.method_decls.items) |method_decl| {
        if (!std.mem.eql(u8, method_decl.method_decl.name.token().data, name)) continue;

        var subst = unification_.Substitutions.init(compiler.allocator());
        defer subst.deinit();
        subst.put("Self", for_type) catch unreachable;
        const cloned = method_decl.clone(&subst, compiler.allocator());
        const new_scope = init(self.parent.?, self.uid_gen, compiler.allocator());
        try walker_.walk_ast(cloned, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
        return cloned;
    }
    for (trait_decl.trait.const_decls.items) |const_decl| {
        if (!std.mem.eql(u8, const_decl.binding.decls.items[0].decl.name.token().data, name)) continue;

        var subst = unification_.Substitutions.init(compiler.allocator());
        defer subst.deinit();
        subst.put("Self", for_type) catch unreachable;
        const cloned = const_decl.clone(&subst, compiler.allocator());
        const new_scope = init(self.parent.?, self.uid_gen, compiler.allocator());
        try walker_.walk_ast(cloned, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
        return cloned;
    }
    for (trait_decl.trait.type_decls.items) |type_decl| {
        if (!std.mem.eql(u8, type_decl.token().data, name)) continue;
        return type_decl;
    }
    return null;
}

var count: usize = 0;
fn lookup_impl_member_impls(self: *Self, for_type: *Type_AST, name: []const u8, compiler: *Compiler_Context) !?*ast_.AST {
    for (self.impls.items) |impl| {
        var subst = unification_.Substitutions.init(compiler.allocator());
        defer subst.deinit();

        try compiler.validate_type.validate_type(impl.impl._type);
        unification_.unify(impl.impl._type, for_type, &subst, .{}) catch {
            continue;
        };

        const instantiated_impl = try self.instantiate_generic_impl(impl, &subst, compiler);
        if (search_impl(instantiated_impl, name)) |res| return res;
    }
    return null;
}

fn lookup_impl_member_super_impls(self: *Self, for_type: *Type_AST, name: []const u8, compiler: *Compiler_Context) !?*ast_.AST {
    for (self.impls.items) |impl| {
        if (impl.impl.trait) |trait| {
            if (trait.symbol() == null) continue;
            for (trait.symbol().?.decl.?.trait.super_traits.items) |super_trait| {
                if (try self.lookup_member_in_trait(super_trait.symbol().?.decl.?, for_type, name, compiler)) |res| return res;
            }
        }
    }
    return null;
}

fn lookup_impl_member_imports(self: *Self, for_type: *Type_AST, name: []const u8, compiler: *Compiler_Context) error{ OutOfMemory, CompileError }!?*ast_.AST {
    for (self.symbols.keys()) |symbol_name| {
        const symbol = self.symbols.get(symbol_name).?;
        if (symbol.kind == .import) {
            var res_symbol: *Symbol = symbol.kind.import.real_symbol orelse self.parent.?.lookup(symbol.kind.import.real_name, .{ .allow_modules = true }).found;

            const module_scope = res_symbol.init_value().?.scope().?;
            const module_scope_lookup = try module_scope.lookup_impl_member(for_type, name, compiler);
            if (module_scope_lookup != null) {
                return module_scope_lookup;
            }
        }
    }
    return null;
}

fn instantiate_generic_impl(self: *Self, impl: *ast_.AST, subst: *unification_.Substitutions, compiler: *Compiler_Context) !*ast_.AST {
    // Not even generic
    if (impl.impl._generic_params.items.len == 0) return impl;

    // Substitution still contains generics, return original impl
    const subst_contains_generics = unification_.substitution_contains_generics(subst);
    if (subst_contains_generics) return impl;

    // Already instantiated, return the memoized impl
    const type_param_list = unification_.type_param_list_from_subst_map(subst, impl.impl._generic_params, compiler.allocator());
    if (impl.impl.instantiations.get(type_param_list)) |instantiated| return instantiated;

    // Create a new impl
    const new_impl: *ast_.AST = impl.clone(subst, compiler.allocator());
    impl.impl.instantiations.put(type_param_list, new_impl) catch unreachable;
    if (!subst_contains_generics) {
        new_impl.impl._generic_params.clearRetainingCapacity();
    }
    const new_scope = init(impl.scope().?.parent.?, self.uid_gen, compiler.allocator());
    try walker_.walk_ast(new_impl, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
    new_impl.set_scope(new_scope);

    if (new_impl.impl.trait == null or new_impl.impl.impls_anon_trait) {
        // impl'd for an anon trait, create a new anon trait for it
        var token = new_impl.token();
        token.kind = .identifier;
        token.data = anon_name_.next_anon_name("Trait", compiler.allocator());
        const anon_trait = ast_.AST.create_trait(
            token,
            ast_.AST.create_pattern_symbol(
                token,
                .trait,
                .local,
                token.data,
                compiler.allocator(),
            ),
            std.array_list.Managed(*Type_AST).init(compiler.allocator()),
            new_impl.impl.method_defs,
            new_impl.impl.const_defs,
            new_impl.impl.type_defs,
            compiler.allocator(),
        );
        try walker_.walk_ast(anon_trait, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
        new_impl.impl.trait = ast_.AST.create_identifier(token, compiler.allocator());
        try walker_.walk_ast(new_impl.impl.trait, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
        new_impl.impl.impls_anon_trait = true;
    }

    try compiler.validate_scope.validate_scope(new_scope);

    // Set all method_decls to be monomorphed
    for (new_impl.impl.method_defs.items) |method_def| {
        method_def.symbol().?.is_monomorphed = true;
    }

    // Store in the memo
    return impl.impl.instantiations.get(type_param_list) orelse new_impl; // TODO: substitutions need to be in the same order as generic params
}

/// Searches an impl for a field name
pub fn search_impl(impl: *ast_.AST, name: []const u8) ?*ast_.AST {
    for (impl.impl.method_defs.items) |method_def| {
        if (std.mem.eql(u8, method_def.method_decl.name.token().data, name)) {
            return method_def;
        }
    }
    for (impl.impl.const_defs.items) |const_def| {
        if (std.mem.eql(u8, const_def.binding.pattern.symbol().?.name, name)) {
            return const_def;
        }
    }
    for (impl.impl.type_defs.items) |type_def| {
        if (std.mem.eql(u8, type_def.symbol().?.name, name)) {
            return type_def;
        }
    }
    return null;
}

pub fn pprint(self: *Self) void {
    std.debug.print("scope_{}:\n", .{self.uid});
    for (self.symbols.keys()) |name| {
        const symbol = self.symbols.get(name).?;
        if (symbol.kind == .import) {
            std.debug.print("  {s} {s} ({s})\n", .{ @tagName(symbol.kind), name, symbol.kind.import.real_name });
        } else if (symbol.kind == .type) {
            std.debug.print("  {s} {s} = {?f}\n", .{ @tagName(symbol.kind), name, symbol.init_typedef() });
        } else {
            std.debug.print("  {s} {s}\n", .{ @tagName(symbol.kind), name });
        }
    }
    for (self.impls.items) |impl| {
        std.debug.print("  impl {?f} for {f}\n", .{ impl.impl.trait, impl.impl._type });
    }
}

pub fn put_symbol(scope: *Self, symbol: *Symbol, errors: *errs_.Errors) error{CompileError}!void {
    const res = scope.lookup(symbol.name, .{});
    switch (res) {
        .found => {
            const first = res.found;
            errors.add_error(errs_.Error{ .redefinition = .{
                .first_defined_span = first.span(),
                .redefined_span = symbol.span(),
                .name = symbol.name,
            } });
            return error.CompileError;
        },
        else => scope.symbols.put(symbol.name, symbol) catch unreachable,
    }
}

pub fn put_all_symbols(scope: *Self, symbols: *std.array_list.Managed(*Symbol), errors: *errs_.Errors) error{CompileError}!void {
    for (symbols.items) |symbol| {
        try scope.put_symbol(symbol, errors);
    }
}

pub fn collect_traits_and_impls_and_enums(
    self: *Self,
    traits: *std.array_hash_map.AutoArrayHashMap(*ast_.AST, void),
    impls: *std.array_list.Managed(*ast_.AST),
    enums: *std.array_hash_map.AutoArrayHashMap(*ast_.AST, void),
) void {
    for (self.traits.keys()) |trait| {
        traits.put(trait, void{}) catch unreachable;
    }
    impls.appendSlice(self.impls.items) catch unreachable;
    for (self.enums.keys()) |@"enum"| {
        enums.put(@"enum", void{}) catch unreachable;
    }

    for (self.children.items) |child| {
        child.collect_traits_and_impls_and_enums(traits, impls, enums);
    }
}

pub fn collect_tests(
    self: *Self,
    tests: *std.array_list.Managed(*ast_.AST),
) void {
    tests.appendSlice(self.tests.items) catch unreachable;

    for (self.children.items) |child| {
        child.collect_tests(tests);
    }
}
