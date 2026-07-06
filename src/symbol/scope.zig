const std = @import("std");
const anon_name_ = @import("../util/anon_name.zig");
const ast_ = @import("../ast/ast.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const Decorate = @import("../ast/decorate.zig");
const errs_ = @import("../util/errors.zig");
const generic_apply_ = @import("../ast/generic_apply.zig");
const Symbol = @import("symbol.zig");
const Symbol_Tree = @import("../ast/symbol-tree.zig");
const module_ = @import("../hierarchy/module.zig");
const unification_ = @import("../types/unification.zig");
const UID_Gen = @import("../util/uid_gen.zig");
const walker_ = @import("../ast/walker.zig");
const Token = @import("../lexer/token.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

const Self = @This();

const Lookup_Result = union(enum) { found_but_rt, found_but_fn, not_found, found: *Symbol };

parent: ?*Self,
children: std.array_list.Managed(*Self),
symbols: std.StringArrayHashMap(*Symbol),
traits: std.array_hash_map.AutoArrayHashMap(*ast_.AST, void), // Set of all `trait`s in this scope. Added to in the `decorate` phase.
enums: std.array_hash_map.AutoArrayHashMap(*ast_.AST, void), // List of all `enum`s in this scope Added to in the `decorate` phase. This is so we can generate the variant_name functions.
tests: std.array_list.Managed(*ast_.AST), // List of all `test`s in this scope Added to in the `decorate` phase.
module: ?*module_.Module, // Enclosing module
uid: usize,
uid_gen: *UID_Gen,

function_depth: usize = 0,
inner_function: ?*Symbol = null,

var twenties: usize = 0;

pub fn init(parent: ?*Self, uid_gen: *UID_Gen, allocator: std.mem.Allocator) *Self {
    var retval = allocator.create(Self) catch unreachable;
    retval.parent = parent;
    retval.children = std.array_list.Managed(*Self).init(allocator);
    retval.symbols = std.StringArrayHashMap(*Symbol).init(allocator);
    retval.traits = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(allocator);
    retval.enums = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(allocator);
    retval.tests = std.array_list.Managed(*ast_.AST).init(allocator);
    retval.uid = uid_gen.uid();
    if (retval.uid == 20) {
        twenties += 1;
        if (twenties == 2) {
            // std.debug.panic("created the 20 scope\n", .{});
        }
    }
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
            try walker_.walk_type(symbol_type, Decorate.new(ctx));
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

pub const Impl_Trait_Lookup_Result = struct {
    count: u8,
    impl_ast: ?*ast_.AST,
    subst: ?unification_.Substitutions,
};

/// Returns the number of impls found for a given type-trait pair, and the impl ast. The impl is unique if count == 1.
pub fn impl_trait_lookup(self: *Self, for_type: *Type_AST, trait: *Symbol, ctx: *Compiler_Context) error{ CompileError, OutOfMemory }!Impl_Trait_Lookup_Result {
    _ = self;

    try ctx.validate_type.validate_type(for_type);

    if (for_type.* == .identifier or for_type.* == .generic_apply or for_type.* == .access) {
        // Hot path, if we've seen (for_type, trait) before, return the cached result
        if (for_type.symbol()) |type_sym| {
            if (type_sym.decl == null or type_sym.decl.?.* != .type_param_decl) {
                const key = Compiler_Context.Impl_Cache_Key{ .type_sym = type_sym, .trait = trait };
                if (ctx.impl_cache.get(key)) |cached| {
                    return cached;
                }
            }
        }
    }

    // Cold path, do full lookup
    var retval: Impl_Trait_Lookup_Result = .{ .count = 0, .impl_ast = null, .subst = null };
    for (ctx.modules.keys()) |module_key| {
        const module_scope = ctx.module_scope(module_key).?;
        const result = try impl_trait_lookup_inner(module_scope, module_scope, for_type, trait, ctx);
        if (result.count > 0) {
            retval.count += 1;
            retval.impl_ast = retval.impl_ast orelse result.impl_ast;
            retval.subst = retval.subst orelse result.subst;
        }
    }
    const result = try impl_trait_lookup_inner(ctx.prelude, ctx.prelude, for_type, trait, ctx);
    if (result.count > 0) {
        retval.count += 1;
        retval.impl_ast = retval.impl_ast orelse result.impl_ast;
        retval.subst = retval.subst orelse result.subst;
    }

    if (for_type.* == .identifier or for_type.* == .generic_apply or for_type.* == .access) {
        // Cache for hot path later
        if (for_type.symbol()) |type_sym| {
            if (type_sym.decl == null or type_sym.decl.?.* != .type_param_decl) {
                const key = Compiler_Context.Impl_Cache_Key{ .type_sym = type_sym, .trait = trait };
                try ctx.impl_cache.put(key, retval);
            }
        }
    }

    return retval;
}

/// True when `candidate` is `target`, or transitively lists `target` among its super-traits.
/// `visited` guards against cyclic trait inheritance
fn trait_is_or_extends(candidate: *Symbol, target: *Symbol, ctx: *Compiler_Context, visited: *std.AutoHashMap(*Symbol, void)) error{ CompileError, OutOfMemory }!bool {
    if (candidate == target) return true;
    if (visited.contains(candidate)) return false;
    try visited.put(candidate, {});
    const decl = candidate.decl orelse return false;
    if (decl.* != .trait) return false;
    for (decl.trait.super_traits.items) |super_trait| {
        const super_sym = try Decorate.symbol(super_trait, ctx);
        if (try trait_is_or_extends(super_sym, target, ctx, visited)) return true;
    }
    return false;
}

fn impl_trait_lookup_inner(self: *Self, original_scope: *Self, for_type: *Type_AST, trait: *Symbol, ctx: *Compiler_Context) error{ CompileError, OutOfMemory }!Impl_Trait_Lookup_Result {
    var retval: Impl_Trait_Lookup_Result = .{ .count = 0, .impl_ast = null, .subst = null };

    // Check if a type param has the trait as a constraint, or as a super trait of one of the constraints
    if (for_type.is_type_param()) {
        const type_param_decl = for_type.symbol().?.decl.?;
        for (type_param_decl.type_param_decl.constraints.items) |constraint| {
            const trait_symbol = try Decorate.symbol(constraint, ctx);
            var visited = std.AutoHashMap(*Symbol, void).init(ctx.allocator());
            defer visited.deinit();
            if (try trait_is_or_extends(trait_symbol, trait, ctx, &visited)) {
                return .{ .count = 1, .impl_ast = null, .subst = null };
            }
        }
    }

    // Check if the as_trait type is of the trait in question, or has it as a super trait
    if (for_type.* == .as_trait) {
        for (for_type.as_trait.constraints.items) |constraint| {
            const constraint_symbol = try Decorate.symbol(constraint, ctx);
            var visited = std.AutoHashMap(*Symbol, void).init(ctx.allocator());
            defer visited.deinit();
            if (try trait_is_or_extends(constraint_symbol, trait, ctx, &visited)) {
                const res = try self.impl_trait_lookup_inner(original_scope, for_type.lhs(), trait, ctx);
                if (res.count > 0) return res;
            }
        }
    }

    const constraints = if (for_type.* == .identifier and for_type.symbol().?.decl.?.* == .type_param_decl)
        for_type.symbol().?.decl.?.type_param_decl.constraints.items
    else
        &[_]*Type_AST{}; // empty slice
    const is_type_param = constraints.len > 0;

    // instantiate_generic_impl may append to module.impls, potentially reallocating the backing array
    var ii: usize = 0;
    while (ii < self.module.?.impls.items.len) : (ii += 1) {
        const impl = self.module.?.impls.items[ii];
        const impl_trait = try Decorate.symbol(impl.impl.trait.?, ctx);
        var traits_match = impl_trait == trait;
        if (!traits_match) {
            // Check if `trait` is a monomorphized form of some generic trait
            for (impl_trait.monomorphs.pairs.items) |pair| {
                if (pair.value == trait) {
                    traits_match = true;
                    break;
                }
            }
        }
        if (!traits_match) continue;

        // Decorate the impl type on-demand, since it may be looked up before its own decl is
        // decorated, leaving inner params (the `T` in `[]T`) unsymbolized and breaking unify.
        // An earlier fn indexing a `[]T` before the `impl Index for []T` decl triggers this.
        // Composite types are walked so their inner params resolve, not just identifiers
        if (impl.impl._type.* == .identifier and impl.impl._type.symbol() == null) {
            _ = try Decorate.symbol(impl.impl._type, ctx);
        } else if (impl.impl._type.* != .identifier) {
            try walker_.walk_type(impl.impl._type, Decorate.new(ctx));
        }

        var subst = unification_.Substitutions.init(std.heap.page_allocator);
        errdefer subst.deinit();
        unification_.unify(impl.impl._type, for_type, &subst, .{ .allow_rigid = !is_type_param }) catch {
            continue;
        };

        if (impl.impl._type.* == .identifier and impl.impl._type.symbol().?.decl.?.* == .type_param_decl) {
            const sat_res = for_type.satisfies_all_constraints(impl.impl._type.symbol().?.decl.?.type_param_decl.constraints.items, null, original_scope, ctx) catch continue;
            if (sat_res != .satisfies) continue;
        }

        if (is_type_param) {
            const sat_res = impl.impl._type.satisfies_all_constraints(constraints, null, original_scope, ctx) catch continue;
            if (sat_res != .satisfies) continue;
        }

        const instantiated_impl = try self.instantiate_generic_impl(impl, &subst, ctx);

        retval.count += 1;
        retval.impl_ast = retval.impl_ast orelse instantiated_impl;
        retval.subst = retval.subst orelse subst;
    }

    return retval;
}

pub fn as_trait_member_lookup(for_type: *Type_AST, traits: []*Type_AST, name: []const u8, matches: *std.array_hash_map.AutoArrayHashMap(*ast_.AST, void), ctx: *Compiler_Context) !void {
    try ctx.validate_type.validate_type(for_type);
    for (traits) |constraint| {
        const scope = constraint.scope().?;
        const constraint_symbol = try Decorate.symbol(constraint, ctx);
        const trait_decl = constraint_symbol.decl.?;
        if (trait_decl.* != .trait) continue; // a non-trait constraint has no members to dispatch through
        const res = try scope.impl_trait_lookup(for_type, constraint_symbol, ctx);
        if (res.count > 0) {
            if (res.impl_ast) |impl_ast| {
                if (search_impl(impl_ast, name)) |method| {
                    try matches.put(method, void{});
                }
            } else {
                // Type parameter satisfies the trait but no concrete impl exists yet.
                // Fall back to the abstract method_decl with Self substituted with for_type
                if (try scope.lookup_member_in_trait(trait_decl, for_type, name, ctx)) |method| {
                    try matches.put(method, void{});
                }
            }
        }
        try as_trait_member_lookup(for_type, trait_decl.trait.super_traits.items, name, matches, ctx);
    }
}

/// Looks up the impl's decl/method_decl ast for a given type, with a given name
pub fn lookup_impl_member(self: *Self, for_type: *Type_AST, name: []const u8, matches: *std.array_hash_map.AutoArrayHashMap(*ast_.AST, void), short_circuit: bool, compiler: *Compiler_Context) !void {
    var mut_short_circuit = short_circuit;

    // for type is a type parameter, check its trait constraints
    if (for_type.* == .identifier and for_type.symbol() != null and for_type.symbol().?.decl.?.* == .type_param_decl) {
        mut_short_circuit = true;
        const type_param_decl = for_type.symbol().?.decl.?;
        for (type_param_decl.type_param_decl.constraints.items) |constraint| {
            const trait_decl = (try Decorate.symbol(constraint, compiler)).decl.?;
            if (try self.lookup_member_in_trait(trait_decl, for_type, name, compiler)) |res| try matches.put(res, void{});
            if (mut_short_circuit and matches.keys().len > 0) return;
            for (trait_decl.trait.super_traits.items) |super_trait| {
                if (try self.lookup_member_in_trait(super_trait.symbol().?.decl.?, for_type, name, compiler)) |res| try matches.put(res, void{});
                if (mut_short_circuit and matches.keys().len > 0) return;
            }
        }
    }

    // Search this module and its transitive imports as a module graph
    if (self.module) |module| {
        var visited = std.AutoArrayHashMap(*module_.Module, void).init(compiler.allocator());
        defer visited.deinit();
        const root = compiler.module_scope(module.absolute_path) orelse self;
        try root.lookup_impl_member_in_modules(for_type, name, matches, mut_short_circuit, compiler, &visited);
    }
}

/// Scans the module's impls, then recurses into each imported module
fn lookup_impl_member_in_modules(self: *Self, for_type: *Type_AST, name: []const u8, matches: *std.array_hash_map.AutoArrayHashMap(*ast_.AST, void), short_circuit: bool, compiler: *Compiler_Context, visited: *std.AutoArrayHashMap(*module_.Module, void)) !void {
    const module = self.module orelse return;
    if (visited.contains(module)) return;
    try visited.put(module, {});

    try self.lookup_impl_member_impls(for_type, name, matches, short_circuit, compiler);
    if (short_circuit and matches.keys().len > 0) return;
    if (try self.lookup_impl_member_super_impls(for_type, name, compiler)) |res| try matches.put(res, void{});
    if (short_circuit and matches.keys().len > 0) return;

    for (module.local_imported_modules.keys()) |imported| {
        const imported_root = compiler.module_scope(imported.absolute_path) orelse continue;
        try imported_root.lookup_impl_member_in_modules(for_type, name, matches, short_circuit, compiler, visited);
        if (short_circuit and matches.keys().len > 0) return;
    }
}

pub fn lookup_member_in_trait(self: *Self, trait_decl: *ast_.AST, for_type: *Type_AST, name: []const u8, compiler: *Compiler_Context) !?*ast_.AST {
    if (self.parent == null) return null;

    // TODO: (for next release) De-duplicate this
    for (trait_decl.trait.method_decls.items) |method_decl| {
        if (!std.mem.eql(u8, method_decl.method_decl.name.token().data, name)) continue;

        var subst = unification_.Substitutions.init(compiler.allocator());
        defer subst.deinit();
        subst.put_type("Self", for_type) catch unreachable;
        const cloned = method_decl.clone(&subst, compiler.allocator());
        const new_scope = init(method_decl.symbol().?.scope.parent.?.parent.?, self.uid_gen, compiler.allocator());
        // new_scope skips the trait scope, so re-bind the trait's generic params so a default body referencing them still resolves
        // They stay generic and monomorphize per instantiation
        for (trait_decl.trait._generic_params.items) |param| {
            if (param.symbol()) |psym| new_scope.symbols.put(param.token().data, psym) catch unreachable;
        }
        if (for_type.* == .identifier and for_type.symbol() != null) {
            // Named `for_type`, bind `Self` straight to its symbol so a type param keeps its constraints for sibling dispatch, a type_alias would hide that from `is_type_param`
            new_scope.symbols.put("Self", for_type.symbol().?) catch unreachable;
        } else {
            // Structural `for_type` with no symbol, alias `Self` to it instead
            const self_token = Token.init_simple("Self");
            const self_type_decl = ast_.AST.create_type_alias(
                method_decl.token(),
                ast_.AST.create_pattern_symbol(self_token, .type, .local, "Self", compiler.allocator()),
                for_type,
                std.array_list.Managed(*ast_.AST).init(compiler.allocator()),
                compiler.allocator(),
            );
            try walker_.walk_ast(self_type_decl, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
        }
        try walker_.walk_ast(cloned, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
        try walker_.walk_ast(cloned, Decorate.new(compiler));
        return cloned;
    }
    for (trait_decl.trait.const_decls.items) |const_decl| {
        if (!std.mem.eql(u8, const_decl.binding.decls.items[0].decl.name.token().data, name)) continue;

        var subst = unification_.Substitutions.init(compiler.allocator());
        defer subst.deinit();
        subst.put_type("Self", for_type) catch unreachable;
        const cloned = const_decl.clone(&subst, compiler.allocator());
        // Parent at the const's declaring (module) scope, not the call site, so re-registering the
        // cloned const does not collide with the same-named const already visible at the call site
        const const_scope = const_decl.binding.decls.items[0].decl.name.symbol().?.scope;
        const new_scope = init(const_scope.parent.?, self.uid_gen, compiler.allocator());
        try walker_.walk_ast(cloned, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
        try walker_.walk_ast(cloned, Decorate.new(compiler));
        // Return the pattern, which carries the symbol, matching what search_impl returns for consts
        return cloned.binding.pattern;
    }
    for (trait_decl.trait.type_decls.items) |type_decl| {
        if (!std.mem.eql(u8, type_decl.token().data, name)) continue;
        return type_decl;
    }
    return null;
}

var count: usize = 0;
fn lookup_impl_member_impls(self: *Self, for_type: *Type_AST, name: []const u8, matches: *std.array_hash_map.AutoArrayHashMap(*ast_.AST, void), short_circuit: bool, compiler: *Compiler_Context) !void {
    if (self.module == null) return;
    const constraints = if (for_type.* == .identifier and for_type.symbol().?.decl.?.* == .type_param_decl)
        for_type.symbol().?.decl.?.type_param_decl.constraints.items
    else
        &[_]*Type_AST{}; // empty slice
    const is_type_param = constraints.len > 0;

    try compiler.validate_type.validate_type(for_type);

    // Go through all the impls of the module, check any that fit
    var i: usize = 0;
    while (i < self.module.?.impls.items.len) : (i += 1) {
        const impl = self.module.?.impls.items[i];

        var subst = unification_.Substitutions.init(compiler.allocator());
        defer subst.deinit();

        try compiler.validate_type.validate_type(impl.impl._type);

        unification_.unify(impl.impl._type, for_type, &subst, .{ .allow_rigid = !is_type_param }) catch {
            continue;
        };

        if (impl.impl._type.* == .identifier and impl.impl._type.symbol().?.decl.?.* == .type_param_decl) {
            const sat_res = for_type.satisfies_all_constraints(impl.impl._type.symbol().?.decl.?.type_param_decl.constraints.items, null, self, compiler) catch continue;
            if (sat_res != .satisfies) continue;
        }

        if (is_type_param) {
            const sat_res = impl.impl._type.satisfies_all_constraints(constraints, null, self, compiler) catch continue;
            if (sat_res != .satisfies) continue;
        }

        // Skip generic impl if it doesnt contain the item
        if (search_impl(impl, name) == null) continue;

        const instantiated_impl = try self.instantiate_generic_impl(impl, &subst, compiler);
        if (search_impl(instantiated_impl, name)) |res| {
            // `instantiate_generic_impl` returns the impl un-substituted when the subst maps this impl's params onto other generic params (like a super-trait's associated type from a sibling generic impl)
            // The found member still references this impl's own param symbols, so remap through the subst
            const remapped = if (instantiated_impl == impl and subst_renames_params(impl, &subst))
                remap_impl_member(res, &subst, compiler)
            else
                res;
            try matches.put(remapped, void{});
        }
        if (short_circuit and matches.keys().len > 0) return;
    }
}

/// True if `subst` maps any of `impl`'s generic type/const params to a different generic type/const params.
/// A type/const param `T`/`n` -> the same `T`/`n` resp. returns false (identity).
pub fn subst_renames_params(impl: *ast_.AST, subst: *const unification_.Substitutions) bool {
    for (impl.impl._generic_params.items) |param| {
        switch (param.*) {
            .type_param_decl => {
                const mapped = subst.get_type(param.symbol().?.name) orelse continue;
                if (mapped.* == .identifier and mapped.symbol() == param.symbol()) continue;
                if (mapped.is_generic()) return true;
            },
            .const_param_decl => {
                const mapped = subst.get_const(param.symbol().?.name) orelse continue;
                if ((mapped.* == .identifier) and mapped.symbol() == param.symbol()) continue;
                if (!unification_.const_arg_is_concrete(mapped)) return true;
            },
            else => {},
        }
    }
    return false;
}

/// Clones an impl member (associated `type`, `method`, or `const`) with `subst` applied, so it refers
/// to the querying impl's params instead of the sibling impl's. Needed when
/// `instantiate_generic_impl` returns the impl un-substituted (subst maps onto generic params)
/// For methods only the signature (`_decl_type`) is remapped, the body is checked at validate_impl
pub fn remap_impl_member(res: *ast_.AST, subst: *const unification_.Substitutions, compiler: *Compiler_Context) *ast_.AST {
    if (res.* != .type_alias and res.* != .method_decl and res.* != .pattern_symbol) return res;
    const old_symbol = res.symbol().?;
    if (res.* == .pattern_symbol) {
        // Remap just the type (like method_decl remaps just the signature) the value isn't needed until monomorphization
        const cloned_type = old_symbol.decl.?.decl_type().clone(subst, compiler.allocator());
        const new_scope = init(old_symbol.scope, old_symbol.scope.uid_gen, compiler.allocator());
        walker_.walk_type(cloned_type, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator())) catch {};
        const cloned_decl = ast_.AST.create_decl(res.token(), res.clone(subst, compiler.allocator()), cloned_type, null, compiler.allocator());
        const symbol = Symbol.init(
            old_symbol.scope,
            old_symbol.name,
            cloned_decl,
            old_symbol.kind,
            old_symbol.storage,
            compiler.allocator(),
        );
        cloned_decl.decl.name.set_symbol(symbol);
        return cloned_decl.decl.name;
    }
    const cloned = res.clone(subst, compiler.allocator());
    if (res.* == .method_decl) {
        // The cloned signature can contain access-types like `[]T::Output` whose scope is lost
        // by cloning. Re-scope just the function type so the access can re-resolve
        if (cloned.method_decl._decl_type) |decl_type| {
            const new_scope = init(old_symbol.scope, old_symbol.scope.uid_gen, compiler.allocator());
            walker_.walk_type(decl_type, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator())) catch {};
        }
    }
    const symbol = Symbol.init(
        old_symbol.scope,
        old_symbol.name,
        cloned,
        old_symbol.kind,
        old_symbol.storage,
        compiler.allocator(),
    );
    cloned.set_symbol(symbol);
    return cloned;
}

fn lookup_impl_member_super_impls(self: *Self, for_type: *Type_AST, name: []const u8, compiler: *Compiler_Context) !?*ast_.AST {
    if (self.module == null) return null;
    for (self.module.?.impls.items) |impl| {
        if (impl.impl.trait) |trait| {
            if (trait.symbol() == null) continue;
            for (trait.symbol().?.decl.?.trait.super_traits.items) |super_trait| {
                // Decorate the super-trait on-demand, since it may be reached before its own decl is decorated
                if (super_trait.symbol() == null) try walker_.walk_type(super_trait, Decorate.new(compiler));
                if (try self.lookup_member_in_trait(super_trait.symbol().?.decl.?, for_type, name, compiler)) |res| return res;
            }
        }
    }
    return null;
}

pub fn instantiate_generic_impl(self: *Self, impl: *ast_.AST, subst: *const unification_.Substitutions, compiler: *Compiler_Context) !*ast_.AST {
    // Not even generic
    if (impl.impl._generic_params.items.len == 0) return impl;

    // Substitution still contains generics, return original impl
    const subst_contains_generics = unification_.substitution_contains_generics(subst);
    if (subst_contains_generics) return impl;

    // Already instantiated, return the memoized impl
    const generic_arg_list = unification_.generic_arg_list_from_subst_map(subst, impl.impl._generic_params, compiler.allocator());
    if (impl.impl.instantiations.get(generic_arg_list)) |instantiated| return instantiated;

    // Create a new impl
    const new_impl: *ast_.AST = impl.clone(subst, compiler.allocator());
    // Pass on the virtual method count onto the monomorph
    new_impl.impl.num_virtual_methods = impl.impl.num_virtual_methods;
    impl.impl.instantiations.put(generic_arg_list, new_impl) catch unreachable;
    if (!subst_contains_generics) {
        new_impl.impl._generic_params.clearRetainingCapacity();
    }
    const new_scope = init(impl.scope().?.parent.?, self.uid_gen, compiler.allocator());
    try walker_.walk_ast(new_impl, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
    new_impl.set_scope(new_scope);

    // Re-attach the trait symbol after cloning
    if (new_impl.impl.trait != null and !new_impl.impl.impls_anon_trait) {
        if (new_impl.impl.trait.?.* == .generic_apply) {
            // Generic trait, monomorphize it for these args so the vtable references a concrete trait
            // The clone dropped the lhs trait-name symbol, so copy it from the template before instantiating
            if (impl.impl.trait.?.lhs().symbol()) |lhs_sym| {
                new_impl.impl.trait.?.lhs().set_symbol(lhs_sym);
            }
            try generic_apply_.instantiate(new_impl.impl.trait.?, compiler);
        } else if (impl.impl.trait.?.symbol()) |sym| {
            new_impl.impl.trait.?.set_symbol(sym);
        }
    }

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
            std.array_list.Managed(*ast_.AST).init(compiler.allocator()),
            std.array_list.Managed(*Type_AST).init(compiler.allocator()),
            new_impl.impl.method_defs,
            new_impl.impl.const_defs,
            new_impl.impl.type_defs,
            compiler.allocator(),
        );
        try walker_.walk_ast(anon_trait, Symbol_Tree.new(new_scope, &compiler.errors, compiler.allocator()));
        new_impl.impl.trait = ast_.AST.create_identifier(token, compiler.allocator());
        new_impl.impl.trait.?.set_symbol(anon_trait.symbol().?);
        new_impl.impl.impls_anon_trait = true;
    }

    try compiler.validate_scope.validate_scope(new_scope);

    // Set all method_defs and const_defs to be monomorphed
    for (new_impl.impl.method_defs.items) |method_def| {
        method_def.symbol().?.is_monomorphed = true;
    }
    for (new_impl.impl.const_defs.items) |const_def| {
        const_def.binding.pattern.symbol().?.is_monomorphed = true;
    }

    // Store in the memo
    return impl.impl.instantiations.get(generic_arg_list) orelse new_impl; // TODO: substitutions need to be in the same order as generic params
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
            return const_def.binding.pattern;
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
    if (self.module) |mod| {
        for (mod.impls.items) |impl| {
            std.debug.print("  impl {?f} for {f}\n", .{ impl.impl.trait, impl.impl._type });
        }
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

pub fn collect_traits_and_enums(
    self: *Self,
    traits: *std.array_hash_map.AutoArrayHashMap(*ast_.AST, void),
    enums: *std.array_hash_map.AutoArrayHashMap(*ast_.AST, void),
) void {
    for (self.traits.keys()) |trait| {
        traits.put(trait, void{}) catch unreachable;
    }
    for (self.enums.keys()) |@"enum"| {
        enums.put(@"enum", void{}) catch unreachable;
    }

    for (self.children.items) |child| {
        child.collect_traits_and_enums(traits, enums);
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
