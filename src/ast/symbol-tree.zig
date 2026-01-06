//! This file represents a struct for an AST walk, and constructs the symbol tree for the AST.

const std = @import("std");
const anon_name_ = @import("../util/anon_name.zig");
const ast_ = @import("../ast/ast.zig");
const Canonical_Type_Fmt = @import("../types/canonical_type_fmt.zig");
const core_ = @import("../hierarchy/core.zig");
const errs_ = @import("../util/errors.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const Token = @import("../lexer/token.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Tree_Writer = @import("../ast/tree_writer.zig");
const walk_ = @import("../ast/walker.zig");

scope: *Scope,
in_loop: bool,
is_param_scope: bool,
defers: ?*std.array_list.Managed(*ast_.AST),
errors: *errs_.Errors,
allocator: std.mem.Allocator,

const Self = @This();
const Error = walk_.Error;

pub fn new(scope: *Scope, errors: *errs_.Errors, allocator: std.mem.Allocator) Self {
    return Self{
        .scope = scope,
        .in_loop = false,
        .is_param_scope = false,
        .defers = null,
        .errors = errors,
        .allocator = allocator,
    };
}

pub fn prefix(self: Self, ast: *ast_.AST) walk_.Error!?Self {
    return self.symbol_tree_prefix(ast);
}

pub fn prefix_type(self: Self, _type: *Type_AST) walk_.Error!?Self {
    return self.symbol_tree_type_prefix(_type);
}

pub fn postfix(self: Self, ast: *ast_.AST) walk_.Error!void {
    return self.symbol_tree_postfix(ast);
}

fn symbol_tree_prefix(self: Self, ast: *ast_.AST) walk_.Error!?Self {
    switch (ast.*) {
        else => {},

        // Capture scope
        .@"comptime", .access, .call, .invoke, .addr_of, .identifier, .generic_apply, .print, .format_args, .write, .type_access => ast.set_scope(self.scope),

        // Check that AST is inside a loop
        .@"break", .@"continue" => try self.in_loop_check(ast, self.errors),

        // Check that AST is inside a function
        .@"try", .@"return" => ast.set_symbol(try in_function_check(ast, self.scope, self.errors)),

        // Add defers to scope
        .@"defer" => if (self.defers) |defers| {
            try defers.append(ast);
        } else {
            // TODO: an error
        },

        .@"errdefer" => if (self.defers) |defers| {
            try defers.append(ast);
        } else {
            // TODO: an error
        },

        // Create a new scope, pass it to children
        .@"if", .match, .@"while", .@"for" => {
            var new_self = self.new_scope(ast);
            new_self.in_loop = new_self.in_loop or ast.* == .@"while" or ast.* == .@"for";
            return new_self;
        },

        // Create a new scope, set defers, pass it to children
        .block => {
            var new_self = self.new_scope(ast);
            if (ast.block.defers.items.len == 0) {
                // Only register defers list to be appended if the list is empty
                new_self.defers = &ast.block.defers;
            }
            new_self.in_loop = new_self.in_loop or ast.* == .@"while" or ast.* == .@"for";
            return new_self;
        },

        // Create symbols (potentially >1) from pattern, put inside scope
        .binding => {
            var symbols = std.array_list.Managed(*Symbol).init(self.allocator);
            defer symbols.deinit();
            try create_symbol(
                &symbols,
                ast.binding.pattern,
                ast,
                ast.binding.type,
                ast.binding.init,
                self.scope,
                self.errors,
                self.allocator,
            );
            for (symbols.items) |symbol| {
                try ast.binding.decls.append(symbol.decl.?);
            }
            try self.scope.put_all_symbols(&symbols, self.errors);
        },

        .struct_decl, .enum_decl, .type_alias => {
            std.debug.assert(ast.symbol() == null);

            const new_self = self.new_scope(ast);

            const symbol = Symbol.init(
                self.scope,
                ast.decl_name().token().data,
                ast,
                .type,
                ast.decl_name().pattern_symbol.storage,
                self.allocator,
            );
            try self.register_symbol(ast, symbol);

            return new_self;
        },

        .context_value_decl => {
            std.debug.assert(ast.symbol() == null);

            const number = self.scope.num_visible_contexts();
            var out = std.array_list.Managed(u8).init(self.allocator);
            defer out.deinit();
            try out.print("context_value__{}", .{number});

            const symbol = Symbol.init(
                self.scope,
                try out.toOwnedSlice(),
                ast,
                .let,
                .local,
                self.allocator,
            );
            try self.register_symbol(ast, symbol);

            return self;
        },

        .context_decl => {
            std.debug.assert(ast.symbol() == null);

            const new_self = self.new_scope(ast);

            const symbol = Symbol.init(
                self.scope,
                ast.decl_name().token().data,
                ast,
                .context,
                ast.decl_name().pattern_symbol.storage,
                self.allocator,
            );
            try self.register_symbol(ast, symbol);

            return new_self;
        },

        .type_param_decl => {
            const symbol = Symbol.init(
                self.scope,
                ast.token().data,
                ast,
                .type,
                .local,
                self.allocator,
            );
            try self.register_symbol(ast, symbol);
        },

        .@"test" => {
            var new_self = self.new_scope(ast);

            const symbol = try create_test_symbol(ast, self.allocator);
            try self.register_symbol(ast, symbol);

            new_self.scope.function_depth = self.scope.function_depth + 1;

            return new_self;
        },

        // Create a symbol for this function
        .fn_decl => {
            // this can happen sometimes with: decl(fn_decl()), its fine, just return
            if (ast.symbol() != null) return null;

            var new_self = self.new_scope(ast);

            const symbol = try create_function_symbol(ast, self.allocator);
            try self.register_symbol(ast, symbol);

            new_self.scope.function_depth = self.scope.function_depth + 1;

            return new_self;
        },

        // Create new scope, create and walk trait symbols/decls
        .trait => {
            // this can happen sometimes with anonymous traits, its fine, just return
            if (ast.symbol() != null) return null;

            const new_self = self.new_scope(ast);

            const symbol = try create_trait_symbol(ast, self.scope, self.allocator);
            try self.register_symbol(ast, symbol);

            const self_token = Token.init_simple("Self");
            var self_identifier = Type_AST.create_type_identifier(self_token, self.allocator);
            self_identifier.set_symbol(symbol);
            var self_type_constraints = std.array_list.Managed(*Type_AST).init(self.allocator);
            try self_type_constraints.append(self_identifier);
            const self_type_decl = ast_.AST.create_type_param_decl(
                self_token,
                true,
                self_type_constraints,
                self.allocator,
            );
            try walk_.walk_ast(self_type_decl, new_self);

            return new_self;
        },

        // Create new scope, create anon trait, create and walk impl symbols/decls
        .impl => {
            // Impls get their own scope, actually
            const new_self = self.new_scope(ast);

            const self_type_decl = ast_.AST.create_type_alias(
                ast.token(),
                ast_.AST.create_pattern_symbol(
                    Token.init_simple("Self"),
                    .type,
                    .local,
                    "Self",
                    self.allocator,
                ),
                ast.impl._type,
                std.array_list.Managed(*ast_.AST).init(self.allocator),
                self.allocator,
            );
            try walk_.walk_ast(self_type_decl, new_self);

            for (ast.impl.method_defs.items, 0..) |method_def, i| {
                var subst = std.StringArrayHashMap(*Type_AST).init(self.allocator);
                defer subst.deinit();

                try subst.put("Self", ast.impl._type);

                ast.impl.method_defs.items[i] = method_def.clone(&subst, self.allocator);
            }

            if (ast.impl.trait == null) {
                // impl'd for an anon trait, create an anon trait for it
                var new_self_for_anon_trait = self;
                new_self_for_anon_trait.scope = Scope.init(self.scope, self.scope.uid_gen, self.allocator);

                // TODO: Don't do this for generic impls (but those'll be a new node anyway)
                var subst = std.StringArrayHashMap(*Type_AST).init(self.allocator);
                defer subst.deinit();

                var token = ast.token();
                token.kind = .identifier;
                token.data = anon_name_.next_anon_name("Trait", self.allocator);
                const cloned_methods = ast_.AST.clone_children(ast.impl.method_defs, &subst, self.allocator);
                for (cloned_methods.items) |cloned_method| {
                    cloned_method.method_decl.init = null;
                }
                const anon_trait = ast_.AST.create_trait(
                    token,
                    ast_.AST.create_pattern_symbol(
                        token,
                        .trait,
                        .local,
                        token.data,
                        self.allocator,
                    ),
                    std.array_list.Managed(*ast_.AST).init(self.allocator),
                    std.array_list.Managed(*Type_AST).init(self.allocator),
                    cloned_methods,
                    ast_.AST.clone_children(ast.impl.const_defs, &subst, self.allocator),
                    ast_.AST.clone_children(ast.impl.type_defs, &subst, self.allocator),
                    self.allocator,
                );
                try new_self_for_anon_trait.scope.traits.put(anon_trait, void{});
                try walk_.walk_ast(anon_trait, new_self_for_anon_trait);
                ast.impl.trait = ast_.AST.create_identifier(token, self.allocator);
                ast.impl.trait.?.set_symbol(anon_trait.symbol().?);
                ast.impl.impls_anon_trait = true;
            }

            try self.scope.impls.append(ast);

            return new_self;
        },

        // Create scope
        .method_decl => {
            if (ast.symbol() != null) return null;

            var new_self = self.new_scope(ast);

            const symbol = try create_method_symbol(ast, self.errors, self.allocator);
            try self.register_symbol(ast, symbol);

            new_self.scope.function_depth = new_self.scope.function_depth + 1;
            std.debug.assert(ast.symbol() != null);

            return new_self;
        },
    }

    return self;
}

fn symbol_tree_type_prefix(self: Self, _type: *Type_AST) walk_.Error!?Self {
    switch (_type.*) {
        .access, .generic_apply, .identifier => {
            _type.set_scope(self.scope);
        },
        else => {},
    }
    return self;
}

fn register_symbol(self: Self, ast: *ast_.AST, symbol: *Symbol) walk_.Error!void {
    self.scope.put_symbol(symbol, self.errors) catch |e| {
        return e;
    };
    ast.set_symbol(symbol);
}

fn new_scope(self: Self, ast: *ast_.AST) Self {
    var new_self = self;
    new_self.scope = Scope.init(self.scope, self.scope.uid_gen, self.allocator);
    ast.set_scope(new_self.scope);
    return new_self;
}

fn symbol_tree_postfix(self: Self, ast: *ast_.AST) walk_.Error!void {
    switch (ast.*) {
        else => {},

        .match => {
            try create_match_pattern_symbol(ast, self.scope, self.errors, self.allocator);
        },

        .trait => {
            for (ast.trait.method_decls.items) |method_decl| {
                method_decl.method_decl.c_type = try create_method_type(method_decl, self.allocator);
            }
        },

        .fn_decl => {
            for (ast.children().items) |param_binding| {
                const symbol = param_binding.binding.decls.items[0].decl.name.symbol().?;
                try ast.param_symbols().?.append(symbol);
                symbol.defined = true;
                symbol.param = true;
            }
            for (ast.fn_decl.context_decls.items) |context_param| {
                const symbol = context_param.symbol().?;
                try ast.context_param_symbols().?.append(symbol);
                symbol.defined = true;
                symbol.param = true;
            }
        },

        .@"test" => {
            for (ast.@"test".context_decls.items) |context_param| {
                const symbol = context_param.symbol().?;
                try ast.context_param_symbols().?.append(symbol);
                symbol.defined = true;
                symbol.param = true;
            }
        },

        .method_decl => {
            for (ast.children().items) |param_binding| {
                const symbol = param_binding.binding.decls.items[0].decl.name.symbol().?;
                try ast.param_symbols().?.append(symbol);
                symbol.defined = true;
                symbol.param = true;
            }
            for (ast.method_decl.context_decls.items) |context_param| {
                const symbol = context_param.symbol().?;
                try ast.context_param_symbols().?.append(symbol);
                symbol.defined = true;
                symbol.param = true;
            }
        },
    }
}

fn in_loop_check(self: Self, ast: *ast_.AST, errors: *errs_.Errors) Error!void {
    if (!self.in_loop) {
        errors.add_error(errs_.Error{ .not_inside_loop = .{
            .span = ast.token().span,
            .name = @tagName(ast.*),
        } });
        return error.CompileError;
    }
}

/// Returns the inner symbol of a scope, or an error if one doesnt exist
fn in_function_check(ast: *ast_.AST, scope: *Scope, errors: *errs_.Errors) Error!*Symbol {
    if (scope.inner_function == null) {
        errors.add_error(errs_.Error{ .not_inside_function = .{
            .span = ast.token().span,
            .name = @tagName(ast.*),
        } });
        return error.CompileError;
    } else {
        return scope.inner_function.?;
    }
}

/// Creates symbols from a given pattern, declaration, type, and initializer within the given scope.
fn create_symbol(
    symbols: *std.array_list.Managed(*Symbol), // Mutable list that accumulates all the created symbols for a given scope, to be put() later.
    pattern: *ast_.AST, // Represents the pattern being matched; processed recursively to create symbols.
    decl: ?*ast_.AST, // Potential decl AST to be given to symbols.
    _type: *Type_AST, // Type of the current pattern.
    init: ?*ast_.AST, // The value being matched on. Null for function parameter symbols.
    scope: *Scope,
    errors: *errs_.Errors,
    allocator: std.mem.Allocator,
) Error!void {
    switch (pattern.*) {
        .pattern_symbol => {
            if (std.mem.eql(u8, pattern.pattern_symbol.name, "_")) {
                if (pattern.pattern_symbol.kind != .let) {
                    // It is an error for `_` to be marked as `const` or `mut`
                    errors.add_error(errs_.Error{ .discard_marked = .{
                        .span = pattern.token().span,
                        .kind = pattern.pattern_symbol.kind,
                    } });
                    return error.CompileError;
                } else {
                    // Register the symbol of the symbol pattern as the blackhole symbol, but do not append
                    pattern.set_symbol(prelude_.blackhole);
                    return;
                }
            }

            if (pattern.symbol()) |sym| {
                try symbols.append(sym);
                return;
            }

            const symbol = Symbol.init(
                scope,
                pattern.pattern_symbol.name,
                ast_.AST.create_decl(pattern.token(), pattern, _type, init, allocator),
                pattern.pattern_symbol.kind,
                pattern.pattern_symbol.storage,
                allocator,
            );
            pattern.set_symbol(symbol);
            try symbols.append(symbol);
        },
        .tuple_value, .array_value => {
            for (pattern.children().items, 0..) |term, i| {
                const index = ast_.AST.create_int(pattern.token(), i, allocator);
                const new_type: *Type_AST = Type_AST.create_index_type(_type.token(), _type, index, allocator);
                var index_rhs = std.array_list.Managed(*ast_.AST).init(allocator);
                try index_rhs.append(index);
                const new_init: ?*ast_.AST = if (init != null) ast_.AST.create_index(init.?.token(), init.?, index_rhs, allocator) else null;
                try create_symbol(symbols, term, decl, new_type, new_init, scope, errors, allocator);
            }
        },
        .enum_value => {
            const lhs_type = Type_AST.create_type_of(pattern.token(), init.?, allocator);
            const rhs_type = Type_AST.create_domain_of(pattern.token(), lhs_type, pattern.enum_value.get_name(), allocator);

            if (pattern.enum_value.init != null) {
                // Here init is null!
                try create_symbol(symbols, pattern.enum_value.init.?, decl, rhs_type, pattern.enum_value.init.?, scope, errors, allocator);
            }
        },

        // Likely literals etc, for `match` mappings
        else => {},
    }
}

/// Creates and initializes pattern symbols for each mapping in a match expression, defining the symbols within the
/// mapping's scope.
fn create_match_pattern_symbol(match: *ast_.AST, scope: *Scope, errors: *errs_.Errors, allocator: std.mem.Allocator) Error!void {
    for (match.children().items) |mapping| {
        const mpaping_scope = Scope.init(scope, scope.uid_gen, allocator);
        mapping.set_scope(mpaping_scope);
        var symbols = std.array_list.Managed(*Symbol).init(allocator);
        defer symbols.deinit();
        const match_expr = match.expr();
        const match_expr_token = match_expr.token();
        const _type = Type_AST.create_type_of(match_expr_token, match_expr, allocator);
        try create_symbol(&symbols, mapping.lhs(), null, _type, match_expr, mpaping_scope, errors, allocator);
        for (symbols.items) |symbol| {
            symbol.defined = true;
        }
        for (symbols.items) |symbol| {
            try mapping.mapping.captures.append(symbol.decl.?);
        }
        try mpaping_scope.put_all_symbols(&symbols, errors);
        try walk_.walk_ast(mapping.rhs(), Self.new(mpaping_scope, errors, allocator));
    }
}

pub fn create_function_symbol(ast: *ast_.AST, allocator: std.mem.Allocator) !*Symbol {
    // Calculate the domain type from the function paramter types
    const domain = try extract_domain(ast.children().*, allocator);

    var contexts = std.array_list.Managed(*Type_AST).init(allocator);
    for (ast.fn_decl.context_decls.items) |ctx| {
        try contexts.append(ctx.context_value_decl.parent);
    }

    // Create the function type
    const _type = Type_AST.create_function(
        ast.fn_decl.ret_type.token(),
        domain,
        ast.fn_decl.ret_type,
        contexts,
        allocator,
    );
    ast.fn_decl._decl_type = _type;

    // Choose name (maybe anon)
    var buf: []const u8 = undefined;
    if (ast.fn_decl.name) |name| {
        buf = name.token().data;
    } else {
        buf = anon_name_.next_anon_name("anon", allocator);
    }

    // Create the symbol
    const retval = Symbol.init(
        ast.scope().?,
        buf,
        ast,
        .@"fn",
        .local,
        allocator,
    );
    ast.scope().?.inner_function = retval;
    return retval;
}

pub fn create_test_symbol(ast: *ast_.AST, allocator: std.mem.Allocator) Error!*Symbol {
    var contexts = std.array_list.Managed(*Type_AST).init(allocator);
    for (ast.@"test".context_decls.items) |ctx| {
        try contexts.append(ctx.context_value_decl.parent);
    }

    const args = std.array_list.Managed(*Type_AST).init(allocator);
    const _type = Type_AST.create_function(
        ast.token(),
        args,
        core_.test_result_type,
        contexts,
        allocator,
    );
    ast.@"test"._decl_type = _type;

    // Choose name (maybe anon)
    const buf: []const u8 = anon_name_.next_anon_name("test", allocator);
    const retval = Symbol.init(
        ast.scope().?,
        buf,
        ast,
        .@"test",
        .local,
        allocator,
    );
    ast.scope().?.inner_function = retval;

    return retval;
}

pub fn extract_domain(params: std.array_list.Managed(*ast_.AST), allocator: std.mem.Allocator) !std.array_list.Managed(*Type_AST) {
    var param_types = std.array_list.Managed(*Type_AST).init(allocator);
    for (0..params.items.len) |i| {
        try param_types.append(Type_AST.create_annotation(
            params.items[i].token(),
            params.items[i].binding.pattern,
            params.items[i].binding.type,
            params.items[i].binding.init,
            allocator,
        ));
    }
    return param_types;
}

fn extract_domain_with_receiver(impl_type: *Type_AST, receiver: *ast_.AST, params: std.array_list.Managed(*ast_.AST), allocator: std.mem.Allocator) !std.array_list.Managed(*Type_AST) {
    const receiver_addr_type = create_receiver_addr(impl_type, receiver, allocator);
    receiver.receiver._type = receiver_addr_type;
    const _receiver_type = create_receiver_annot(receiver_addr_type, receiver, allocator);
    var param_types = std.array_list.Managed(*Type_AST).init(allocator);
    try param_types.append(_receiver_type);
    for (0..params.items.len) |i| {
        try param_types.append(Type_AST.create_annotation(
            params.items[i].token(),
            params.items[i].binding.pattern,
            params.items[i].binding.type,
            params.items[i].binding.init,
            allocator,
        ));
    }
    return param_types;
}

fn create_receiver_addr(impl_type: *Type_AST, receiver: *ast_.AST, allocator: std.mem.Allocator) *Type_AST {
    if (impl_type.* == .anyptr_type) return impl_type; // If the type is already an anyptr, then dont make it an address too
    return switch (receiver.receiver.kind) {
        .value, .addr_of => Type_AST.create_addr_of_type(receiver.token(), impl_type, false, false, allocator),
        .mut_addr_of => Type_AST.create_addr_of_type(receiver.token(), impl_type, true, false, allocator),
    };
}

fn create_receiver_annot(receiver_addr: *Type_AST, receiver: *ast_.AST, allocator: std.mem.Allocator) *Type_AST {
    return Type_AST.create_annotation(
        receiver.token(),
        receiver,
        receiver_addr,
        null,
        allocator,
    );
}

/// Creates a symbol which represents the comptime function to be ran.
pub fn create_temp_comptime_symbol(
    ast: *ast_.AST,
    rhs_type_hint: ?*Type_AST,
    scope: *Scope,
    allocator: std.mem.Allocator,
) Error!*Symbol {
    // Create the function type. The rhs is a typeof node, since type expansion is done in a later time
    const args = std.array_list.Managed(*Type_AST).init(allocator);
    const rhs = Type_AST.create_type_of(ast.token(), ast, allocator);
    const _type = Type_AST.create_function(
        ast.token(),
        args,
        rhs_type_hint orelse rhs,
        std.array_list.Managed(*Type_AST).init(allocator),
        allocator,
    );

    // Create the comptime scope
    // This is to prevent `comptime` expressions from using runtime variables
    var comptime_scope = Scope.init(scope, scope.uid_gen, allocator);
    comptime_scope.function_depth = scope.function_depth + 1;

    // Choose name
    var buf: []const u8 = undefined;
    buf = anon_name_.next_anon_name("comptime", allocator);

    const decl = ast_.AST.create_decl(
        ast.token(),
        ast_.AST.create_pattern_symbol(
            ast.token(),
            .@"comptime",
            .local,
            buf,
            allocator,
        ),
        _type,
        ast,
        allocator,
    );

    // Create the symbol
    const retval = Symbol.init(
        comptime_scope,
        buf,
        decl,
        .@"comptime",
        .local,
        allocator,
    );
    comptime_scope.inner_function = retval;

    return retval;
}

fn create_trait_symbol(
    ast: *ast_.AST,
    scope: *Scope,
    allocator: std.mem.Allocator,
) Error!*Symbol {
    // Create the symbol
    const retval = Symbol.init(
        scope,
        ast.trait.name.token().data,
        ast,
        .trait,
        .local,
        allocator,
    );
    retval.defined = true;
    return retval;
}

fn create_method_type(ast: *ast_.AST, allocator: std.mem.Allocator) !*Type_AST {
    const receiver_base_type: *Type_AST = if (ast.method_decl.impl != null)
        ast.method_decl.impl.?.impl._type
    else
        Type_AST.create_anyptr_type(ast.token(), allocator);

    // Calculate the domain type from the function paramter types
    const args = if (ast.method_decl.receiver) |receiver|
        try extract_domain_with_receiver(receiver_base_type, receiver, ast.children().*, allocator)
    else
        try extract_domain(ast.children().*, allocator);

    var contexts = std.array_list.Managed(*Type_AST).init(allocator);
    for (ast.method_decl.context_decls.items) |ctx| {
        try contexts.append(ctx.context_value_decl.parent);
    }

    // Create the function type
    const _type = Type_AST.create_function(
        ast.method_decl.ret_type.token(),
        args,
        ast.method_decl.ret_type,
        contexts,
        allocator,
    );
    return _type;
}

fn create_method_symbol(
    ast: *ast_.AST,
    errors: *errs_.Errors,
    allocator: std.mem.Allocator,
) Error!*Symbol {
    const receiver_base_type: ?*Type_AST = if (ast.method_decl.impl) |impl| impl.impl._type else null;

    // Create the function type
    ast.method_decl._decl_type = try create_method_type(ast, allocator);

    if (ast.method_decl.receiver != null and receiver_base_type != null) {
        // addr-of receiver, prepend receiver to parameters as normal
        const recv_type = create_receiver_addr(receiver_base_type.?, ast.method_decl.receiver.?, allocator);
        recv_type.addr_of.anytptr = true;
        // value receiver, prepend a void* self_ptr parameter
        const receiver_symbol = Symbol.init(
            ast.scope().?,
            if (ast.method_decl.receiver.?.receiver.kind == .value) "self__ptr" else "self",
            ast.method_decl.receiver,
            .let,
            .local,
            allocator,
        );
        receiver_symbol.param = true;
        try ast.scope().?.put_symbol(receiver_symbol, errors);
        try ast.param_symbols().?.append(receiver_symbol);

        if (ast.method_decl.receiver.?.receiver.kind == .value and !ast.method_decl.receiver.?.receiver.value_derefd) {
            const self_type = recv_type.child();
            const self_init = ast_.AST.create_dereference(ast.token(), ast_.AST.create_identifier(Token.init_simple("self__ptr"), allocator), allocator);
            const receiver_span = ast.method_decl.receiver.?.token().span;
            const self_decl = ast_.AST.create_binding(
                ast.token(),
                ast_.AST.create_pattern_symbol(Token.init("self", .identifier, receiver_span.filename, receiver_span.line_text, receiver_span.line_number, receiver_span.col), .let, .local, "self", allocator),
                self_type,
                self_init,
                allocator,
            );
            if (ast.method_decl.init != null) {
                if (ast.method_decl.init.?.* != .unit_value) {
                    const method_block_statements = ast.method_decl.init.?.children();
                    try method_block_statements.insert(0, self_decl);
                } else {
                    // Technically, init COULD be `{ }`, and it would cause a not-used error later on, but we need to handle this properly here before then
                    var statements = std.array_list.Managed(*ast_.AST).init(allocator);
                    try statements.append(self_decl);
                    ast.method_decl.init = ast_.AST.create_block(ast.method_decl.init.?.token(), statements, null, allocator);
                }
            }
            ast.method_decl.receiver.?.receiver.value_derefd = true;
        }
    }

    const retval = Symbol.init(
        ast.scope().?,
        ast.method_decl.name.token().data,
        ast,
        .@"fn",
        .local,
        allocator,
    );
    ast.scope().?.inner_function = retval;

    return retval;
}
