//! This file represents a struct for an AST walk, and constructs the symbol tree for the AST.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const core_ = @import("../hierarchy/core.zig");
const errs_ = @import("../util/errors.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const Token = @import("../lexer/token.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Type_Map = @import("../ast/type_map.zig").Type_Map;
const walk_ = @import("../ast/walker.zig");

scope: *Scope,
in_loop: bool,
is_param_scope: bool,
defers: ?*std.ArrayList(*ast_.AST),
errdefers: ?*std.ArrayList(*ast_.AST),
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
        .errdefers = null,
        .errors = errors,
        .allocator = allocator,
    };
}

pub fn prefix(self: Self, ast: *ast_.AST) walk_.Error!?Self {
    switch (ast.*) {
        else => {},

        // Capture scope
        .@"comptime", .access, .invoke, .addr_of => ast.set_scope(self.scope),

        // Check that AST is inside a loop
        .@"break", .@"continue" => try self.in_loop_check(ast, self.errors),

        // Check that AST is inside a function
        .@"try", .@"return" => ast.set_symbol(try in_function_check(ast, self.scope, self.errors)),

        // Add defers to scope
        .@"defer" => if (self.defers) |defers| {
            defers.append(ast.statement()) catch unreachable;
        } else {
            // TODO: an error
        },

        .@"errdefer" => if (self.errdefers) |errdefers| {
            errdefers.append(ast.statement()) catch unreachable;
        } else {
            // TODO: an error
        },

        // Create a new scope, pass it to children
        .@"if", .match, .@"while", .@"for" => {
            var new_self = self;
            new_self.scope = Scope.init(self.scope, self.scope.uid_gen, self.allocator);
            new_self.in_loop = new_self.in_loop or ast.* == .@"while" or ast.* == .@"for";
            ast.set_scope(new_self.scope);
            return new_self;
        },

        // Create a new scope, set defers, pass it to children
        .block => {
            var new_self = self;
            new_self.scope = Scope.init(self.scope, self.scope.uid_gen, self.allocator);
            new_self.defers = &ast.block.defers;
            new_self.errdefers = &ast.block.errdefers;
            new_self.in_loop = new_self.in_loop or ast.* == .@"while" or ast.* == .@"for";
            ast.set_scope(new_self.scope);
            return new_self;
        },

        // Create symbols (potentially >1) from pattern, put inside scope
        .binding => {
            var symbols = std.ArrayList(*Symbol).init(self.allocator);
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
                ast.binding.decls.append(symbol.decl.?) catch unreachable;
            }
            try self.scope.put_all_symbols(&symbols, self.errors);
        },

        .struct_decl, .enum_decl, .type_alias => {
            var new_self = self;
            new_self.scope = Scope.init(self.scope, self.scope.uid_gen, self.allocator);

            const symbol = Symbol.init(
                self.scope,
                ast.decl_name().token().data,
                ast,
                .type,
                ast.decl_name().pattern_symbol.storage,
                self.allocator,
            );
            try self.register_symbol(ast, symbol);

            ast.set_scope(new_self.scope);
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
            const symbol = try create_test_symbol(ast, self.scope, self.errors, self.allocator);
            try self.register_symbol(ast, symbol);
            return null;
        },

        // Create a symbol for this function
        // Transform into template if templated
        .fn_decl => {
            if (ast.symbol() != null) {
                // Do not re-do symbol if already declared
                return null;
            } else if (ast.fn_decl.is_templated()) {
                // TOOD: stamp
            } else {
                // Normal function declaration
                const symbol = try create_function_symbol(ast, self.scope, self.errors, self.allocator);
                try self.register_symbol(ast, symbol);
            }

            return null; // NOTE: DO NOT WALK CHILDREN!
        },

        // Create new scope, create and walk trait symbols/decls
        .trait => {
            if (ast.symbol() != null) {
                // Do not re-do symboo if already declared
                return null;
            }
            var new_self = self;
            new_self.scope = Scope.init(self.scope, self.scope.uid_gen, self.allocator);
            ast.set_scope(new_self.scope);
            const symbol = try create_trait_symbol(ast, self.scope, self.allocator);
            try self.register_symbol(ast, symbol);

            const self_type_decl = ast_.AST.create_type_alias(
                ast.token(),
                ast_.AST.create_pattern_symbol(
                    Token.init_simple("Self"),
                    .type,
                    .local,
                    "Self",
                    self.allocator,
                ),
                null,
                std.ArrayList(*ast_.AST).init(self.allocator),
                self.allocator,
            );
            try walk_.walk_ast(self_type_decl, new_self);

            return new_self;
        },

        // Create new scope, create anon trait, create and walk impl symbols/decls
        .impl => {
            // Impls get there own scope, actually
            var new_self = self;
            new_self.scope = Scope.init(self.scope, self.scope.uid_gen, self.allocator);
            ast.set_scope(new_self.scope);

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
                std.ArrayList(*ast_.AST).init(self.allocator),
                self.allocator,
            );
            try walk_.walk_ast(self_type_decl, new_self);

            if (ast.impl.trait == null) {
                // impl'd for an anon trait, create an anon trait for it
                var new_self_for_anon_trait = self;
                new_self_for_anon_trait.scope = Scope.init(self.scope, self.scope.uid_gen, self.allocator);

                // TODO: Don't do this for generic impls (but those'll be a new node anyway)
                var subst = std.StringArrayHashMap(*Type_AST).init(self.allocator);
                defer subst.deinit();

                var token = ast.token();
                token.kind = .identifier;
                token.data = next_anon_name("Trait", self.allocator);
                const cloned_methods = ast_.AST.clone_children(ast.impl.method_defs, &subst, self.allocator);
                for (cloned_methods.items) |cloned_method| {
                    cloned_method.method_decl.init = null;
                }
                const anon_trait = ast_.AST.create_trait(
                    token,
                    cloned_methods,
                    ast_.AST.clone_children(ast.impl.const_defs, &subst, self.allocator),
                    self.allocator,
                );
                try walk_.walk_ast(anon_trait, new_self_for_anon_trait);
                ast.impl.trait = ast_.AST.create_identifier(token, self.allocator);
                ast.impl.trait.?.set_symbol(anon_trait.symbol().?);
                ast.impl.impls_anon_trait = true;
            }

            return new_self;
        },

        // Create scope
        .method_decl => {
            if (ast.symbol() != null) {
                // Do not re-do symbol if already declared
                return null;
            } else if (ast.method_decl.init == null) {
                // Trait method decl
                var new_self = self;
                new_self.scope = Scope.init(self.scope, self.scope.uid_gen, self.allocator);
                new_self.scope.function_depth = new_self.scope.function_depth + 1;
                ast.method_decl._decl_type = create_method_type(ast, self.allocator);
                return new_self;
            } else {
                // Impl method decl
                const symbol = try create_method_symbol(ast, self.scope, self.errors, self.allocator);
                try self.register_symbol(ast, symbol);
                return null; // NOTE: DO NOT WALK CHILDREN!
            }
        },
    }

    return self;
}

fn register_symbol(self: Self, ast: *ast_.AST, symbol: *Symbol) walk_.Error!void {
    try self.scope.put_symbol(symbol, self.errors);
    ast.set_symbol(symbol);
}

pub fn postfix(self: Self, ast: *ast_.AST) walk_.Error!void {
    switch (ast.*) {
        else => {},

        .match => {
            try create_match_pattern_symbol(ast, self.scope, self.errors, self.allocator);
        },

        .trait => {
            for (ast.trait.method_decls.items) |method_decl| {
                method_decl.method_decl.c_type = create_method_type(method_decl, self.allocator);
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
    symbols: *std.ArrayList(*Symbol), // Mutable list that accumulates all the created symbols for a given scope, to be put() later.
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

            const symbol = Symbol.init(
                scope,
                pattern.pattern_symbol.name,
                ast_.AST.create_decl(pattern.token(), pattern, _type, init, allocator),
                pattern.pattern_symbol.kind,
                pattern.pattern_symbol.storage,
                allocator,
            );
            pattern.set_symbol(symbol);
            symbols.append(symbol) catch unreachable;
        },
        .tuple_value, .array_value => {
            for (pattern.children().items, 0..) |term, i| {
                const index = ast_.AST.create_int(pattern.token(), i, allocator);
                const new_type: *Type_AST = Type_AST.create_index(_type.token(), _type, index, allocator);
                var index_rhs = std.ArrayList(*ast_.AST).init(allocator);
                index_rhs.append(index) catch unreachable;
                const new_init: *ast_.AST = ast_.AST.create_index(init.?.token(), init.?, index_rhs, allocator);
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
        const new_scope = Scope.init(scope, scope.uid_gen, allocator);
        mapping.set_scope(new_scope);
        var symbols = std.ArrayList(*Symbol).init(allocator);
        defer symbols.deinit();
        const match_expr = match.expr();
        const match_expr_token = match_expr.token();
        const _type = Type_AST.create_type_of(match_expr_token, match_expr, allocator);
        try create_symbol(&symbols, mapping.lhs(), null, _type, match_expr, new_scope, errors, allocator);
        for (symbols.items) |symbol| {
            symbol.defined = true;
        }
        for (symbols.items) |symbol| {
            mapping.mapping.captures.append(symbol.decl.?) catch unreachable;
        }
        try new_scope.put_all_symbols(&symbols, errors);
        try walk_.walk_ast(mapping.rhs(), Self.new(new_scope, errors, allocator));
    }
}

pub fn create_function_symbol(
    ast: *ast_.AST,
    scope: *Scope,
    errors: *errs_.Errors,
    allocator: std.mem.Allocator,
) Error!*Symbol {
    // Calculate the domain type from the function paramter types
    const domain = extract_domain(ast.children().*, allocator);

    // Create the function type
    const _type = Type_AST.create_function(
        ast.fn_decl.ret_type.token(),
        domain,
        ast.fn_decl.ret_type,
        allocator,
    );
    ast.fn_decl._decl_type = _type;

    // Create the function scope
    var fn_scope = Scope.init(scope, scope.uid_gen, allocator);
    fn_scope.function_depth = scope.function_depth + 1;

    // Recurse parameters and init
    var symbol_walk = Self.new(fn_scope, errors, allocator);
    symbol_walk.is_param_scope = true;
    try walk_.walk_asts(ast.children(), symbol_walk);

    // Put the param symbols in the param symbols list
    for (ast.children().items) |param_binding| {
        const symbol = param_binding.binding.decls.items[0].decl.name.symbol().?;
        ast.param_symbols().?.append(symbol) catch unreachable;
    }

    const key_set = fn_scope.symbols.keys();
    for (0..key_set.len) |i| {
        const key = key_set[i];
        var symbol = fn_scope.symbols.get(key).?;
        symbol.defined = true;
        symbol.param = true;
    }

    // Choose name (maybe anon)
    var buf: []const u8 = undefined;
    if (ast.fn_decl.name) |name| {
        buf = name.token().data;
    } else {
        buf = next_anon_name("anon", allocator);
    }
    const retval = Symbol.init(
        fn_scope,
        buf,
        ast,
        .@"fn",
        .local,
        allocator,
    );
    fn_scope.inner_function = retval;

    try walk_.walk_ast(ast.fn_decl.init, symbol_walk);
    return retval;
}

pub fn create_test_symbol(
    ast: *ast_.AST,
    scope: *Scope,
    errors: *errs_.Errors,
    allocator: std.mem.Allocator,
) Error!*Symbol {
    // Create the function scope
    var fn_scope = Scope.init(scope, scope.uid_gen, allocator);
    fn_scope.function_depth = scope.function_depth + 1;

    // Choose name (maybe anon)
    const buf: []const u8 = next_anon_name("test", allocator);
    const retval = Symbol.init(
        fn_scope,
        buf,
        ast,
        .@"test",
        .local,
        allocator,
    );
    fn_scope.inner_function = retval;

    const symbol_walk = Self.new(fn_scope, errors, allocator);
    try walk_.walk_ast(ast.@"test".init, symbol_walk);
    return retval;
}

var num_anons: usize = 0;
pub fn next_anon_name(class: []const u8, allocator: std.mem.Allocator) []const u8 {
    defer num_anons += 1;
    var out = String.init(allocator);
    defer out.deinit();
    const writer = out.writer();
    writer.print("{s}${}", .{ class, num_anons }) catch unreachable;
    return (out.toOwned() catch unreachable).?;
}

pub fn extract_domain(params: std.ArrayList(*ast_.AST), allocator: std.mem.Allocator) *Type_AST {
    if (params.items.len == 0) {
        return prelude_.unit_type;
    } else if (params.items.len <= 1) {
        return Type_AST.create_annotation(
            params.items[0].token(),
            params.items[0].binding.pattern,
            params.items[0].binding.type,
            params.items[0].binding.init,
            allocator,
        );
    } else {
        std.debug.assert(params.items.len >= 2);
        var param_types = std.ArrayList(*Type_AST).init(allocator);
        return build_paramlist(params, &param_types, allocator);
    }
}

fn extract_domain_with_receiver(impl_type: *Type_AST, receiver: *ast_.AST, params: std.ArrayList(*ast_.AST), allocator: std.mem.Allocator) *Type_AST {
    const receiver_addr_type = create_receiver_addr(impl_type, receiver, allocator);
    receiver.receiver._type = receiver_addr_type;
    const _receiver_type = create_receiver_annot(receiver_addr_type, receiver, allocator);
    if (params.items.len == 0) {
        return _receiver_type;
    } else {
        var param_types = std.ArrayList(*Type_AST).init(allocator);
        param_types.append(_receiver_type) catch unreachable;
        return build_paramlist(params, &param_types, allocator);
    }
}

fn build_paramlist(params: std.ArrayList(*ast_.AST), param_types: *std.ArrayList(*Type_AST), allocator: std.mem.Allocator) *Type_AST {
    for (0..params.items.len) |i| {
        param_types.append(Type_AST.create_annotation(
            params.items[i].token(),
            params.items[i].binding.pattern,
            params.items[i].binding.type,
            params.items[i].binding.init,
            allocator,
        )) catch unreachable;
    }
    const retval = Type_AST.create_tuple_type(params.items[0].token(), param_types.*, allocator);
    return retval;
}

fn create_receiver_addr(impl_type: *Type_AST, receiver: *ast_.AST, allocator: std.mem.Allocator) *Type_AST {
    return switch (receiver.receiver.kind) {
        .value, .addr_of => Type_AST.create_addr_of(receiver.token(), impl_type, false, false, allocator),
        .mut_addr_of => Type_AST.create_addr_of(receiver.token(), impl_type, true, false, allocator),
    };
}

fn create_receiver_annot(receiver_addr: *Type_AST, receiver: *ast_.AST, allocator: std.mem.Allocator) *Type_AST {
    return Type_AST.create_annotation(
        receiver.token(),
        ast_.AST.create_identifier(receiver.token(), allocator),
        receiver_addr,
        null,
        allocator,
    );
}

/// Creates the comptime context symbol for `const` initializers
fn create_comptime_init(
    old_init: *ast_.AST,
    scope: *Scope,
    allocator: std.mem.Allocator,
) Error!*ast_.AST {
    const retval = ast_.AST.create_comptime(old_init.token(), old_init, allocator);
    retval.set_scope(scope);
    return retval;
}

/// Creates a symbol which represents the comptime function to be ran.
pub fn create_temp_comptime_symbol(
    ast: *ast_.AST,
    rhs_type_hint: ?*Type_AST,
    scope: *Scope,
    allocator: std.mem.Allocator,
) Error!*Symbol {
    // Create the function type. The rhs is a typeof node, since type expansion is done in a later time
    const lhs = prelude_.unit_type;
    const rhs = Type_AST.create_type_of(ast.token(), ast, allocator);
    const _type = Type_AST.create_function(ast.token(), lhs, rhs_type_hint orelse rhs, allocator);

    // Create the comptime scope
    // This is to prevent `comptime` expressions from using runtime variables
    var comptime_scope = Scope.init(scope, scope.uid_gen, allocator);
    comptime_scope.function_depth = scope.function_depth + 1;

    // Choose name
    var buf: []const u8 = undefined;
    buf = next_anon_name("comptime", allocator);

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
        ast.token().data,
        ast,
        .trait,
        .local,
        allocator,
    );
    retval.defined = true;
    return retval;
}

fn create_method_type(ast: *ast_.AST, allocator: std.mem.Allocator) *Type_AST {
    const receiver_base_type: *Type_AST = if (ast.method_decl.impl != null)
        ast.method_decl.impl.?.impl._type
    else
        Type_AST.create_anyptr_type(ast.token(), allocator);
    // Calculate the domain type from the function paramter types
    ast.method_decl.domain = if (ast.method_decl.receiver) |receiver|
        extract_domain_with_receiver(receiver_base_type, receiver, ast.children().*, allocator)
    else
        extract_domain(ast.children().*, allocator);

    // Create the function type
    const _type = Type_AST.create_function(
        ast.method_decl.ret_type.token(),
        ast.method_decl.domain.?,
        ast.method_decl.ret_type,
        allocator,
    );
    return _type;
}

fn create_method_symbol(
    ast: *ast_.AST,
    scope: *Scope,
    errors: *errs_.Errors,
    allocator: std.mem.Allocator,
) Error!*Symbol {
    const receiver_base_type: *Type_AST = ast.method_decl.impl.?.impl._type;

    // Create the function type
    ast.method_decl._decl_type = create_method_type(ast, allocator);

    // Create the function scope
    var fn_scope = Scope.init(scope, scope.uid_gen, allocator);
    fn_scope.function_depth = scope.function_depth + 1;

    // Recurse parameters and init
    const symbol_walk = Self.new(fn_scope, errors, allocator);
    try walk_.walk_asts(ast.children(), symbol_walk);
    // try walk_.walk_ast(ast.method_decl.ret_type, symbol_walk); // TODO: walk types

    if (ast.method_decl.receiver != null) {
        // addr-of receiver, prepend receiver to parameters as normal
        const recv_type = create_receiver_addr(receiver_base_type, ast.method_decl.receiver.?, allocator);
        recv_type.addr_of.anytptr = true;
        // value receiver, prepend a void* self_ptr parameter
        const receiver_symbol = Symbol.init(
            fn_scope,
            if (ast.method_decl.receiver.?.receiver.kind == .value) "$self_ptr" else "self",
            ast.method_decl.receiver,
            .let,
            .local,
            allocator,
        );
        try fn_scope.put_symbol(receiver_symbol, errors);
        ast.param_symbols().?.append(receiver_symbol) catch unreachable;

        if (ast.method_decl.receiver.?.receiver.kind == .value) {
            const self_type = recv_type.child();
            const self_init = ast_.AST.create_dereference(ast.token(), ast_.AST.create_identifier(Token.init_simple("$self_ptr"), allocator), allocator);
            const receiver_span = ast.method_decl.receiver.?.token().span;
            const self_decl = ast_.AST.create_binding(
                ast.token(),
                ast_.AST.create_pattern_symbol(Token.init("self", .identifier, receiver_span.filename, receiver_span.line_text, receiver_span.line_number, receiver_span.col), .let, .local, "self", allocator),
                self_type,
                self_init,
                allocator,
            );
            if (ast.method_decl.init.?.* != .unit_value) {
                const method_block_statements = ast.method_decl.init.?.children();
                method_block_statements.insert(0, self_decl) catch unreachable;
            } else {
                // Technically, init COULD be `{ }`, and it would cause a not-used error later on, but we need to handle this properly here before then
                var statements = std.ArrayList(*ast_.AST).init(allocator);
                statements.append(self_decl) catch unreachable;
                ast.method_decl.init = ast_.AST.create_block(ast.method_decl.init.?.token(), statements, null, allocator);
            }
        }
    }

    // Put the param symbols in the param symbols list
    for (ast.children().items) |param_binding| {
        const symbol = param_binding.binding.decls.items[0].decl.name.symbol().?;
        ast.param_symbols().?.append(symbol) catch unreachable;
    }

    const key_set = fn_scope.symbols.keys();
    for (0..key_set.len) |i| {
        const key = key_set[i];
        var symbol = fn_scope.symbols.get(key).?;
        symbol.defined = true;
        symbol.param = true;
    }

    const retval = Symbol.init(
        fn_scope,
        ast.method_decl.name.token().data,
        ast,
        .@"fn",
        .local,
        allocator,
    );
    fn_scope.inner_function = retval;

    try walk_.walk_ast(ast.method_decl.init, symbol_walk);
    return retval;
}

fn create_template_symbol(
    ast: *ast_.AST,
    scope: *Scope,
    allocator: std.mem.Allocator,
) Error!*Symbol {
    var buf: []const u8 = undefined;
    if (ast.template.decl.fn_decl.name) |name| {
        buf = name.token().data;
    } else {
        buf = next_anon_name("", allocator);
    }
    const retval = Symbol.init(
        scope,
        buf,
        ast.template.decl.fn_decl.name.?.token().span,
        prelude_.unit_type,
        prelude_.unit_value,
        ast,
        .template,
        allocator,
    );
    retval.defined = true;
    return retval;
}
