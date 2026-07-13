//! This file represents a struct for an AST walk, and is used to decorate identifier ASTs with symbols they refer to.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const core_ = @import("../hierarchy/core.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const generic_apply_ = @import("generic_apply.zig");
const Scope = @import("../symbol/scope.zig");
const Span = @import("../util/span.zig");
const Symbol = @import("../symbol/symbol.zig");
const Symbol_Tree = @import("symbol-tree.zig");
const Token = @import("../lexer/token.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Type_Decorate = @import("../ast/type_decorate.zig");
const union_fields_ = @import("../util/union_fields.zig");
const walk_ = @import("../ast/walker.zig");
const unification_ = @import("../types/unification.zig");

ctx: *Compiler_Context,

const Self = @This();

pub fn new(ctx: *Compiler_Context) Self {
    return Self{
        .ctx = ctx,
    };
}

pub fn symbol(ast: anytype, ctx: *Compiler_Context) walk_.Error!*Symbol {
    const Ast_Type = @TypeOf(ast);
    if (Ast_Type == *ast_.AST) {
        return ast_symbol(ast, ctx);
    } else if (Ast_Type == *Type_AST) {
        return type_symbol(ast, ctx);
    } else {
        std.debug.panic("cannot call symbol() with type {}", .{Ast_Type});
    }
}

fn ast_symbol(ast: *ast_.AST, ctx: *Compiler_Context) walk_.Error!*Symbol {
    const self = Self.new(ctx);
    try walk_.walk_ast(ast, self);
    return ast.symbol().?;
}

fn type_symbol(_type: *Type_AST, ctx: *Compiler_Context) walk_.Error!*Symbol {
    const self = Self.new(ctx);
    try walk_.walk_type(_type, self);
    return _type.symbol().?;
}

pub fn prefix(self: Self, ast: *ast_.AST) walk_.Error!?Self {
    return self.decorate_prefix(ast);
}

pub fn postfix(self: Self, ast: *ast_.AST) walk_.Error!void {
    return self.decorate_postfix(ast);
}

pub fn prefix_type(self: Self, _type: *Type_AST) walk_.Error!?Self {
    return self.decorate_prefix_type(_type);
}

pub fn postfix_type(self: Self, _type: *Type_AST) walk_.Error!void {
    return self.decorate_postfix_type(_type);
}

fn decorate_prefix(self: Self, ast: *ast_.AST) walk_.Error!?Self {
    switch (ast.*) {
        else => return self,

        .assign => {
            // Rewrite lvalue brackets to `Index_Mut`, propagating through selects so
            // `x[i].a = v` mutates the element not a copy
            try self.rewrite_lvalue(ast.lhs());
            return self;
        },

        .addr_of => {
            // `&mut x[i]` rewrites the bracket to its `index_mut(...)^` lval, keeping the `addr_of`
            if (ast.addr_of.mut) {
                try self.rewrite_lvalue(ast.expr());
            }
            return self;
        },

        .identifier => {
            if (ast.symbol() != null) {
                return null;
            }

            const res = ast.scope().?.lookup(ast.token().data, .{ .allow_modules = false });
            switch (res) {
                // Found the symbol, decorate the identifier AST with it
                .found => ast.set_symbol(res.found),

                // Couldn't find the symbol
                .not_found => {
                    self.ctx.errors.add_error(errs_.Error{ .undeclared_identifier = .{ .identifier = ast.token(), .expected = null } });
                    return error.CompileError;
                },

                // Found the symbol, but must cross a comptime-boundary to access it, and it is not const
                .found_but_rt => {
                    self.ctx.errors.add_error(errs_.Error{ .comptime_access_runtime = .{ .identifier = ast.token() } });
                    return error.CompileError;
                },

                // Found the symbol, but must cross an inner-function boundary to access it, and it is not const
                .found_but_fn => {
                    self.ctx.errors.add_error(errs_.Error{ .inner_fn_access_runtime = .{ .identifier = ast.token() } });
                    return error.CompileError;
                },
            }

            return self;
        },
        .impl => {
            // Copy defaulted trait methods
            if (ast.impl.trait == null) return self;
            const trait_symbol = try symbol(ast.impl.trait.?, self.ctx);
            const trait_ast = trait_symbol.decl.?;
            if (trait_ast.* != .trait) return self; // error, catch it later
            for (trait_ast.trait.method_decls.items) |method_decl| {
                if (method_decl.method_decl.init == null) continue; // not defaulted, ignore
                if (impl_provides_method(ast, method_decl.method_decl.name.token().data)) continue; // overridden

                var subst = unification_.Substitutions.init(self.ctx.allocator());
                defer subst.deinit();
                try subst.put_type("Self", ast.impl._type);
                const new_method = method_decl.clone(&subst, self.ctx.allocator());
                new_method.method_decl.impl = ast; // bind the template to this impl
                ast.impl.method_defs.append(new_method) catch unreachable;
                try self.scope_subtree(new_method, ast.scope().?);
            }

            return self;
        },
    }
}

fn decorate_prefix_type(self: Self, _type: *Type_AST) walk_.Error!?Self {
    switch (_type.*) {
        else => return self,

        .identifier => {
            if (_type.symbol()) |_| {
                // Do not re-decorate.
                // Symbols are injected into ident types for lexical generic scoping
                // Keep those symbols the way they are, even if they're not "visible" from this scope!
                return self;
            }
            const res = _type.scope().?.lookup(_type.token().data, .{});
            switch (res) {
                // Found the symbol, decorate the identifier AST with it
                .found => _type.set_symbol(res.found),

                // Couldn't find the symbol
                .not_found => {
                    self.ctx.errors.add_error(errs_.Error{ .undeclared_identifier = .{ .identifier = _type.token(), .expected = null } });
                    return error.CompileError;
                },

                // Found the symbol, but must cross a comptime-boundary to access it, and it is not const
                .found_but_rt => {
                    self.ctx.errors.add_error(errs_.Error{ .comptime_access_runtime = .{ .identifier = _type.token() } });
                    return error.CompileError;
                },

                // Found the symbol, but must cross an inner-function boundary to access it, and it is not const
                .found_but_fn => {
                    self.ctx.errors.add_error(errs_.Error{ .inner_fn_access_runtime = .{ .identifier = _type.token() } });
                    return error.CompileError;
                },
            }

            return self;
        },
    }
}

/// A whole-arg type appeared where a value was required, a const argument or an index subscript,
/// and could not be reinterpreted as one
fn value_expected(self: Self, span: Span) walk_.Error {
    self.ctx.errors.add_error(errs_.Error{ .basic = .{ .msg = "expected a value, got a type", .span = span } });
    return error.CompileError;
}

fn decorate_postfix(self: Self, ast: *ast_.AST) walk_.Error!void {
    switch (ast.*) {
        else => {},

        .access => {
            if (ast.symbol() != null) {
                return;
            }
            if (ast.lhs().refers_to_type() or ast.lhs().refers_to_trait()) {
                const scope = ast.scope();
                ast.* = ast_.AST.create_type_access(ast.token(), Type_AST.from_ast(ast.lhs(), self.ctx.allocator()), ast.rhs(), self.ctx.allocator()).*;
                ast.set_scope(scope);
                ast.set_symbol(try self.resolve_type_access_ast(ast));
            } else {
                ast.set_symbol(try self.resolve_access_ast(ast));
            }
        },

        .type_access => {
            if (ast.symbol() != null) {
                return;
            }
            ast.set_symbol(try self.resolve_type_access_ast(ast));
        },

        .call => {
            if (ast.lhs().* == .enum_value) {
                // Enum value
                ast.lhs().enum_value.init = ast.children().items[0];
                ast.* = ast.lhs().*;
            } else if (ast.lhs().refers_to_type()) {
                // Struct value construction
                const struct_type = Type_AST.from_ast(ast.lhs(), self.ctx.allocator());
                const struct_value = ast_.AST.create_struct_value(
                    ast.lhs().token(),
                    struct_type,
                    ast.children().*,
                    self.ctx.allocator(),
                );
                ast.* = struct_value.*;
            }
        },

        .bracket => {
            const child = ast.lhs();

            // Rewrite bracket to generic apply if its a generic apply
            if (child.* == .identifier or child.* == .access) {
                if (child.symbol()) |sym| if (sym.decl) |decl| {
                    if (decl.num_generic_params() > 0) {
                        var generic_args = std.array_list.Managed(ast_.GenericArg).init(self.ctx.allocator());
                        const params = decl.generic_params().items;
                        for (ast.bracket._args.items, 0..) |arg, i| {
                            const wants_const = i < params.len and params[i].* == .const_param_decl;
                            const wants_ability = i < params.len and params[i].* == .ability_param_decl;
                            switch (arg) {
                                .const_arg => |v| if (wants_const) {
                                    try generic_args.append(.{ .const_arg = v });
                                } else if (wants_ability) {
                                    self.ctx.errors.add_error(errs_.Error{ .basic = .{
                                        .msg = "expected an ability argument, got a value",
                                        .span = v.token().span,
                                    } });
                                    return error.CompileError;
                                } else {
                                    self.ctx.errors.add_error(errs_.Error{ .basic = .{
                                        .msg = "expected a type argument, got a value",
                                        .span = v.token().span,
                                    } });
                                    return error.CompileError;
                                },

                                .type_arg => |ty| if (wants_const) {
                                    try generic_args.append(.{ .const_arg = ty.to_value_expr(self.ctx.allocator()) orelse return self.value_expected(ty.token().span) });
                                } else if (wants_ability) {
                                    const resolved = (try ty.resolve_ability_reference(self.ctx)) orelse {
                                        self.ctx.errors.add_error(errs_.Error{ .expected_ability = .{
                                            .span = ty.token().span,
                                            .got = ty,
                                        } });
                                        return error.CompileError;
                                    };
                                    ty.* = resolved.*;
                                    try generic_args.append(.{ .type_arg = ty });
                                } else {
                                    try generic_args.append(.{ .type_arg = ty });
                                },
                            }
                        }
                        ast.* = ast_.AST.create_generic_apply(ast.token(), child, generic_args, self.ctx.allocator()).*;
                        try generic_apply_.instantiate(ast, self.ctx);
                        return;
                    }
                };
            }

            // Rewrite bracket to Index FQS call if its an index
            const idx = switch (ast.bracket._args.items[0]) {
                .const_arg => |v| v,
                .type_arg => |ty| ty.to_value_expr(self.ctx.allocator()) orelse return self.value_expected(ty.token().span),
            };
            const index_call = try ast_.AST.create_index_call(ast.token(), child, idx, false, self.ctx.allocator());
            try self.scope_subtree(index_call, ast.scope().?);
            ast.* = index_call.*;
        },

        .select => {
            var child = ast.lhs();
            if (child.* == .identifier and child.symbol() != null) {
                var s = child.symbol().?;
                // Hop type alias chains
                while (s.decl != null and s.decl.?.* == .type_alias) {
                    const td = s.init_typedef() orelse break;
                    try walk_.walk_type(td, Self.new(self.ctx));
                    if (td.* != .identifier or td.symbol() == null) break;
                    s = td.symbol().?;
                }
                if (s.kind == .ability) child.set_symbol(s);
            }
            if (child.* == .identifier and child.symbol() != null and child.symbol().?.is_type()) {
                const enum_value = ast_.AST.create_enum_value(ast.rhs().token(), self.ctx.allocator());
                enum_value.enum_value.base = Type_AST.create_type_identifier(child.token(), self.ctx.allocator());
                enum_value.enum_value.base.?.set_symbol(child.symbol());
                ast.* = enum_value.*;
                return;
            }
        },

        .write => {
            const scope = ast.scope().?;
            const format_all_call = try self.create_format_all_call(ast, ast.write.writer);
            ast.* = format_all_call.*;
            try self.scope_subtree(ast, scope);
        },

        .print => {
            const scope = ast.scope().?;
            const ability_val_symbol = try ast.scope().?.ability_lookup(core_.writing_ability, self.ctx) orelse {
                self.ctx.errors.add_error(errs_.Error{ .missing_ability = .{
                    .span = ast.token().span,
                    .ability = Type_AST.create_type_identifier(core_.writing_ability.token(), self.ctx.allocator()),
                } });
                return error.CompileError;
            };

            // Append writer to the call args
            var writer_token = ast.token();
            writer_token.data = ability_val_symbol.name;
            const writing_ability = ast_.AST.create_identifier(writer_token, self.ctx.allocator());
            writing_ability.set_symbol(ability_val_symbol);

            const format_all_call = try self.create_format_all_call(ast, writing_ability);
            ast.* = format_all_call.*;
            try self.scope_subtree(ast, scope);
        },

        .format_args => {
            const scope = ast.scope().?;
            const format_args_slice = try self.create_format_args_slice(ast);
            ast.* = format_args_slice.*;
            try self.scope_subtree(ast, scope);
        },

        .decl => {
            if (ast.decl.init != null and ast.decl.init.?.* == .access and ast.decl.init.?.symbol().?.kind == .type) {
                const init_symbol = ast.decl.init.?.symbol().?;
                ast.* = ast_.AST.create_type_alias(
                    ast.token(),
                    ast.decl.name,
                    Type_AST.from_ast(ast.decl.init.?, self.ctx.allocator()),
                    std.array_list.Managed(*ast_.AST).init(self.ctx.allocator()),
                    self.ctx.allocator(),
                ).*;
                ast.type_alias.init.?.set_symbol(init_symbol);
            }
        },
        .generic_apply => {
            try generic_apply_.instantiate(ast, self.ctx);
        },
        .trait => try ast.scope().?.traits.put(ast, void{}),
        .enum_decl => try ast.scope().?.enums.put(ast, void{}),
        .@"test" => try ast.scope().?.tests.append(ast),
    }
}

fn decorate_postfix_type(self: Self, ast: *Type_AST) walk_.Error!void {
    switch (ast.*) {
        .access => {
            if (ast.symbol() != null) {
                return;
            }
            ast.set_symbol(try self.resolve_access_type(ast));
        },

        .generic_apply => {
            // The parser can only detect const_args lexically (integer/float/bool literals).
            // Identifier const param refs (`n` in `Buf[n]`) arrive as type_arg when parsed in a type position.
            // Reclassify them here, after symbols have been set.
            for (ast.generic_apply.args.items) |*arg| {
                switch (arg.*) {
                    .type_arg => |ty| {
                        if (ty.* == .identifier) {
                            const sym = ty.symbol() orelse continue;
                            const decl = sym.decl orelse continue;
                            if (decl.* == .const_param_decl) {
                                const id = ast_.AST.create_identifier(ty.token(), self.ctx.allocator());
                                id.set_symbol(sym);
                                arg.* = .{ .const_arg = id };
                            }
                        }
                    },
                    .const_arg => {},
                }
            }
            try generic_apply_.instantiate(ast, self.ctx);
        },
        else => {},
    }
}

fn resolve_access_ast(self: Self, ast: *ast_.AST) walk_.Error!*Symbol {
    std.debug.assert(ast.* == .access);

    const stripped_lhs = if (ast.lhs().* == .addr_of)
        ast.lhs().expr()
    else
        ast.lhs();

    const stripped_lhs_type = Type_AST.from_ast(stripped_lhs, self.ctx.allocator());

    // A structural type lhs has no symbol, resolve it as a type access
    if (!union_fields_.has_struct_field(stripped_lhs.*, "_symbol")) {
        return self.resolve_lhs_type_access(stripped_lhs_type, ast.rhs().token(), ast.scope());
    }

    const sym = stripped_lhs.symbol().?;
    return self.resolve_access_symbol(sym, ast.rhs().token(), ast.scope(), stripped_lhs_type);
}

fn resolve_type_access_ast(self: Self, ast: *ast_.AST) walk_.Error!*Symbol {
    return self.resolve_lhs_type_access(ast.type_access._lhs_type, ast.rhs().token(), ast.scope());
}

fn resolve_access_type(self: Self, ast: *Type_AST) walk_.Error!*Symbol {
    return self.resolve_lhs_type_access(ast.lhs(), ast.rhs().token(), ast.scope());
}

fn resolve_lhs_type_access(self: Self, lhs: *Type_AST, rhs: Token, scope: ?*Scope) walk_.Error!*Symbol {
    // Deref a reference receiver for member lookup, but not a multiptr, which is its own indexable type
    const stripped_lhs = if (lhs.* == .addr_of and !lhs.addr_of.multiptr)
        lhs.child()
    else
        lhs;

    try walk_.walk_type(stripped_lhs, Type_Decorate.new(self.ctx));
    if (stripped_lhs.* == .generic_apply) {
        try generic_apply_.instantiate(stripped_lhs, self.ctx);
    }
    if (stripped_lhs.* == .as_trait) {
        // Auto-deref the receiver to its base indexable, so a `&mut [n]T` receiver resolves
        // against `impl ... for [n]T` instead of falling to the abstract trait method
        var base = stripped_lhs.lhs();
        while (true) {
            const exp = base.expand_identifier();
            if (exp.* == .addr_of and !exp.addr_of.multiptr) base = exp.child() else break;
        }
        stripped_lhs.as_trait._lhs = base;
        for (stripped_lhs.as_trait.constraints.items) |constraint| {
            const res = try Scope.impl_trait_lookup(stripped_lhs.lhs(), constraint.symbol().?, self.ctx);
            if (res.impl_ast) |impl_ast| {
                const decl = Scope.search_impl(impl_ast, rhs.data) orelse continue;
                // Decorate the original member first so `Self::Output` etc resolve before any remap
                try walk_.walk_ast(decl, self);
                if (res.subst) |*s| {
                    // Generic impl came back un-substituted, remap the member's signature through
                    // the subst so its types refer to the querying impl's params
                    if (Scope.subst_renames_params(impl_ast, s)) {
                        return Scope.remap_impl_member(decl, s, self.ctx).symbol().?;
                    }
                }
                return decl.symbol().?;
            } else {
                const decl = try Scope.lookup_member_in_trait(constraint.symbol().?.decl.?, stripped_lhs.lhs(), rhs.data, self.ctx);
                return decl.?.symbol().?;
            }
        }
    }
    if (stripped_lhs.* != .access and stripped_lhs.* != .identifier and stripped_lhs.* != .generic_apply) {
        return try self.resolve_access_const(stripped_lhs, rhs, scope.?);
    }

    const sym = stripped_lhs.symbol().?;
    return self.resolve_access_symbol(sym, rhs, scope, stripped_lhs);
}

fn resolve_access_symbol(self: Self, sym: *Symbol, rhs: Token, scope: ?*Scope, stripped_lhs_type: *Type_AST) walk_.Error!*Symbol {
    switch (sym.kind) {
        .module => return try self.resolve_access_module(sym, rhs),

        .import => {
            const module_symbol = sym.kind.import.real_symbol.?;
            return try self.resolve_access_module(module_symbol, rhs);
        },

        .import_inner => {
            const new_symbol =
                if (sym.decl.?.* == .type_alias)
                    try self.resolve_access_type(sym.init_typedef().?)
                else
                    try self.resolve_access_ast(sym.init_value().?);
            return self.resolve_access_symbol(new_symbol, rhs, scope, stripped_lhs_type);
        },

        .type => return try self.resolve_access_const(stripped_lhs_type, rhs, scope.?),

        .trait => {
            // bind abstract method_decl, typecheck resolves concrete impl at call site.
            const trait_decl = sym.decl.?;
            for (trait_decl.trait.method_decls.items) |method_decl| {
                if (std.mem.eql(u8, method_decl.method_decl.name.token().data, rhs.data)) {
                    return method_decl.symbol().?;
                }
            }
            self.ctx.errors.add_error(errs_.Error{
                .member_not_in_module = .{
                    .span = rhs.span,
                    .identifier = rhs.data,
                    .name = "trait",
                    .module_name = sym.name,
                },
            });
            return error.CompileError;
        },

        else => {
            self.ctx.errors.add_error(errs_.Error{
                .member_not_in_module = .{
                    .span = rhs.span,
                    .identifier = rhs.data,
                    .name = "symbol",
                    .module_name = sym.name,
                },
            });
            return error.CompileError;
        },
    }
}

/// Resolves a symbol access from a module
fn resolve_access_module(self: Self, module_symbol: *Symbol, rhs: Token) walk_.Error!*Symbol {
    std.debug.assert(module_symbol.kind == .module);

    const module_lookup_res = module_symbol.init_value().?.scope().?.lookup(
        rhs.data,
        .{},
    );
    const rhs_decl = switch (module_lookup_res) {
        .found => module_lookup_res.found.decl.?,
        else => {
            self.ctx.errors.add_error(errs_.Error{
                .member_not_in_module = .{
                    .span = rhs.span,
                    .identifier = rhs.data,
                    .name = "module",
                    .module_name = module_symbol.name,
                },
            });
            return error.CompileError;
        },
    };
    return extract_symbol_from_decl(rhs_decl);
}

/// Resolves a symbol access on a constant symbol, likely a trait lookup
fn resolve_access_const(self: Self, lhs: *Type_AST, rhs_token: Token, scope: *Scope) walk_.Error!*Symbol {
    var matches = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(self.ctx.allocator());
    defer matches.deinit();
    scope.lookup_impl_member(lhs, rhs_token.data, &matches, true, self.ctx) catch return error.CompileError;
    if (matches.keys().len == 0) {
        self.ctx.errors.add_error(errs_.Error{
            .type_not_impl_method = .{
                .span = rhs_token.span,
                .method_name = rhs_token.data,
                ._type = lhs.strip_as_trait(),
                .candidates = null,
            },
        });
        return error.CompileError;
    } else if (matches.keys().len > 1) {
        self.ctx.errors.add_error(errs_.Error{
            .basic = .{
                .span = rhs_token.span,
                .msg = "AMBIGUOUS [TODO: make better]",
            },
        });
        return error.CompileError;
    } else {
        return extract_symbol_from_decl(matches.keys()[0]);
    }
}

/// Extracts the symbol that a decl-like AST decls
fn extract_symbol_from_decl(decl: *ast_.AST) *Symbol {
    if (decl.* == .decl) {
        return decl.decl.name.symbol().?;
    } else if (decl.* == .method_decl or decl.* == .fn_decl or decl.* == .trait or decl.* == .struct_decl or decl.* == .enum_decl or decl.* == .type_alias or decl.* == .ability_decl or decl.* == .type_param_decl) {
        return decl.symbol().?;
    } else if (decl.* == .binding) {
        return decl.binding.pattern.symbol().?;
    } else if (decl.* == .pattern_symbol) {
        return decl.symbol().?;
    } else {
        std.debug.panic("compiler error: unsupported access symbol resolution for decl-like AST: {s}", .{@tagName(decl.*)});
    }
}

/// Assigns scopes to a freshly-built subtree (created mid-Decorate) by replaying the
/// Symbol_Tree walk over it. Needed so the later Type_Decorate pass and trait-member
/// lookup (which read `node.scope()`) work on the inserted nodes.
fn scope_subtree(self: *const Self, subtree: *ast_.AST, scope: *Scope) !void {
    const st = Symbol_Tree.new(scope, &self.ctx.errors, self.ctx.allocator());
    try walk_.walk_ast(subtree, st);
}

/// Whether the impl already defines a method with the given name, so a trait default is not copied
fn impl_provides_method(impl: *ast_.AST, name: []const u8) bool {
    for (impl.impl.method_defs.items) |method_def| {
        if (std.mem.eql(u8, method_def.method_decl.name.token().data, name)) return true;
    }
    return false;
}

/// Recursively desugars an lvalue bracket chain into nested `index_mut` addresses
fn rewrite_lvalue_bracket(self: *const Self, bracket: *ast_.AST) !*ast_.AST {
    const idx = switch (bracket.bracket._args.items[0]) {
        .const_arg => |v| v,
        .type_arg => |ty| ty.to_value_expr(self.ctx.allocator()) orelse return self.value_expected(ty.token().span),
    };
    const inner = bracket.lhs();
    // Inner bracket yields a `index_mut(...)^`, the receiver for the next level
    const receiver = if (inner.* == .bracket)
        try self.rewrite_lvalue_bracket(inner)
    else
        inner;
    return ast_.AST.create_index_call(bracket.token(), receiver, idx, true, self.ctx.allocator());
}

/// Rewrites brackets in an lvalue into `index_mut(...)^`
fn rewrite_lvalue(self: *const Self, node: *ast_.AST) walk_.Error!void {
    switch (node.*) {
        .select => try self.rewrite_lvalue(node.lhs()),
        // Destructuring pattern, each element is its own lvalue
        .array_value, .tuple_value => for (node.children().items) |child| try self.rewrite_lvalue(child),
        .bracket => {
            const place = try self.rewrite_lvalue_bracket(node);
            try self.scope_subtree(place, node.scope().?);
            node.* = place.*;
        },
        else => {},
    }
}

fn create_format_all_call(self: *const Self, ast: *ast_.AST, writer: *ast_.AST) !*ast_.AST {
    var core_token = ast.token();
    core_token.data = "core";
    const core_ident = ast_.AST.create_identifier(core_token, self.ctx.allocator());
    var fa_token = ast.token();
    fa_token.data = "format_all";
    const format_all = ast_.AST.create_access(
        ast.token(),
        core_ident,
        ast_.AST.create_field(fa_token, self.ctx.allocator()),
        self.ctx.allocator(),
    );

    format_all.set_symbol(self.ctx.get_core_symbol("format_all"));
    var args = std.array_list.Managed(*ast_.AST).init(self.ctx.allocator());
    try args.append(writer);

    const args_slice = try self.create_format_args_slice(ast);
    try args.append(args_slice);

    const format_all_call = ast_.AST.create_call(ast.token(), format_all, args, self.ctx.allocator());
    return format_all_call;
}

fn create_format_args_slice(self: *const Self, ast: *ast_.AST) !*ast_.AST {
    var array_terms = std.array_list.Managed(*ast_.AST).init(self.ctx.allocator());
    var format_ident_token = ast.token();
    format_ident_token.data = "Format";
    var format_ident = Type_AST.create_type_identifier(format_ident_token, self.ctx.allocator());
    format_ident.set_symbol(self.ctx.get_core_symbol("Format"));
    const dyn_type = Type_AST.create_dyn_type(ast.token(), format_ident, false, self.ctx.allocator());
    for (ast.children().items) |child| {
        const dyn_value = ast_.AST.create_dyn_value(
            child.token(),
            dyn_type,
            child,
            ast.scope().?,
            false,
            self.ctx.allocator(),
        );
        try array_terms.append(dyn_value);
    }
    const args_array = ast_.AST.create_array_value(ast.token(), array_terms, self.ctx.allocator());
    return ast_.AST.create_addr_of(ast.token(), args_array, false, false, self.ctx.allocator());
}
