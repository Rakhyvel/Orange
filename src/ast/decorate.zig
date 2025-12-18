//! This file represents a struct for an AST walk, and is used to decorate identifier ASTs with symbols they refer to.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const core_ = @import("../hierarchy/core.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const generic_apply_ = @import("generic_apply.zig");
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const Symbol_Tree = @import("symbol-tree.zig");
const Token = @import("../lexer/token.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Type_Decorate = @import("../ast/type_decorate.zig");
const walk_ = @import("../ast/walker.zig");

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

        .identifier => {
            if (ast.symbol() != null) {
                return self;
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

fn decorate_postfix(self: Self, ast: *ast_.AST) walk_.Error!void {
    switch (ast.*) {
        else => {},

        .access => {
            if (ast.lhs().refers_to_type()) {
                const scope = ast.scope();
                ast.* = ast_.AST.create_type_access(ast.token(), Type_AST.from_ast(ast.lhs(), self.ctx.allocator()), ast.rhs(), self.ctx.allocator()).*;
                ast.set_scope(scope);
                ast.set_symbol(try self.resolve_type_access_ast(ast));
            } else {
                ast.set_symbol(try self.resolve_access_ast(ast));
            }
        },

        .type_access => ast.set_symbol(try self.resolve_type_access_ast(ast)),

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

        .index => {
            var child = ast.lhs();
            if (child.* != .identifier and child.* != .access) return;

            const sym = child.symbol() orelse return;
            const decl = sym.decl orelse return;

            // child points to a generic function
            if (decl.num_generic_params() > 0) {
                var types = std.array_list.Managed(*Type_AST).init(self.ctx.allocator());
                for (ast.children().items) |arg| {
                    try types.append(Type_AST.from_ast(arg, self.ctx.allocator()));
                }
                ast.* = ast_.AST.create_generic_apply(ast.token(), child, types, self.ctx.allocator()).*;
                try generic_apply_.instantiate(ast, self.ctx);
            }
        },

        .select => {
            var child = ast.lhs();
            if (child.* == .identifier and child.symbol() != null and child.symbol().?.is_type()) {
                const enum_value = ast_.AST.create_enum_value(ast.rhs().token(), self.ctx.allocator());
                enum_value.enum_value.base = Type_AST.create_type_identifier(child.token(), self.ctx.allocator());
                enum_value.enum_value.base.?.set_symbol(child.symbol());
                ast.* = enum_value.*;
                return;
            }

            // child points to a generic function
            if (child.* != .identifier and child.* != .access) return;

            const sym = child.symbol() orelse return;
            const decl = sym.decl orelse return;

            if (decl.* == .context_decl) {
                const fn_ctx = decl.decl_typedef().?;
                const context_val_symbol = try child.scope().?.parent.?.context_lookup(fn_ctx, self.ctx) orelse {
                    self.ctx.errors.add_error(errs_.Error{ .missing_context = .{
                        .span = ast.token().span,
                        .context = Type_AST.create_type_identifier(decl.token(), self.ctx.allocator()),
                    } });
                    return error.CompileError;
                };
                var token = ast.token();
                token.data = context_val_symbol.name;
                const context_val_ident = ast_.AST.create_identifier(token, self.ctx.allocator());
                context_val_ident.set_symbol(context_val_symbol);
                ast.set_lhs(context_val_ident);
            }
        },

        .write => {
            const format_all_call = try self.create_format_all_call(ast, ast.write.writer);
            ast.* = format_all_call.*;
        },

        .print => {
            const context_val_symbol = try ast.scope().?.context_lookup(core_.io_context, self.ctx) orelse {
                self.ctx.errors.add_error(errs_.Error{ .missing_context = .{
                    .span = ast.token().span,
                    .context = Type_AST.create_type_identifier(core_.io_context.token(), self.ctx.allocator()),
                } });
                return error.CompileError;
            };

            // Append writer to the call args
            var writer_token = ast.token();
            writer_token.data = context_val_symbol.name;
            const io_context = ast_.AST.create_identifier(writer_token, self.ctx.allocator());
            io_context.set_symbol(context_val_symbol);
            var writer_field_token = ast.token();
            writer_field_token.data = "writer";
            const writer_field = ast_.AST.create_field(writer_field_token, self.ctx.allocator());
            const writer = ast_.AST.create_select(writer_token, io_context, writer_field, self.ctx.allocator());

            const format_all_call = try self.create_format_all_call(ast, writer);

            ast.* = format_all_call.*;
        },

        .format_args => {
            const format_args_slice = try self.create_format_args_slice(ast);
            ast.* = format_args_slice.*;
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
            ast.set_symbol(try self.resolve_access_type(ast));
        },

        .generic_apply => {
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

    const sym = stripped_lhs.symbol().?;
    const stripped_lhs_type = Type_AST.from_ast(stripped_lhs, self.ctx.allocator());
    return self.resolve_access_symbol(sym, ast.rhs().token(), ast.scope(), stripped_lhs_type);
}

fn resolve_type_access_ast(self: Self, ast: *ast_.AST) walk_.Error!*Symbol {
    return self.resolve_lhs_type_access(ast.type_access._lhs_type, ast.rhs().token(), ast.scope());
}

fn resolve_access_type(self: Self, ast: *Type_AST) walk_.Error!*Symbol {
    return self.resolve_lhs_type_access(ast.lhs(), ast.rhs().token(), ast.scope());
}

fn resolve_lhs_type_access(self: Self, lhs: *Type_AST, rhs: Token, scope: ?*Scope) walk_.Error!*Symbol {
    const stripped_lhs = if (lhs.* == .addr_of)
        lhs.child()
    else
        lhs;

    if (stripped_lhs.* == .type_of) {
        try walk_.walk_type(stripped_lhs, Type_Decorate.new(self.ctx));
    } else if (stripped_lhs.* == .generic_apply) {
        try generic_apply_.instantiate(stripped_lhs, self.ctx);
    }
    if (stripped_lhs.* == .as_trait) {
        const res = try scope.?.impl_trait_lookup(stripped_lhs.lhs(), stripped_lhs.rhs().symbol().?, self.ctx);
        if (res.ast != null) {
            const decl = Scope.search_impl(res.ast.?, rhs.data);
            try walk_.walk_ast(decl, self);
            return decl.?.symbol().?;
        } else {
            const decl = try scope.?.lookup_member_in_trait(stripped_lhs.rhs().symbol().?.decl.?, stripped_lhs.lhs(), rhs.data, self.ctx);
            return decl.?.symbol().?;
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
    const rhs_decl = scope.lookup_impl_member(lhs, rhs_token.data, self.ctx) catch return error.CompileError;
    if (rhs_decl == null) {
        self.ctx.errors.add_error(errs_.Error{
            .type_not_impl_method = .{
                .span = rhs_token.span,
                .method_name = rhs_token.data,
                ._type = lhs,
            },
        });
        return error.CompileError;
    } else {
        return extract_symbol_from_decl(rhs_decl.?);
    }
}

/// Extracts the symbol that a decl-like AST decls
fn extract_symbol_from_decl(decl: *ast_.AST) *Symbol {
    if (decl.* == .decl) {
        return decl.decl.name.symbol().?;
    } else if (decl.* == .method_decl or decl.* == .fn_decl or decl.* == .trait or decl.* == .struct_decl or decl.* == .enum_decl or decl.* == .type_alias or decl.* == .context_decl or decl.* == .type_param_decl) {
        return decl.symbol().?;
    } else if (decl.* == .binding) {
        return decl.binding.pattern.symbol().?;
    } else {
        std.debug.panic("compiler error: unsupported access symbol resolution for decl-like AST: {s}", .{@tagName(decl.*)});
    }
}

fn create_format_all_call(self: *const Self, ast: *ast_.AST, writer: *ast_.AST) !*ast_.AST {
    var format_all = ast_.AST.create_identifier(ast.token(), self.ctx.allocator());
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
    return ast_.AST.create_slice_of(ast.token(), args_array, false, self.ctx.allocator());
}
