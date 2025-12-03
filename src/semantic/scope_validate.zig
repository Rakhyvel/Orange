//! This file contains the semantic validation logic for scopes.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

const Validate_Error_Enum = error{ OutOfMemory, CompileError };

const Self: type = @This();

ctx: *Compiler_Context,

pub fn init(ctx: *Compiler_Context) Self {
    return Self{ .ctx = ctx };
}

pub fn validate_scope(self: *Self, scope: *Scope) Validate_Error_Enum!void {
    for (scope.symbols.keys()) |key| {
        const symbol = scope.symbols.get(key).?;

        try self.ctx.validate_symbol.validate_symbol(symbol);
    }
    var i: usize = 0;
    while (i < scope.children.items.len) : (i += 1) {
        const child = scope.children.items[i];
        try self.validate_scope(child);
    }
    for (scope.impls.items) |impl| {
        try self.validate_impl(impl);
    }
}

// TODO: Split up into smaller functions
fn validate_impl(self: *Self, impl: *ast_.AST) Validate_Error_Enum!void {
    if (impl.impl._type.* == .addr_of) {
        self.ctx.errors.add_error(errs_.Error{ .basic = .{
            .span = impl.impl._type.token().span,
            .msg = "cannot implement method for address types",
        } });
        return error.CompileError;
    }

    _ = self.ctx.typecheck.typecheck_AST(impl.impl.trait.?, null) catch |e| switch (e) {
        error.UnexpectedTypeType => {
            self.ctx.errors.add_error(errs_.Error{ .basic = .{
                .span = impl.impl.trait.?.token().span,
                .msg = "cannot implement for this, not a trait",
            } });
            return error.CompileError;
        },
        else => return error.CompileError,
    };
    try self.ctx.validate_type.validate_type(impl.impl._type);

    const trait_symbol: *Symbol = impl.impl.trait.?.symbol().?;
    if (trait_symbol.kind != .trait) {
        self.ctx.errors.add_error(errs_.Error{ .basic = .{
            .span = trait_symbol.span(),
            .msg = "cannot implement for this, not a trait",
        } });
        return error.CompileError;
    }
    const trait_ast = trait_symbol.decl.?;

    // Check that the (trait, type) pair is unique for this scope
    const lookup_res = impl.scope().?.impl_trait_lookup(impl.impl._type, trait_symbol);
    if (lookup_res.count > 1) {
        // Check if there's already an implementation for the same trait and type
        self.ctx.errors.add_error(errs_.Error{ .reimpl = .{
            .first_defined_span = lookup_res.ast.?.token().span,
            .redefined_span = impl.token().span,
            .name = if (!impl.impl.impls_anon_trait) trait_symbol.name else null,
            ._type = impl.impl._type,
        } });
        return error.CompileError;
    }

    // Check that super traits are implemented
    for (trait_ast.trait.super_traits.items) |super_trait| {
        const super_trait_symbol = super_trait.symbol().?;
        const super_lookup_res = impl.scope().?.impl_trait_lookup(impl.impl._type, super_trait_symbol);
        if (super_lookup_res.count == 0) {
            self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                ._type = impl.impl._type,
                .span = impl.token().span,
                .trait_name = super_trait.symbol().?.name,
            } });
            return error.CompileError;
        }
    }

    // Construct a map of all trait decls
    var trait_method_decls = std.StringArrayHashMap(*ast_.AST).init(self.ctx.allocator()); // Map name -> Method Decl
    defer trait_method_decls.deinit();
    for (trait_ast.trait.method_decls.items) |decl| {
        trait_method_decls.put(decl.method_decl.name.token().data, decl) catch unreachable;
    }
    var trait_type_decls = std.StringArrayHashMap(*ast_.AST).init(self.ctx.allocator());
    defer trait_type_decls.deinit();
    for (trait_ast.trait.type_decls.items) |decl| {
        trait_type_decls.put(decl.token().data, decl) catch unreachable;
    }

    // Subtract trait defs - impl decls
    for (impl.impl.method_defs.items) |def| {
        const def_key = def.method_decl.name.token().data;
        const trait_decl = trait_method_decls.get(def_key);

        // Check that the trait defines the method
        if (trait_decl == null) {
            self.ctx.errors.add_error(errs_.Error{ .method_not_in_trait = .{
                .method_span = def.token().span,
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
            } });
            return error.CompileError;
        }

        // Check that receivers match
        if (!receivers_match(def.method_decl.receiver, trait_decl.?.method_decl.receiver)) {
            self.ctx.errors.add_error(errs_.Error{ .impl_receiver_mismatch = .{
                .receiver_span = if (def.method_decl.receiver != null) def.method_decl.receiver.?.token().span else def.token().span,
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
                .trait_receiver = if (trait_decl.?.method_decl.receiver != null) trait_decl.?.method_decl.receiver.?.receiver.kind else null,
                .impl_receiver = if (def.method_decl.receiver != null) def.method_decl.receiver.?.receiver.kind else null,
            } });
            return error.CompileError;
        }

        // Check that paramter arity matches
        if (def.children().items.len != trait_decl.?.children().items.len) {
            self.ctx.errors.add_error(errs_.Error{ .mismatch_method_param_arity = .{
                .span = def.token().span,
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
                .trait_arity = trait_decl.?.children().items.len + @intFromBool(trait_decl.?.method_decl.receiver != null),
                .impl_arity = def.children().items.len + @intFromBool(def.method_decl.receiver != null),
            } });
            return error.CompileError;
        }

        // Check that parameters match
        for (def.children().items, trait_decl.?.children().items) |impl_param, trait_param| {
            const impl_type = impl_param.binding.type;
            if (!impl_type.types_match(trait_param.binding.type)) {
                self.ctx.errors.add_error(errs_.Error{ .mismatch_method_type = .{
                    .span = impl_param.binding.type.token().span,
                    .method_name = def.method_decl.name.token().data,
                    .trait_name = trait_ast.token().data,
                    .trait_type = trait_param.binding.type,
                    .impl_type = impl_type,
                } });
                return error.CompileError;
            }
        }

        // Check that return type matches
        if (!def.method_decl.ret_type.types_match(trait_decl.?.method_decl.ret_type)) {
            self.ctx.errors.add_error(errs_.Error{ .mismatch_method_type = .{
                .span = def.method_decl.ret_type.token().span,
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
                .trait_type = trait_decl.?.method_decl.ret_type,
                .impl_type = def.method_decl.ret_type,
            } });
            return error.CompileError;
        }

        // Copy over the c_type from trait method decl
        def.method_decl.c_type = trait_decl.?.method_decl.c_type;

        // Verify that impl virtuality matches trait virtuality
        if (def.method_decl.is_virtual != trait_decl.?.method_decl.is_virtual) {
            self.ctx.errors.add_error(errs_.Error{ .mismatch_method_virtuality = .{
                .span = def.token().span,
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
                .trait_method_is_virtual = trait_decl.?.method_decl.is_virtual,
                .impl_method_is_virtual = def.method_decl.is_virtual,
            } });
            return error.CompileError;
        }

        if (def.method_decl.is_virtual) {
            impl.impl.num_virtual_methods += 1;
        }

        // Subtract the method from the set
        _ = trait_method_decls.swapRemove(def_key);
    }

    for (impl.impl.type_defs.items) |typedef| {
        const def_key = typedef.type_alias.name.token().data;
        const trait_type_decl = trait_type_decls.get(def_key);

        // Check that the trait defines the method
        if (trait_type_decl == null) {
            self.ctx.errors.add_error(errs_.Error{ .type_not_in_trait = .{
                .type_span = typedef.token().span,
                .type_name = typedef.type_alias.name.token().data,
                .trait_name = trait_ast.token().data,
            } });
            return error.CompileError;
        }

        // Check that contraints match
        if (typedef.decl_typedef().?.satisfies_all_constraints(trait_type_decl.?.type_param_decl.constraints.items)) |unsatisfied_trait| {
            self.ctx.errors.add_error(errs_.Error{ .unsatisfied_constraint = .{
                .type_span = typedef.decl_typedef().?.token().span,
                .type = typedef.decl_typedef().?,
                .trait_name = unsatisfied_trait.name,
            } });
            return error.CompileError;
        }
        // Subtract the type from the set
        _ = trait_type_decls.swapRemove(def_key);
    }

    var errant = false;
    for (trait_method_decls.keys()) |trait_key| {
        const trait_decl = trait_method_decls.get(trait_key).?;
        self.ctx.errors.add_error(errs_.Error{ .method_not_in_impl = .{
            .impl_span = impl.token().span,
            .method_span = trait_decl.token().span,
            .method_name = trait_decl.method_decl.name.token().data,
            .trait_name = trait_ast.token().data,
        } });
        errant = true;
    }
    for (trait_type_decls.keys()) |trait_key| {
        const trait_decl = trait_type_decls.get(trait_key).?;
        self.ctx.errors.add_error(errs_.Error{ .type_not_in_impl = .{
            .impl_span = impl.token().span,
            .type_span = trait_decl.token().span,
            .type_name = trait_decl.token().data,
            .trait_name = trait_ast.token().data,
        } });
        errant = true;
    }
    if (errant) {
        return error.CompileError;
    }

    for (impl.impl.method_defs.items) |def| {
        _ = self.ctx.typecheck.typecheck_AST(def, null) catch return error.CompileError;
    }
}

fn receivers_match(a: ?*ast_.AST, b: ?*ast_.AST) bool {
    if (a == null and b != null) {
        return false;
    } else if (a != null and b == null) {
        return false;
    } else if (a == null and b == null) {
        return true;
    } else {
        return a.?.receiver.kind == b.?.receiver.kind;
    }
}
