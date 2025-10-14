//! This file contains the semantic validation logic for scopes.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const typing_ = @import("typing.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

const Validate_Error_Enum = error{ OutOfMemory, CompileError };

const Self: type = @This();

ctx: *Compiler_Context,

pub fn init(ctx: *Compiler_Context) Self {
    return Self{ .ctx = ctx };
}

pub fn validate(self: *Self, scope: *Scope) Validate_Error_Enum!void {
    for (scope.symbols.keys()) |key| {
        const symbol = scope.symbols.get(key).?;

        try self.ctx.validate_symbol.validate(symbol);
    }
    for (scope.children.items) |child| {
        try self.validate(child);
    }
    for (scope.impls.items) |impl| {
        try self.validate_impl(impl);
    }
}

// TODO: Split up into smaller functions
fn validate_impl(self: *Self, impl: *ast_.AST) Validate_Error_Enum!void {
    if (impl.impl._type.* == .addr_of) {
        self.ctx.errors.add_error(errs_.Error{ .basic = .{
            .span = impl.impl._type.span(),
            .msg = "cannot implement method for address types",
        } });
        return error.CompileError;
    }

    _ = self.ctx.typecheck.typecheck_AST(impl.impl.trait.?, null) catch |e| switch (e) {
        error.UnexpectedTypeType => {
            self.ctx.errors.add_error(errs_.Error{ .basic = .{
                .span = impl.impl.trait.?.span(),
                .msg = "cannot implement for this, not a trait",
            } });
            return error.CompileError;
        },
        else => return error.CompileError,
    };
    try self.ctx.validate_type.validate(impl.impl._type);

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
            .first_defined_span = lookup_res.ast.?.span(),
            .redefined_span = impl.span(),
            .name = if (!impl.impl.impls_anon_trait) trait_symbol.name else null,
            ._type = impl.impl._type,
        } });
        return error.CompileError;
    }

    // Construct a map of all trait decls
    var trait_decls = std.StringArrayHashMap(*ast_.AST).init(self.ctx.allocator()); // Map name -> Method Decl
    defer trait_decls.deinit();
    for (trait_ast.trait.method_decls.items) |decl| {
        trait_decls.put(decl.method_decl.name.token().data, decl) catch unreachable;
    }

    // Subtract trait defs - impl decls
    for (impl.impl.method_defs.items) |def| {
        const def_key = def.method_decl.name.token().data;
        const trait_decl = trait_decls.get(def_key);

        // Check that the trait defines the method
        if (trait_decl == null) {
            self.ctx.errors.add_error(errs_.Error{ .method_not_in_trait = .{
                .method_span = def.span(),
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
            } });
            return error.CompileError;
        }

        // Check that receivers match
        if (!receivers_match(def.method_decl.receiver, trait_decl.?.method_decl.receiver)) {
            self.ctx.errors.add_error(errs_.Error{ .impl_receiver_mismatch = .{
                .receiver_span = if (def.method_decl.receiver != null) def.method_decl.receiver.?.span() else def.span(),
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
                .span = def.span(),
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
                .trait_arity = trait_decl.?.children().items.len + @intFromBool(trait_decl.?.method_decl.receiver != null),
                .impl_arity = def.children().items.len + @intFromBool(def.method_decl.receiver != null),
            } });
            return error.CompileError;
        }

        // Check that parameters match
        var subst = std.StringArrayHashMap(*Type_AST).init(self.ctx.allocator());
        defer subst.deinit();
        subst.put("Self", impl.impl._type) catch unreachable;

        for (def.children().items, trait_decl.?.children().items) |impl_param, trait_param| {
            const impl_type = impl_param.binding.type;
            const trait_type = Type_AST.clone(trait_param.binding.type, &subst, self.ctx.allocator());
            if (!impl_type.types_match(trait_type)) {
                self.ctx.errors.add_error(errs_.Error{ .mismatch_method_type = .{
                    .span = impl_param.binding.type.span(),
                    .method_name = def.method_decl.name.token().data,
                    .trait_name = trait_ast.token().data,
                    .trait_type = trait_type,
                    .impl_type = impl_type,
                } });
                return error.CompileError;
            }
        }

        // Check that return type matches
        const trait_method_ret_type = Type_AST.clone(trait_decl.?.method_decl.ret_type, &subst, self.ctx.allocator());
        const def_method_ret_type = Type_AST.clone(def.method_decl.ret_type, &subst, self.ctx.allocator());
        if (!def_method_ret_type.types_match(trait_method_ret_type)) {
            self.ctx.errors.add_error(errs_.Error{ .mismatch_method_type = .{
                .span = def.method_decl.ret_type.span(),
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
                .trait_type = trait_method_ret_type,
                .impl_type = def.method_decl.ret_type,
            } });
            return error.CompileError;
        }

        // Copy over the c_type from trait method decl
        def.method_decl.c_type = trait_decl.?.method_decl.c_type;

        // Verify that impl virtuality matches trait virtuality
        if (def.method_decl.is_virtual != trait_decl.?.method_decl.is_virtual) {
            self.ctx.errors.add_error(errs_.Error{ .mismatch_method_virtuality = .{
                .span = def.span(),
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
        _ = trait_decls.swapRemove(def_key);
    }

    var errant = false;
    for (trait_decls.keys()) |trait_key| {
        const trait_decl = trait_decls.get(trait_key).?;
        self.ctx.errors.add_error(errs_.Error{ .method_not_in_impl = .{
            .impl_span = impl.span(),
            .method_span = trait_decl.span(),
            .method_name = trait_decl.method_decl.name.token().data,
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
