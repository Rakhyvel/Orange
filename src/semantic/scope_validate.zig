//! This file contains the semantic validation logic for scopes.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Decorate = @import("../ast/decorate.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const Symbol_Tree = @import("../ast/symbol-tree.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const unification_ = @import("../types/unification.zig");
const walk_ = @import("../ast/walker.zig");

const Validate_Error_Enum = error{ OutOfMemory, CompileError };

const Self: type = @This();

ctx: *Compiler_Context,

pub fn init(ctx: *Compiler_Context) Self {
    return Self{ .ctx = ctx };
}

pub fn validate_scope(self: *Self, scope: *Scope) Validate_Error_Enum!void {
    var i: usize = 0;
    while (i < scope.symbols.keys().len) : (i += 1) {
        const key = scope.symbols.keys()[i];
        const symbol = scope.symbols.get(key).?;

        try self.ctx.validate_symbol.validate_symbol(symbol);
    }
    i = 0;
    while (i < scope.children.items.len) : (i += 1) {
        const child = scope.children.items[i];
        try self.validate_scope(child);
    }
}

// TODO: Split up into smaller functions
pub fn validate_impl(self: *Self, impl: *ast_.AST) Validate_Error_Enum!void {
    if (impl.impl._type.* == .addr_of and !impl.impl._type.addr_of.multiptr) {
        self.ctx.errors.add_error(errs_.Error{ .basic = .{
            .span = impl.impl._type.token().span,
            .msg = "cannot implement method for address types",
        } });
        return error.CompileError;
    }

    try self.ctx.validate_type.validate_type(impl.impl._type);

    if (impl.impl._type.* != .generic_apply and impl.impl._type.has_symbol() and impl.impl._type.symbol().?.decl.?.num_generic_params() > 0) {
        self.ctx.errors.add_error(errs_.Error{ .unapplied_generic = .{
            .span = impl.impl._type.token().span,
            .symbol_name = impl.impl._type.symbol().?.name,
            .num_generics = impl.impl._type.symbol().?.decl.?.num_generic_params(),
        } });
        return error.CompileError;
    }

    var subst = unification_.generate_substitutions(impl.impl._type, self.ctx.allocator());
    defer subst.deinit();

    _ = self.ctx.typecheck.typecheck_AST(impl.impl.trait.?, null, &subst) catch |e| switch (e) {
        error.UnexpectedTypeType => {
            self.ctx.errors.add_error(errs_.Error{ .basic = .{
                .span = impl.impl.trait.?.token().span,
                .msg = "cannot implement for this, not a trait",
            } });
            return error.CompileError;
        },
        else => return error.CompileError,
    };

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
    // TODO: For next release, add check that there is at least one most-specific impl
    // const lookup_res = impl.scope().?.impl_trait_lookup(impl.impl._type, trait_symbol);
    // if (lookup_res.count > 1) {
    //     // Check if there's already an implementation for the same trait and type
    //     self.ctx.errors.add_error(errs_.Error{ .reimpl = .{
    //         .first_defined_span = lookup_res.ast.?.token().span,
    //         .redefined_span = impl.token().span,
    //         .name = if (!impl.impl.impls_anon_trait) trait_symbol.name else null,
    //         ._type = impl.impl._type,
    //     } });
    //     return error.CompileError;
    // }

    // Check that super traits are implemented
    for (trait_ast.trait.super_traits.items) |super_trait| {
        const super_trait_symbol = super_trait.symbol().?;
        const super_lookup_res = try Scope.impl_trait_lookup(impl.impl._type, super_trait_symbol, self.ctx);
        if (super_lookup_res.count == 0) {
            self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                ._type = impl.impl._type,
                .span = impl.token().span,
                .trait_name = super_trait.symbol().?.name,
            } });
            return error.CompileError;
        }
        // Add to super trait impls if impl was found. Might not be found for type parameters, thats ok
        if (super_lookup_res.impl_ast) |impl_ast| try trait_ast.trait.super_trait_impls.append(impl_ast);
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

    // Subtract trait defs from impl decls
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

        // Check that generic parameter arity matches
        if (def.num_generic_params() != trait_decl.?.num_generic_params()) {
            self.ctx.errors.add_error(errs_.Error{ .mismatch_method_generic_param_arity = .{
                .span = def.token().span,
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
                .trait_arity = trait_decl.?.num_generic_params(),
                .impl_arity = def.num_generic_params(),
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

        // Generate the rename map that takes trait method generic param symbol names to their impl method counterpart type ident
        var rename = unification_.Substitutions.init(self.ctx.allocator());
        defer rename.deinit();
        for (trait_decl.?.generic_params().items, def.method_decl._generic_params.items) |trait_gp, impl_gp| {
            if (@intFromEnum(trait_gp.*) != @intFromEnum(impl_gp.*)) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{
                    .span = impl_gp.token().span,
                    .msg = "generic parameter kind does not match the trait's declaration",
                } });
                return error.CompileError;
            }
            // Shared decls (anon traits, monomorphized impls) share constraints too, and renaming would unshare them
            if (trait_gp.symbol() == impl_gp.symbol()) continue;
            if (trait_gp.* == .type_param_decl) {
                // Each constraint the trait requires must be guaranteed by the impl's param
                for (trait_gp.type_param_decl.constraints.items) |trait_constraint| {
                    if (trait_constraint.* == .eq_constraint) continue;
                    // A structurally-equal constraint on the impl param satisfies it directly. Handles monomorph
                    // instances of the same generic trait comparing unequal by symbol identity
                    var structurally_matched = false;
                    for (impl_gp.type_param_decl.constraints.items) |impl_constraint| {
                        if (impl_constraint.* == .eq_constraint) continue;
                        var c_subst = unification_.Substitutions.init(self.ctx.allocator());
                        defer c_subst.deinit();
                        unification_.unify(impl_constraint, trait_constraint, &c_subst, .{}) catch continue;
                        structurally_matched = true;
                        break;
                    }
                    if (structurally_matched) continue;
                    const required = try Decorate.symbol(trait_constraint, self.ctx);
                    if (!(try Scope.param_guarantees_trait(impl_gp, required, self.ctx))) {
                        self.ctx.errors.add_error(errs_.Error{ .mismatch_method_constraint = .{
                            .span = impl_gp.token().span,
                            .param_name = impl_gp.token().data,
                            .constraint_name = required.name,
                            .method_name = def.method_decl.name.token().data,
                            .trait_name = trait_ast.token().data,
                            .impl_declares_extra = false,
                        } });
                        return error.CompileError;
                    }
                }
                // The impl's param must not require more than the trait promises
                for (impl_gp.type_param_decl.constraints.items) |impl_constraint| {
                    if (impl_constraint.* == .eq_constraint) continue;
                    // A structurally-equal constraint on the trait param satisfies it, mirror of the check above
                    var structurally_matched = false;
                    for (trait_gp.type_param_decl.constraints.items) |trait_constraint| {
                        if (trait_constraint.* == .eq_constraint) continue;
                        var c_subst = unification_.Substitutions.init(self.ctx.allocator());
                        defer c_subst.deinit();
                        unification_.unify(impl_constraint, trait_constraint, &c_subst, .{}) catch continue;
                        structurally_matched = true;
                        break;
                    }
                    if (structurally_matched) continue;
                    const declared = try Decorate.symbol(impl_constraint, self.ctx);
                    if (!(try Scope.param_guarantees_trait(trait_gp, declared, self.ctx))) {
                        self.ctx.errors.add_error(errs_.Error{ .mismatch_method_constraint = .{
                            .span = impl_constraint.token().span,
                            .param_name = impl_gp.token().data,
                            .constraint_name = declared.name,
                            .method_name = def.method_decl.name.token().data,
                            .trait_name = trait_ast.token().data,
                            .impl_declares_extra = true,
                        } });
                        return error.CompileError;
                    }
                }
            }
            const id = Type_AST.create_type_identifier(impl_gp.token(), self.ctx.allocator());
            id.set_symbol(impl_gp.symbol().?);
            rename.put_type(trait_gp.symbol().?, id) catch unreachable;
        }
        rename.put_type(trait_ast.trait.self_symbol.?, impl.impl._type) catch unreachable;

        // Check that parameter types match
        for (def.children().items, trait_decl.?.children().items) |impl_param, trait_param| {
            const trait_ty = trait_param.binding.type.clone(&rename, self.ctx.allocator());
            _ = try walk_.walk_type(trait_ty, Symbol_Tree.new(impl.scope().?, &self.ctx.errors, self.ctx.allocator()));
            _ = try walk_.walk_type(trait_ty, Decorate.new(self.ctx));
            unification_.unify(trait_ty, impl_param.binding.type, &subst, .{ .allow_rigid = false }) catch {
                self.ctx.errors.add_error(errs_.Error{ .mismatch_method_type = .{
                    .span = impl_param.token().span,
                    .method_name = def.method_decl.name.token().data,
                    .trait_name = trait_ast.token().data,
                    .trait_type = trait_param.binding.type,
                    .impl_type = impl_param.binding.type,
                } });
                return error.CompileError;
            };
        }

        // Check that return type matches
        const trait_ret_ty = trait_decl.?.method_decl.ret_type.clone(&rename, self.ctx.allocator());
        _ = try walk_.walk_type(trait_ret_ty, Symbol_Tree.new(impl.scope().?, &self.ctx.errors, self.ctx.allocator()));
        _ = try walk_.walk_type(trait_ret_ty, Decorate.new(self.ctx));
        unification_.unify(trait_ret_ty, def.method_decl.ret_type, &subst, .{ .allow_rigid = false }) catch {
            self.ctx.errors.add_error(errs_.Error{ .mismatch_method_type = .{
                .span = def.method_decl.ret_type.token().span,
                .method_name = def.method_decl.name.token().data,
                .trait_name = trait_ast.token().data,
                .trait_type = trait_decl.?.method_decl.ret_type,
                .impl_type = def.method_decl.ret_type,
            } });
            return error.CompileError;
        };

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
        try walk_.walk_type(typedef.decl_typedef().?, Decorate.new(self.ctx));
        const sat_res = try typedef.decl_typedef().?.satisfies_all_constraints(trait_type_decl.?.type_param_decl.constraints.items, impl.impl.type_defs.items, self.ctx);
        switch (sat_res) {
            .satisfies => {},
            .not_impl => |unimpld| {
                self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                    .span = typedef.token().span,
                    .trait_name = unimpld.name,
                    ._type = typedef.decl_typedef().?,
                } });
                return error.CompileError;
            },
            .not_eq => |uneqd| {
                self.ctx.errors.add_error(errs_.Error{ .eq_constraint_failed = .{
                    .call_span = typedef.token().span,
                    .associated_type_name = uneqd.associated_type_name,
                    .constraint_span = uneqd.constraint_span,
                    .impl_span = uneqd.impl_span,
                    .expected = uneqd.expected,
                    .got = uneqd.got,
                } });
                return error.CompileError;
            },
            .no_such_assoc_type => |no_assoc| {
                self.ctx.errors.add_error(errs_.Error{ .type_not_in_trait = .{
                    .type_span = no_assoc.eq_constraint.lhs().token().span,
                    .type_name = no_assoc.eq_constraint.lhs().token().data,
                    .trait_name = no_assoc.trait_name,
                } });
                return error.CompileError;
            },
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
        _ = self.ctx.typecheck.typecheck_AST(def.method_decl.init.?, def.method_decl.ret_type, &subst) catch |e| switch (e) {
            error.CompileError => return error.CompileError,
            error.OutOfMemory => return error.OutOfMemory,
            error.UnexpectedTypeType => {
                self.ctx.errors.add_error(errs_.Error{ .unexpected_type_type = .{ .expected = def.method_decl.ret_type, .span = def.method_decl.init.?.token().span } });
                return error.CompileError;
            },
        };
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
