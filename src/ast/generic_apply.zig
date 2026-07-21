const std = @import("std");
const AST = @import("../ast/ast.zig").AST;
const errs_ = @import("../util/errors.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const Symbol = @import("../symbol/symbol.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Span = @import("../util/span.zig");
const process_state_ = @import("../util/process_state.zig");
const generic_arg_ = @import("../ast/generic_arg.zig");
const GenericArg = generic_arg_.GenericArg;
const unification_ = @import("../types/unification.zig");

pub fn instantiate(generic_apply: anytype, ctx: *Compiler_Context) !void {
    if (@TypeOf(generic_apply) == *AST) {
        return instantiate_ast(generic_apply, ctx);
    } else if (@TypeOf(generic_apply) == *Type_AST) {
        return instantiate_type(generic_apply, ctx);
    } else {
        std.debug.panic("cannot call generic_apply.instantiate with arg of type {}", .{@TypeOf(generic_apply)});
    }
}

pub fn instantiate_generic_invoke(sym: *Symbol, ast: *AST, ctx: *Compiler_Context) !void {
    ast.set_symbol(try monomorphize_generic_apply(sym, ast.invoke.generic_args, ast.token().span, &ast.invoke.state, ctx));
}

fn instantiate_ast(ast: *AST, ctx: *Compiler_Context) !void {
    std.debug.assert(ast.* == .generic_apply);
    if (ast.symbol()) |_| return;
    ast.set_symbol(try monomorphize_generic_apply(ast.lhs().symbol().?, ast.generic_apply._children, ast.token().span, &ast.generic_apply.state, ctx));
}

fn instantiate_type(_type: *Type_AST, ctx: *Compiler_Context) !void {
    std.debug.assert(_type.* == .generic_apply);
    // An AST-to-type conversion can hand us a node that already has a symbol, it still needs baking
    if (_type.symbol()) |already| return bake_deferred_args(_type, already, ctx);
    const sym = try monomorphize_generic_apply(_type.lhs().symbol().?, _type.generic_apply.args, _type.token().span, &_type.generic_apply.state, ctx);
    _type.set_symbol(sym);
    bake_deferred_args(_type, sym, ctx);
}

/// Memoizes a deferred monomorph's arg-substituted expansion, its symbol is only the template
fn bake_deferred_args(_type: *Type_AST, sym: *Symbol, ctx: *Compiler_Context) void {
    if (_type.common()._expanded_type != null) return; // already baked
    const params = sym.decl.?.generic_params().items;
    if (params.len == 0) return; // fully monomorphized, its typedef is already concrete
    const typedef = sym.init_typedef() orelse return;

    const Symbol_Tree = @import("../ast/symbol-tree.zig");
    const Decorate = @import("../ast/decorate.zig");
    const walk_ = @import("../ast/walker.zig");

    // bake can run before the template's typedef is decorated, so its param refs still lack symbols and
    // the subst below (keyed by param symbol) would match nothing. Decorate it in the param scope first.
    // Skip if this typedef is already mid-decoration, a self-referential field would recurse forever
    if (!sym.baking_typedef) {
        sym.baking_typedef = true;
        defer sym.baking_typedef = false;
        const param_scope = params[0].symbol().?.scope;
        walk_.walk_type(typedef, Symbol_Tree.new(param_scope, &ctx.errors, ctx.allocator())) catch unreachable;
        walk_.walk_type(typedef, Decorate.new(ctx)) catch unreachable;
    }

    var subst = unification_.Substitutions.init(ctx.allocator());
    var param_i: usize = 0;
    for (_type.generic_apply.args.items) |arg| {
        if (param_i >= params.len) break;
        switch (arg) {
            .type_arg => |ty| {
                if (ty.* == .eq_constraint) continue;
                subst.put_type(params[param_i].symbol().?, ty) catch unreachable;
                param_i += 1;
            },
            .const_arg => |v| {
                subst.put_const(params[param_i].symbol().?, v) catch unreachable;
                param_i += 1;
            },
        }
    }

    const expanded = typedef.clone(&subst, ctx.allocator());
    // Clone wipes scopes, replay Symbol_Tree so accesses like `Iter::Item` resolve after substitution
    const scope = _type.scope() orelse sym.scope;
    walk_.walk_type(expanded, Symbol_Tree.new(scope, &ctx.errors, ctx.allocator())) catch unreachable;
    _type.set_expanded_type(expanded);
}

fn monomorphize_generic_apply(sym: *Symbol, args: std.array_list.Managed(GenericArg), span: Span, state: *process_state_.Process_State, ctx: *Compiler_Context) !*Symbol {
    // Filter out eq_constraint type args, keep all others
    var filtered_args = std.array_list.Managed(GenericArg).init(ctx.allocator());
    defer filtered_args.deinit();
    for (args.items) |arg| {
        switch (arg) {
            .type_arg => |ty| {
                if (ty.* == .eq_constraint) continue;
                filtered_args.append(.{ .type_arg = ty }) catch unreachable;
            },
            .const_arg => |v| filtered_args.append(.{ .const_arg = v }) catch unreachable,
        }
    }

    const params = sym.decl.?.generic_params();

    // Coerce args to the kinds the params expect
    var coerced_args = try coerce_generic_params(params.items, filtered_args.items, ctx);
    defer coerced_args.deinit();

    if (coerced_args.items.len == 0 and params.items.len > 0) {
        ctx.errors.add_error(.{ .unapplied_generic = .{
            .span = span,
            .symbol_name = sym.name,
            .num_generics = params.items.len,
        } });
        return error.CompileError;
    }

    // Check arity
    if (params.items.len != coerced_args.items.len) {
        ctx.errors.add_error(errs_.Error{ .mismatch_arity = .{
            .span = span,
            .takes = params.items.len,
            .given = coerced_args.items.len,
            .thing_name = sym.name,
            .takes_name = "generic parameter",
            .given_name = "argument",
        } });
        return error.CompileError;
    }

    // Constraints may reference sibling generic params, so substitute the given args into the constraints before checking them
    var constraint_subst = unification_.Substitutions.init(ctx.allocator());
    defer constraint_subst.deinit();
    for (coerced_args.items, 0..) |arg, i| {
        const param = params.items[i];
        switch (arg) {
            .type_arg => |ty| if (param.is_typelike_param_decl()) constraint_subst.put_type(param.symbol().?, ty) catch unreachable,
            .const_arg => |v| if (param.* == .const_param_decl) constraint_subst.put_const(param.symbol().?, v) catch unreachable,
        }
    }

    for (coerced_args.items, 0..) |arg, i| {
        const param = params.items[i];
        switch (param.*) {
            .type_param_decl => {
                const ty = arg.type_arg;
                try ctx.validate_type.validate_type(ty);

                const Symbol_Tree = @import("../ast/symbol-tree.zig");
                const walk_ = @import("../ast/walker.zig");
                var substd_constraints = std.array_list.Managed(*Type_AST).init(ctx.allocator());
                defer substd_constraints.deinit();
                for (param.type_param_decl.constraints.items) |constraint| {
                    const cloned = constraint.clone(&constraint_subst, ctx.allocator());
                    // Clone wipes scopes, replay Symbol_Tree so the constraint's accesses resolve
                    if (constraint.scope()) |sc| walk_.walk_type(cloned, Symbol_Tree.new(sc, &ctx.errors, ctx.allocator())) catch unreachable;
                    substd_constraints.append(cloned) catch unreachable;
                }

                const sat_res = try ty.satisfies_all_constraints(substd_constraints.items, null, ctx);
                switch (sat_res) {
                    .satisfies => {},
                    .not_impl => |unimpld| {
                        ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                            .span = ty.token().span,
                            .trait_name = unimpld.name,
                            ._type = ty,
                        } });
                        return error.CompileError;
                    },
                    .not_eq => |uneqd| {
                        ctx.errors.add_error(errs_.Error{ .eq_constraint_failed = .{
                            .call_span = ty.token().span,
                            .associated_type_name = uneqd.associated_type_name,
                            .constraint_span = uneqd.constraint_span,
                            .impl_span = uneqd.impl_span,
                            .expected = uneqd.expected,
                            .got = uneqd.got,
                        } });
                        return error.CompileError;
                    },
                    .no_such_assoc_type => |no_assoc| {
                        ctx.errors.add_error(errs_.Error{ .type_not_in_trait = .{
                            .type_span = no_assoc.eq_constraint.lhs().token().span,
                            .type_name = no_assoc.eq_constraint.lhs().token().data,
                            .trait_name = no_assoc.trait_name,
                        } });
                        return error.CompileError;
                    },
                }
            },
            .const_param_decl => {},
            .ability_param_decl => {
                const ty = arg.type_arg;
                if (try ty.resolve_ability_reference(ctx)) |resolved| {
                    // canonicalize so the subst and the monomorph key see the the ability's own name
                    ty.* = resolved.*;
                } else {
                    ctx.errors.add_error(errs_.Error{ .expected_ability = .{
                        .span = ty.token().span,
                        .got = ty,
                    } });
                    return error.CompileError;
                }
            },
            else => unreachable,
        }
    }

    if (state.* == .unprocessed) {
        state.* = .processing;
        const new_sym = try sym.monomorphize(coerced_args, ctx);
        state.* = .processed;
        return new_sym;
    } else {
        return sym;
    }
}

pub fn coerce_generic_params(params: []const *AST, args: []const GenericArg, ctx: *Compiler_Context) !std.array_list.Managed(GenericArg) {
    var generic_args = std.array_list.Managed(GenericArg).init(ctx.allocator());
    for (args, 0..) |arg, i| {
        const wants_const = i < params.len and params[i].* == .const_param_decl;
        const wants_ability = i < params.len and params[i].* == .ability_param_decl;
        switch (arg) {
            .const_arg => |v| if (wants_const) {
                try generic_args.append(.{ .const_arg = v });
            } else if (wants_ability) {
                ctx.errors.add_error(errs_.Error{ .basic = .{
                    .msg = "expected an ability argument, got a value",
                    .span = v.token().span,
                } });
                return error.CompileError;
            } else {
                ctx.errors.add_error(errs_.Error{ .basic = .{
                    .msg = "expected a type argument, got a value",
                    .span = v.token().span,
                } });
                return error.CompileError;
            },

            .type_arg => |ty| if (wants_const) {
                try generic_args.append(.{ .const_arg = ty.to_value_expr(ctx.allocator()) orelse {
                    ctx.errors.add_error(errs_.Error{ .basic = .{ .msg = "expected a value, got a type", .span = ty.token().span } });
                    return error.CompileError;
                } });
            } else if (wants_ability) {
                const resolved = (try ty.resolve_ability_reference(ctx)) orelse {
                    ctx.errors.add_error(errs_.Error{ .expected_ability = .{
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
    return generic_args;
}
