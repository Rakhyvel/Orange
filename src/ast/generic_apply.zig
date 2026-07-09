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

fn instantiate_ast(ast: *AST, ctx: *Compiler_Context) !void {
    std.debug.assert(ast.* == .generic_apply);
    if (ast.symbol()) |_| return;
    ast.set_symbol(try monomorphize_generic_apply(ast.lhs().symbol().?, ast.generic_apply._children, ast.token().span, &ast.generic_apply.state, ctx));
}

fn instantiate_type(_type: *Type_AST, ctx: *Compiler_Context) !void {
    std.debug.assert(_type.* == .generic_apply);
    if (_type.symbol()) |_| return;
    _type.set_symbol(try monomorphize_generic_apply(_type.lhs().symbol().?, _type.generic_apply.args, _type.token().span, &_type.generic_apply.state, ctx));
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
    if (params.items.len != filtered_args.items.len) {
        ctx.errors.add_error(errs_.Error{ .mismatch_arity = .{
            .span = span,
            .takes = params.items.len,
            .given = filtered_args.items.len,
            .thing_name = sym.name,
            .takes_name = "generic parameter",
            .given_name = "argument",
        } });
        return error.CompileError;
    }

    // Constraints may reference sibling generic params, so substitute the given args into the constraints before checking them
    var constraint_subst = unification_.Substitutions.init(ctx.allocator());
    defer constraint_subst.deinit();
    for (filtered_args.items, 0..) |arg, i| {
        const param = params.items[i];
        switch (arg) {
            .type_arg => |ty| if (param.* == .type_param_decl) constraint_subst.put_type(param.symbol().?.name, ty) catch unreachable,
            .const_arg => |v| if (param.* == .const_param_decl) constraint_subst.put_const(param.symbol().?.name, v) catch unreachable,
        }
    }

    for (filtered_args.items, 0..) |arg, i| {
        const param = params.items[i];
        switch (param.*) {
            .type_param_decl => {
                const ty = arg.type_arg;
                try ctx.validate_type.validate_type(ty);

                var substd_constraints = std.array_list.Managed(*Type_AST).init(ctx.allocator());
                defer substd_constraints.deinit();
                for (param.type_param_decl.constraints.items) |constraint| {
                    substd_constraints.append(constraint.clone(&constraint_subst, ctx.allocator())) catch unreachable;
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
            else => unreachable,
        }
    }

    if (state.* == .unprocessed) {
        state.* = .processing;
        const new_sym = try sym.monomorphize(filtered_args, ctx);
        state.* = .processed;
        return new_sym;
    } else {
        return sym;
    }
}
