const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

pub const Substitutions = std.StringArrayHashMap(*Type_AST);

// Attempt to match the rhs with the lhs
pub fn unify(lhs: *Type_AST, rhs: *Type_AST, withs: std.array_list.Managed(*ast_.AST), subst: *Substitutions) !void {
    if (lhs.* == .identifier and lhs.symbol().?.init_typedef() != null) {
        return try unify(lhs.symbol().?.init_typedef().?, rhs, withs, subst);
    } else if (rhs.* == .identifier and rhs.symbol().?.init_typedef() != null) {
        return try unify(lhs, rhs.symbol().?.init_typedef().?, withs, subst);
    }

    if (rhs.* == .access and rhs.symbol().?.init_typedef() != null) {
        return try unify(lhs, rhs.symbol().?.init_typedef().?, withs, subst);
    }

    switch (lhs.*) {
        .identifier => {
            std.debug.assert(lhs.token().data.len > 0);
            if (identifier_is_type_param(lhs, withs)) |_| {
                try subst.put(lhs.token().data, rhs);
                return;
            }

            if (rhs.* != .identifier or !std.mem.eql(u8, lhs.token().data, rhs.token().data)) {
                return error.TypesMismatch;
            }
        },

        .struct_type => {
            if (rhs.* != .struct_type) {
                return error.TypesMismatch;
            }
            if (lhs.children().items.len != rhs.children().items.len) {
                return error.TypesMismatch;
            }

            for (lhs.children().items, rhs.children().items) |l_arg, r_arg| {
                try unify(l_arg, r_arg, withs, subst);
            }
        },

        .annotation => {
            if (rhs.* != .annotation) {
                return error.TypesMismatch;
            }

            try unify(lhs.child(), rhs.child(), withs, subst);
        },

        .addr_of => {
            if (rhs.* != .addr_of) return error.TypesMismatch;
            if (lhs.addr_of.mut != rhs.addr_of.mut) return error.TypesMismatch;
            if (lhs.addr_of.multiptr != rhs.addr_of.multiptr) return error.TypesMismatch;

            try unify(lhs.child(), rhs.child(), withs, subst);
        },

        .unit_type => {
            if (rhs.* != .unit_type) {
                return error.TypesMismatch;
            }
        },

        .generic_apply => {
            if (rhs.* != .generic_apply) {
                return error.TypesMismatch;
            }
            const lhs_constructor = lhs.lhs();
            const rhs_constructor = rhs.lhs();
            if (!std.mem.eql(u8, lhs_constructor.token().data, rhs_constructor.token().data)) {
                return error.TypesMismatch;
            }
            if (lhs.children().items.len != rhs.children().items.len) {
                return error.TypesMismatch;
            }

            for (lhs.children().items, rhs.children().items) |l_arg, r_arg| {
                try unify(l_arg, r_arg, withs, subst);
            }
        },

        else => return, // TODO: Support more types, add error for unsupported constructs
    }
}

pub fn type_param_list_from_subst_map(subst: *Substitutions, generic_params: std.array_list.Managed(*ast_.AST), alloc: std.mem.Allocator) std.array_list.Managed(*Type_AST) {
    var retval = std.array_list.Managed(*Type_AST).init(alloc);
    for (generic_params.items) |type_param| {
        const with_value = subst.get(type_param.token().data).?;
        retval.append(with_value) catch unreachable;
    }
    return retval;
}

fn identifier_is_type_param(ident: *Type_AST, generic_params: std.array_list.Managed(*ast_.AST)) ?*ast_.AST {
    for (generic_params.items) |type_param| {
        if (std.mem.eql(u8, type_param.token().data, ident.token().data)) {
            return type_param;
        }
    }
    return null;
}

pub fn substitution_contains_generics(subst: *const Substitutions) bool {
    for (subst.keys()) |key| {
        const ty = subst.get(key).?;
        const bad = ty.* == .identifier and ty.symbol().?.decl.?.* == .type_param_decl;
        if (bad) {
            return true;
        }
    }
    return false;
}

pub fn print_substitutions(subst: *const Substitutions) void {
    std.debug.print("{} substitutions: {{\n", .{subst.keys().len});
    for (subst.keys()) |key| {
        const ty = subst.get(key).?;
        const bad = ty.* == .identifier and ty.symbol().?.decl.?.* == .type_param_decl;
        std.debug.print("    {s}: {?f} ({})\n", .{ key, subst.get(key), bad });
    }
    std.debug.print("}}\n", .{});
}
