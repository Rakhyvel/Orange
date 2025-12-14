const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Tree_Writer = @import("../ast/tree_writer.zig");

pub const Substitutions = std.StringArrayHashMap(*Type_AST);

// Attempt to match the rhs with the lhs
pub fn unify(lhs: *Type_AST, rhs: *Type_AST, subst: *Substitutions) !void {
    std.debug.print("{f} ~ {f}\n", .{ lhs, rhs });
    std.debug.print("{t} ~ {t}\n\n", .{ lhs.*, rhs.* });

    if (lhs.is_ident_type("Void")) {
        return; // Bottom type - vacuously true
    }

    if (lhs.* == .identifier and lhs.symbol().?.init_typedef() != null) {
        return try unify(lhs.symbol().?.init_typedef().?, rhs, subst);
    } else if (rhs.* == .identifier and rhs.symbol().?.init_typedef() != null) {
        return try unify(lhs, rhs.symbol().?.init_typedef().?, subst);
    }

    if (rhs.* == .access and rhs.symbol().?.init_typedef() != null) {
        return try unify(lhs, rhs.symbol().?.init_typedef().?, subst);
    }
    if (rhs.* == .access and rhs.lhs().symbol().?.decl.?.* == .type_param_decl) {
        if (rhs.associated_type_from_constraint()) |assoc_type| return try unify(lhs, assoc_type, subst);
    }

    if ((lhs.* == .identifier or lhs.* == .access) and lhs.symbol().?.decl.?.* == .type_param_decl) {
        try subst.put(lhs.symbol().?.name, rhs);
        return;
    }
    if ((rhs.* == .identifier or rhs.* == .access) and rhs.symbol().?.decl.?.* == .type_param_decl) {
        try subst.put(rhs.symbol().?.name, lhs);
        return;
    }

    switch (lhs.*) {
        .identifier => {
            std.debug.assert(lhs.token().data.len > 0);

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
                try unify(l_arg, r_arg, subst);
            }
        },

        .enum_type, .untagged_sum_type => {
            if (rhs.* != .enum_type and rhs.* != .untagged_sum_type) {
                return error.TypesMismatch;
            }
            if (!lengths_match(lhs.children(), rhs.children())) return error.TypeMismatch;
            // need to make sure the types and variant names match
            for (lhs.children().items, rhs.children().items) |term, B_term| {
                const this_name = term.annotation.pattern.token().data;
                const B_name = B_term.annotation.pattern.token().data;
                if (!std.mem.eql(u8, this_name, B_name)) return error.TypeMismatch;
                try unify(term, B_term, subst);
            }
        },

        .annotation => {
            if (rhs.* != .annotation) {
                return error.TypesMismatch;
            }

            try unify(lhs.child(), rhs.child(), subst);
        },

        .addr_of => {
            if (rhs.* != .addr_of) return error.TypesMismatch;
            if (lhs.addr_of.mut != rhs.addr_of.mut) return error.TypesMismatch;
            if (lhs.addr_of.multiptr != rhs.addr_of.multiptr) return error.TypesMismatch;

            try unify(lhs.child(), rhs.child(), subst);
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
            if (lhs_constructor.symbol() != rhs_constructor.symbol()) {
                return error.TypesMismatch;
            }
            if (lhs.children().items.len != rhs.children().items.len) {
                std.debug.print("arg lengths differ\n", .{});
                return error.TypesMismatch;
            }

            for (lhs.children().items, rhs.children().items) |l_arg, r_arg| {
                try unify(l_arg, r_arg, subst);
            }
        },

        else => return, // TODO: Support more types, add error for unsupported constructs
    }
}

fn lengths_match(as: *const std.array_list.Managed(*Type_AST), bs: *const std.array_list.Managed(*Type_AST)) bool {
    return bs.items.len == as.items.len;
}

pub fn type_param_list_from_subst_map(subst: *Substitutions, generic_params: std.array_list.Managed(*ast_.AST), alloc: std.mem.Allocator) std.array_list.Managed(*Type_AST) {
    var retval = std.array_list.Managed(*Type_AST).init(alloc);
    for (generic_params.items) |type_param| {
        const with_value = subst.get(type_param.token().data) orelse continue;
        retval.append(with_value) catch unreachable;
    }
    return retval;
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
