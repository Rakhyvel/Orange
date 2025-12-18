const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Symbol = @import("../symbol/symbol.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");

pub const Substitutions = std.StringArrayHashMap(*Type_AST);
// pub const Sym_Substitutions = std.array_hash_map.AutoArrayHashMap(*Symbol, *Type_AST);
const Pair = struct { lhs: *Type_AST, rhs: *Type_AST };
const Visited_Map = std.array_hash_map.AutoArrayHashMap(Pair, void);

const Check_Mode = enum { unify, assignable };
const Options = struct { allow_rigid: bool = true, mode: Check_Mode = .unify };

// Attempt to match the rhs with the lhs
pub fn unify(lhs: *Type_AST, rhs: *Type_AST, subst: *Substitutions, options: Options) !void {
    var visited_map = Visited_Map.init(std.heap.page_allocator);
    defer visited_map.deinit();

    try unify_inner(lhs, rhs, subst, &visited_map, options);
}

fn unify_inner(lhs: *Type_AST, rhs: *Type_AST, subst: *Substitutions, visited_map: *Visited_Map, options: Options) !void {
    if (visited_map.contains(Pair{ .lhs = lhs, .rhs = rhs })) {
        return;
    }
    try visited_map.put(Pair{ .lhs = lhs, .rhs = rhs }, void{});

    if (lhs.is_ident_type("Void")) {
        return; // Bottom type - vacuously true
    }

    if (lhs.* == .annotation) return try unify_inner(lhs.child(), rhs, subst, visited_map, options);
    if (rhs.* == .annotation) return try unify_inner(lhs, rhs.child(), subst, visited_map, options);

    if (lhs.* == .identifier and subst.get(lhs.symbol().?.name) != null) {
        if (lhs.symbol().?.decl.?.* != .type_param_decl or !lhs.symbol().?.decl.?.type_param_decl.rigid) {
            const sub = subst.get(lhs.symbol().?.name).?;
            return try unify_inner(sub, rhs, subst, visited_map, options);
        }
    }
    if (rhs.* == .identifier and subst.get(rhs.symbol().?.name) != null) {
        const sub = subst.get(rhs.symbol().?.name).?;
        if (rhs.symbol().?.decl.?.* != .type_param_decl or !rhs.symbol().?.decl.?.type_param_decl.rigid) {
            return try unify_inner(lhs, sub, subst, visited_map, options);
        }
    }

    if (lhs.* == .identifier and lhs.symbol() != null and lhs.symbol().?.init_typedef() != null) {
        return try unify_inner(lhs.symbol().?.init_typedef().?, rhs, subst, visited_map, options);
    } else if (rhs.* == .identifier and rhs.symbol().?.init_typedef() != null) {
        return try unify_inner(lhs, rhs.symbol().?.init_typedef().?, subst, visited_map, options);
    }

    if (lhs.* == .access) {
        if (lhs.symbol() != null and lhs.symbol().?.init_typedef() != null) {
            return try unify_inner(lhs.symbol().?.init_typedef().?, rhs, subst, visited_map, options);
        }
        // if (lhs.lhs().symbol().?.decl.?.* == .type_param_decl) {
        //     if (lhs.associated_type_from_constraint()) |assoc_type|
        //         return try unify_inner(assoc_type, rhs, subst, visited_map, options);
        // }
    }

    if (rhs.* == .access) {
        if (rhs.symbol() != null and rhs.symbol().?.init_typedef() != null) {
            return try unify_inner(lhs, rhs.symbol().?.init_typedef().?, subst, visited_map, options);
        }
        if (rhs.lhs().symbol().?.decl.?.* == .type_param_decl) {
            if (rhs.associated_type_from_constraint()) |assoc_type|
                return try unify_inner(lhs, assoc_type, subst, visited_map, options);
        }
    }

    const lhs_is_type_param = (lhs.* == .identifier or lhs.* == .access) and lhs.symbol() != null and lhs.symbol().?.decl.?.* == .type_param_decl;
    const rhs_is_type_param = (rhs.* == .identifier or rhs.* == .access) and rhs.symbol() != null and rhs.symbol().?.decl.?.* == .type_param_decl;

    if (lhs_is_type_param) {
        if (!options.allow_rigid and lhs.symbol().?.decl.?.type_param_decl.rigid and !can_unify_rigid(lhs, rhs)) {
            return error.TypesMismatch;
        }
        try subst.put(lhs.symbol().?.name, rhs);
        if (!options.allow_rigid and lhs.symbol().?.decl.?.type_param_decl.rigid and !can_unify_rigid(lhs, rhs)) {
            return error.TypesMismatch;
        }
        return;
    }
    if (rhs_is_type_param) {
        if (!options.allow_rigid and rhs.symbol().?.decl.?.type_param_decl.rigid and !can_unify_rigid(rhs, lhs)) {
            return error.TypesMismatch;
        }
        try subst.put(rhs.symbol().?.name, lhs);
        if (!options.allow_rigid and rhs.symbol().?.decl.?.type_param_decl.rigid and !can_unify_rigid(rhs, lhs)) {
            return error.TypesMismatch;
        }
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
                try unify_inner(l_arg, r_arg, subst, visited_map, options);
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
                try unify_inner(term, B_term, subst, visited_map, options);
            }
        },

        .annotation => {
            if (rhs.* != .annotation) {
                return error.TypesMismatch;
            }

            try unify_inner(lhs.child(), rhs.child(), subst, visited_map, options);
        },

        .addr_of => {
            if (rhs.* != .addr_of) return error.TypesMismatch;
            if (rhs.addr_of.mut and !lhs.addr_of.mut) return error.TypesMismatch;
            if (lhs.addr_of.multiptr != rhs.addr_of.multiptr) return error.TypesMismatch;

            try unify_inner(lhs.child(), rhs.child(), subst, visited_map, options);
        },

        .dyn_type => {
            if (rhs.* == .anyptr_type) return;
            if (rhs.* != .dyn_type) return error.TypesMismatch;
            if (rhs.dyn_type.mut and !lhs.dyn_type.mut) return error.TypesMismatch;
            if (lhs.child().symbol() != rhs.child().symbol()) return error.TypesMismatch;
        },

        .unit_type => {
            if (rhs.* != .unit_type) {
                return error.TypesMismatch;
            }
        },

        .function => {
            if (rhs.* != .function) return error.TypesMismatch;
            if (lhs.children().items.len != rhs.children().items.len) {
                return error.TypesMismatch;
            }

            try unify_inner(lhs.rhs(), rhs.rhs(), subst, visited_map, options);
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
                return error.TypesMismatch;
            }

            for (lhs.children().items, rhs.children().items) |l_arg, r_arg| {
                try unify_inner(l_arg, r_arg, subst, visited_map, options);
            }
        },

        else => return, // TODO: Support more types, add error for unsupported constructs
    }
}

pub fn generate_substitutions(_type: *Type_AST, alloc: std.mem.Allocator) Substitutions {
    var subst = Substitutions.init(alloc);
    if (_type.* == .generic_apply) {
        const constructor = _type.lhs();

        const params = constructor.symbol().?.decl.?.generic_params().items;
        const args = _type.children().items;

        std.debug.assert(params.len == args.len);

        errdefer subst.deinit();

        for (params, args) |param, arg| {
            subst.put(param.symbol().?.name, arg) catch unreachable;
        }
    }

    return subst;
}

fn can_unify_rigid(param: *Type_AST, arg: *Type_AST) bool {
    if ((arg.* == .identifier or arg.* == .access) and arg.symbol().?.decl.?.* == .type_param_decl and !arg.symbol().?.decl.?.type_param_decl.rigid) {
        return true;
    }
    const expanded_arg = arg.expand_identifier();
    return param.types_match(expanded_arg);
}

fn lengths_match(as: *const std.array_list.Managed(*Type_AST), bs: *const std.array_list.Managed(*Type_AST)) bool {
    return bs.items.len == as.items.len;
}

pub fn type_param_list_from_subst_map(subst: *const Substitutions, generic_params: std.array_list.Managed(*ast_.AST), alloc: std.mem.Allocator) std.array_list.Managed(*Type_AST) {
    var retval = std.array_list.Managed(*Type_AST).init(alloc);
    for (generic_params.items) |type_param| {
        const with_value = subst.get(type_param.symbol().?.name) orelse continue;
        retval.append(with_value) catch unreachable;
    }
    return retval;
}

pub fn substitution_contains_type_params(subst: *const Substitutions) bool {
    for (subst.keys()) |key| {
        const ty = subst.get(key).?;
        std.debug.assert(ty.* != .identifier or ty.symbol() != null);
        const bad = (ty.* == .identifier) and ty.symbol().?.decl.?.* == .type_param_decl;
        if (bad) {
            return true;
        }
    }
    return false;
}

pub fn substitution_contains_generics(subst: *const Substitutions) bool {
    for (subst.keys()) |key| {
        const ty = subst.get(key).?;
        const bad = ty.is_generic();
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
        const bad = ty.is_generic();
        std.debug.print("    {s}: {?f} ({})\n", .{ key.name, subst.get(key), bad });
    }
    std.debug.print("}}\n", .{});
}
