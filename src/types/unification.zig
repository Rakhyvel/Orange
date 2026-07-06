const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Symbol = @import("../symbol/symbol.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");

pub const Substitutions = struct {
    type_subst: std.StringArrayHashMap(*Type_AST),
    const_subst: std.StringArrayHashMap(*ast_.AST),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Substitutions {
        return .{
            .type_subst = std.StringArrayHashMap(*Type_AST).init(allocator),
            .const_subst = std.StringArrayHashMap(*ast_.AST).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Substitutions) void {
        self.type_subst.deinit();
        self.const_subst.deinit();
    }

    pub fn put_type(self: *Substitutions, name: []const u8, ty: *Type_AST) !void {
        try self.type_subst.put(name, ty);
    }

    pub fn get_type(self: *const Substitutions, name: []const u8) ?*Type_AST {
        return self.type_subst.get(name);
    }

    pub fn put_const(self: *Substitutions, name: []const u8, val: *ast_.AST) !void {
        try self.const_subst.put(name, val);
    }

    pub fn get_const(self: *const Substitutions, name: []const u8) ?*ast_.AST {
        return self.const_subst.get(name);
    }
};

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
    // Tree_Writer.print(lhs);
    // Tree_Writer.print(rhs);
    // std.debug.print("\n", .{});
    if (visited_map.contains(Pair{ .lhs = lhs, .rhs = rhs })) {
        return;
    }
    try visited_map.put(Pair{ .lhs = lhs, .rhs = rhs }, void{});

    if (lhs.is_ident_type("Void")) {
        return; // Bottom type - vacuously true
    }
    if (lhs.* == .anyptr_type) return;
    if (rhs.* == .anyptr_type) return;

    if (lhs.* == .annotation) return try unify_inner(lhs.child(), rhs, subst, visited_map, options);
    if (rhs.* == .annotation) return try unify_inner(lhs, rhs.child(), subst, visited_map, options);

    // Check if the lhs/rhs are type params whose name does not appear in the substitutions (meaning they're unbound/free type variables)
    const lhs_unbound_tp = (lhs.* == .identifier or lhs.* == .access) and lhs.symbol() != null and lhs.symbol().?.decl.?.* == .type_param_decl and subst.get_type(lhs.symbol().?.name) == null;
    const rhs_unbound_tp = (rhs.* == .identifier or rhs.* == .access) and rhs.symbol() != null and rhs.symbol().?.decl.?.* == .type_param_decl and subst.get_type(rhs.symbol().?.name) == null;
    // Check if lhs/rhs binds the other, as an as-trait
    const lhs_binds_rhs = rhs.* == .as_trait and lhs_unbound_tp and !(rhs.lhs().* == .identifier and rhs.lhs().symbol() == lhs.symbol());
    const rhs_binds_lhs = lhs.* == .as_trait and rhs_unbound_tp and !(lhs.lhs().* == .identifier and lhs.lhs().symbol() == rhs.symbol());
    // If lhs/rhs are as-traits, and we don't have a cycle, attempt to unify the inner lhs of the as-trait
    if (lhs.* == .as_trait and !rhs_binds_lhs) return try unify_inner(lhs.lhs(), rhs, subst, visited_map, options);
    if (rhs.* == .as_trait and !lhs_binds_rhs) return try unify_inner(lhs, rhs.lhs(), subst, visited_map, options);

    if (lhs.* == .identifier and lhs.symbol() != null and subst.get_type(lhs.symbol().?.name) != null) {
        if (lhs.symbol().?.decl.?.* != .type_param_decl or !lhs.symbol().?.decl.?.type_param_decl.rigid) {
            const sub = subst.get_type(lhs.symbol().?.name).?;
            return try unify_inner(sub, rhs, subst, visited_map, options);
        }
    }
    if (rhs.* == .identifier and rhs.symbol() != null and subst.get_type(rhs.symbol().?.name) != null) {
        const sub = subst.get_type(rhs.symbol().?.name).?;
        if (rhs.symbol().?.decl.?.* != .type_param_decl or !rhs.symbol().?.decl.?.type_param_decl.rigid) {
            return try unify_inner(lhs, sub, subst, visited_map, options);
        }
    }

    const lhs_is_type_param = (lhs.* == .identifier or lhs.* == .access) and lhs.symbol() != null and lhs.symbol().?.decl.?.* == .type_param_decl;
    const rhs_is_type_param = (rhs.* == .identifier or rhs.* == .access) and rhs.symbol() != null and rhs.symbol().?.decl.?.* == .type_param_decl;

    // Bind a type param to an extern type nominally rather than expanding it to the structural
    // form, so its C name survives into codegen and matches the array element representation
    const lhs_is_extern = lhs.* == .identifier and lhs.symbol() != null and lhs.symbol().?.storage == .@"extern";
    const rhs_is_extern = rhs.* == .identifier and rhs.symbol() != null and rhs.symbol().?.storage == .@"extern";

    if (lhs.* == .identifier and lhs.symbol() != null and lhs.symbol().?.init_typedef() != null and !(lhs_is_extern and rhs_is_type_param)) {
        return try unify_inner(lhs.symbol().?.init_typedef().?, rhs, subst, visited_map, options);
    } else if (rhs.* == .identifier and rhs.symbol() != null and rhs.symbol().?.init_typedef() != null and !(rhs_is_extern and lhs_is_type_param)) {
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

    if (lhs_is_type_param) {
        if (!options.allow_rigid and lhs.symbol().?.decl.?.type_param_decl.rigid and !can_unify_rigid(lhs, rhs)) {
            return error.TypesMismatch;
        }
        try subst.put_type(lhs.symbol().?.name, rhs);
        if (!options.allow_rigid and lhs.symbol().?.decl.?.type_param_decl.rigid and !can_unify_rigid(lhs, rhs)) {
            return error.TypesMismatch;
        }
        return;
    }
    if (rhs_is_type_param) {
        if (!options.allow_rigid and rhs.symbol().?.decl.?.type_param_decl.rigid and !can_unify_rigid(rhs, lhs)) {
            return error.TypesMismatch;
        }
        try subst.put_type(rhs.symbol().?.name, lhs);
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
        .tuple_type => {
            if (rhs.* != .tuple_type) {
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
            if (rhs.* == .anyptr_type) return;
            if (rhs.* != .addr_of) return error.TypesMismatch;
            if (rhs.addr_of.mut and !lhs.addr_of.mut) return error.TypesMismatch;
            if (lhs.addr_of.multiptr != rhs.addr_of.multiptr) return error.TypesMismatch;

            try unify_inner(lhs.child(), rhs.child(), subst, visited_map, options);
        },

        .array_of => {
            if (rhs.* != .array_of) return error.TypesMismatch;
            try unify_inner(lhs.child(), rhs.child(), subst, visited_map, options);
            const l_len = lhs.array_of.len;
            const r_len = rhs.array_of.len;
            const l_is_param = l_len.is_const_param_ref();
            const r_is_param = r_len.is_const_param_ref();
            if (l_is_param) {
                try subst.put_const(l_len.token().data, r_len);
            } else if (r_is_param) {
                if (!options.allow_rigid) return error.TypesMismatch;
                try subst.put_const(r_len.token().data, l_len);
            } else {
                if (l_len.int.data != r_len.int.data) return error.TypesMismatch;
            }
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
            if (rhs.* != .generic_apply) return error.TypesMismatch;
            std.debug.assert(lhs.lhs().symbol() != null);
            std.debug.assert(rhs.lhs().symbol() != null);
            if (lhs.lhs().symbol() != rhs.lhs().symbol()) return error.TypesMismatch;
            const l_args = lhs.generic_apply.args.items;
            const r_args = rhs.generic_apply.args.items;
            if (l_args.len != r_args.len) return error.TypesMismatch;
            for (l_args, r_args) |l_arg, r_arg| {
                if (std.meta.activeTag(l_arg) != std.meta.activeTag(r_arg)) return error.TypesMismatch;
                switch (l_arg) {
                    .type_arg => |l_ty| try unify_inner(l_ty, r_arg.type_arg, subst, visited_map, options),
                    .const_arg => |l_v| {
                        const r_v = r_arg.const_arg;
                        if (l_v.is_const_param_ref()) {
                            try subst.put_const(l_v.token().data, r_v);
                        } else if (r_v.is_const_param_ref()) {
                            if (!options.allow_rigid) return error.TypesMismatch;
                            try subst.put_const(r_v.token().data, l_v);
                        } else {
                            if (l_v.int.data != r_v.int.data) return error.TypesMismatch;
                        }
                    },
                }
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
        const args = _type.generic_apply.args.items;
        std.debug.assert(params.len == args.len);
        errdefer subst.deinit();
        for (params, args) |param, arg| {
            switch (param.*) {
                .type_param_decl => subst.put_type(param.symbol().?.name, arg.type_arg) catch unreachable,
                .const_param_decl => subst.put_const(param.symbol().?.name, arg.const_arg) catch unreachable,
                else => unreachable,
            }
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
        if (type_param.* != .type_param_decl) continue;
        const with_value = subst.get_type(type_param.symbol().?.name) orelse continue;
        retval.append(with_value) catch unreachable;
    }
    return retval;
}

pub fn generic_arg_list_from_subst_map(subst: *const Substitutions, generic_params: std.array_list.Managed(*ast_.AST), alloc: std.mem.Allocator) std.array_list.Managed(ast_.GenericArg) {
    var retval = std.array_list.Managed(ast_.GenericArg).init(alloc);
    for (generic_params.items) |param| {
        switch (param.*) {
            .type_param_decl => {
                const with_value = subst.get_type(param.symbol().?.name) orelse continue;
                retval.append(.{ .type_arg = with_value }) catch unreachable;
            },
            .const_param_decl => {
                const with_value = subst.get_const(param.symbol().?.name) orelse continue;
                retval.append(.{ .const_arg = with_value }) catch unreachable;
            },
            else => {},
        }
    }
    return retval;
}

pub fn substitution_contains_type_params(subst: *const Substitutions) bool {
    for (subst.type_subst.keys()) |key| {
        const ty = subst.get_type(key).?;
        std.debug.assert(ty.* != .identifier or ty.symbol() != null);
        const bad = (ty.* == .identifier) and ty.symbol().?.decl.?.* == .type_param_decl;
        if (bad) {
            return true;
        }
    }
    return false;
}

pub fn substitution_contains_generics(subst: *const Substitutions) bool {
    for (subst.type_subst.keys()) |key| {
        const ty = subst.get_type(key).?;
        const bad = ty.is_generic();
        if (bad) {
            return true;
        }
    }
    // A const arg that is not a concrete literal (like an unresolved const param `n`) is still generic
    for (subst.const_subst.keys()) |key| {
        if (!const_arg_is_concrete(subst.get_const(key).?)) {
            return true;
        }
    }
    return false;
}

/// Whether a const arg has been resolved to a concrete comptime literal, as opposed to still being
/// an unresolved const param reference
pub fn const_arg_is_concrete(arg: *ast_.AST) bool {
    return switch (arg.*) {
        .int, .float, .true, .false => true,
        else => false,
    };
}

pub fn print_substitutions(subst: *const Substitutions) void {
    std.debug.print("{} substitutions: {{\n", .{subst.type_subst.keys().len});
    for (subst.type_subst.keys()) |key| {
        const ty = subst.get_type(key).?;
        const bad = ty.is_generic();
        std.debug.print("    {s}: {?f} ({})\n", .{ key, subst.get_type(key), bad });
    }
    for (subst.const_subst.keys()) |key| {
        std.debug.print("    {s}: {?f}\n", .{ key, subst.get_const(key) });
    }
    std.debug.print("}}\n", .{});
}
