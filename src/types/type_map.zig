const std = @import("std");
const AST = @import("../ast/ast.zig").AST;
const Type_AST = @import("../types/type.zig").Type_AST;
const generic_arg_ = @import("../ast/generic_arg.zig");
const GenericArg = generic_arg_.GenericArg;

pub fn Type_Map(comptime Value: type) type {
    return Linear_Map(*Type_AST, Value, Type_AST.types_match);
}

/// A type map using C type equivelence
pub fn C_Type_Map(comptime Value: type) type {
    return Linear_Map(*Type_AST, Value, Type_AST.c_types_match);
}

/// A type map using an arraylist of GenericArgs as a key
pub fn Monomorph_Map(comptime Value: type) type {
    return Linear_Map(std.array_list.Managed(GenericArg), Value, generic_args_match);
}

/// TODO: This could be moved out to util
/// A map data structure that stores its key-value pairs linearly in an arraylist, with get and put operations
/// defined based on an equivalence function.
pub fn Linear_Map(comptime Key: type, comptime Value: type, comptime eq: fn (Key, Key) bool) type {
    return struct {
        /// A key-value pair
        const Pair = struct {
            key: Key,
            value: Value,
        };

        const Self = @This();

        /// List of pairs of the map
        pairs: std.array_list.Managed(Pair),

        /// Initialize the map with an allocator
        pub fn init(alloc: std.mem.Allocator) Self {
            return .{
                .pairs = std.array_list.Managed(Pair).init(alloc),
            };
        }

        /// Deinitializes the map
        pub fn deinit(self: *Self) void {
            self.pairs.deinit();
        }

        /// Returns the value corresponding to the given key, or null
        pub fn get(self: *const Self, key: Key) ?Value {
            for (self.pairs.items) |pair| {
                if (eq(pair.key, key)) {
                    return pair.value;
                }
            }
            return null;
        }

        /// Determines if the map contains the given key
        pub fn contains(self: *const Self, key: Key) bool {
            return self.get(key) != null;
        }

        /// Adds the key value pair if the map does not contain the key, does nothing if the map does contain the key
        pub fn put(self: *Self, key: Key, value: Value) !void {
            if (self.get(key)) |_| {
                return;
            }

            try self.pairs.append(.{ .key = key, .value = value });
        }

        /// Adds many keys to the same value
        pub fn put_many(self: *Self, keys: []const Key, value: Value) !void {
            for (keys) |key| {
                try self.put(key, value);
            }
        }
    };
}

pub fn generic_args_match(lhs: std.array_list.Managed(GenericArg), rhs: std.array_list.Managed(GenericArg)) bool {
    const unification_ = @import("../types/unification.zig");
    if (lhs.items.len != rhs.items.len) return false;

    // Same monomorph only if unifying the two keys needs no param bindings
    var subst = unification_.Substitutions.init(std.heap.page_allocator);
    defer subst.deinit();

    for (lhs.items, rhs.items) |l, r| {
        switch (l) {
            .type_arg => |lt| switch (r) {
                .type_arg => |rt| unification_.unify(rt, lt, &subst, .{ .strict_assoc = true }) catch return false,
                .const_arg => return false,
            },
            .const_arg => |lv| switch (r) {
                .type_arg => return false,
                .const_arg => |rv| {
                    if (!const_args_equal(lv, rv)) return false;
                },
            },
        }
    }

    // Ignore identity self-bindings, any binding to a different type means distinct monomorphs
    var type_it = subst.type_subst.iterator();
    while (type_it.next()) |entry| {
        const val = entry.value_ptr.*;
        if (!(val.* == .identifier and val.symbol() == entry.key_ptr.*)) return false;
    }
    var const_it = subst.const_subst.iterator();
    while (const_it.next()) |entry| {
        const val = entry.value_ptr.*;
        if (!(val.* == .identifier and val.symbol() == entry.key_ptr.*)) return false;
    }

    return true;
}

fn const_args_equal(a: *AST, b: *AST) bool {
    if (@intFromEnum(a.*) != @intFromEnum(b.*)) return false;
    return switch (a.*) {
        .int => a.int.data == b.int.data,
        .float => a.float.data == b.float.data,
        .true, .false => @intFromEnum(a.*) == @intFromEnum(b.*),
        else => false,
    };
}

pub fn type_lists_match(lhs: std.array_list.Managed(*Type_AST), rhs: std.array_list.Managed(*Type_AST)) bool {
    const unification_ = @import("../types/unification.zig");
    if (lhs.items.len != rhs.items.len) {
        return false;
    }

    for (lhs.items, rhs.items) |lhs_item, rhs_item| {
        var subst = unification_.Substitutions.init(std.heap.page_allocator);
        defer subst.deinit();
        unification_.unify(lhs_item, rhs_item, &subst, .{}) catch return false;
    }

    return true;
}
