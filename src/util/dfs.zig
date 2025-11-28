const std = @import("std");

pub fn Dfs_Iterator(comptime T: type) type {
    return struct {
        const Self = @This();
        current: ?T,

        stack: std.array_list.Managed(T),
        visited: std.AutoHashMap(T, void),

        pub fn init(start: T, allocator: std.mem.Allocator) Self {
            return Self{
                .current = start,
                .stack = std.array_list.Managed(T).init(allocator),
                .visited = std.AutoHashMap(T, void).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.stack.deinit();
            self.visited.deinit();
        }

        pub fn next(self: *Self) ?T {
            // If we're out of values, then we're done
            if (self.current == null) return null;

            // Our result is our current value
            const result = self.current.?;
            self.visited.put(result, {}) catch unreachable;

            // Add all adjacent edges to the stack.
            // We do a visited check here to avoid revisiting vertices
            const adjacent: []T = self.current.?.get_adjacent();
            for (adjacent) |target| {
                if (!self.visited.contains(target)) {
                    self.stack.append(target) catch unreachable;
                }
            }

            // If the type allows us to free the adjacency slice, free it.
            if (@typeInfo(T) == .@"struct" and @hasDecl(T, "free_adjacent")) {
                self.current.?.free_adjacent(adjacent);
                // Side note: this sucks! If only zig had traits!
            } else if (@typeInfo(T) == .pointer and @typeInfo(@typeInfo(T).pointer.child) == .@"struct" and @hasDecl(@typeInfo(T).pointer.child, "free_adjacent")) {
                self.current.?.free_adjacent(adjacent);
            }

            // Advance to the next value
            self.current = null;
            while (self.stack.pop()) |next_val| {
                if (!self.visited.contains(next_val)) {
                    self.current = next_val;
                    break;
                }
            }

            return result;
        }
    };
}
