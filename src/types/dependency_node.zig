//! This file defines the type depenency node struct, which is used to represent a type in a type set. The
//! Dependency_Node is used to store the dependencies between types, and is used to generate a topological
//! sort of the types.

const std = @import("std");
const Type_AST = @import("../types/type.zig").Type_AST;
const Canonical_Type_Fmt = @import("../types/canonical_type_fmt.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");

const Self = @This();

base: *Type_AST,
uid: usize,
dependencies: std.array_list.Managed(*Self),
visited: bool,

pub fn init(base: *Type_AST, uid: usize, allocator: std.mem.Allocator) *Self {
    var retval = allocator.create(Self) catch unreachable;
    retval.base = base;
    retval.uid = uid;
    retval.dependencies = std.array_list.Managed(*Self).init(allocator);
    retval.visited = false;
    return retval;
}

pub fn add_dependency(self: *Self, dependency: *Self) void {
    self.dependencies.append(dependency) catch unreachable;
}

pub fn mark_visited(self: *Self) void {
    self.visited = true;
}

// kcov-ignore-start
pub fn print(self: *Self) void {
    std.debug.print("{} <= ", .{self.uid});
    for (self.dependencies.items) |dep| {
        std.debug.print("{} ", .{dep.uid});
    }
    std.debug.print("\n", .{});
    Tree_Writer.print(self.base);
}

/// Prints out a graphviz representation of the dependency node
pub fn graphviz(self: *Self) void {
    for (self.dependencies.items) |dep| {
        std.debug.print("\"{f}\" {f} -> \"{f}\" {f};\n", .{ self.base, dep.base, Canonical_Type_Fmt{ .type = self.base }, Canonical_Type_Fmt{ .type = dep.base } });
    }
}
// kcov-ignore-end
