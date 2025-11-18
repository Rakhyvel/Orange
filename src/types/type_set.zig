//! This file defines type sets and directed acyclic graphs (Dependency_Nodes) for types. The type set is used
//! to store types based on their C-equivalence.

const std = @import("std");
const Dependency_Node = @import("dependency_node.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Canonical_Type_Fmt = @import("../types/canonical_type_fmt.zig");

const Self = @This();

types: std.array_list.Managed(*Dependency_Node),
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{ .types = std.array_list.Managed(*Dependency_Node).init(allocator), .allocator = allocator };
}

pub fn deinit(self: *Self) void {
    self.types.deinit();
}

/// Adds a type to the type set. Returns the dependency node for the corresponding type
pub fn add_type(self: *Self, oldast_: *Type_AST) ?*Dependency_Node {
    return self.add_internal(oldast_, false);
}

fn add_internal(self: *Self, oldast_: *Type_AST, from_function: bool) ?*Dependency_Node {
    const ast = oldast_.expand_identifier();
    // var str = Canonical_Type_Fmt.canonical_rep(ast) catch unreachable;
    // defer str.deinit();
    // std.debug.print("{f}: {s}\n", .{ ast, str.str() });
    // std.debug.assert(ast.* != .identifier or ast.symbol().?.decl.?.* != .type_param_decl);

    if (self.get_node(ast)) |dag| {
        // Type is already in the set, return Dependency_Node entry for it
        // std.debug.print("already in dag: {f}\n", .{dag.base});
        return dag;
    }

    // Type wasn't in the set, so we'll need to create a Dependency_Node for it and its children
    switch (ast.*) {
        .function => return self.add_function(ast),
        .struct_type, .tuple_type, .enum_type, .untagged_sum_type, .context_type => return self.add_aggregate(ast),
        .dyn_type => return self.add_dependency_node(ast),
        .addr_of => {
            const retval = self.add_type(ast.child());
            if (from_function) {
                return retval;
            } else {
                return null;
            }
        },
        .array_of => return self.add_array(ast),
        .identifier, .unit_type, .anyptr_type => return null, // Do not add to Dependency_Node
        else => std.debug.panic("error: cannot add unknown typekind `{t}` to set", .{ast.*}),
    }
}

/// Adds a function type to the type set
fn add_function(self: *Self, function_type_ast: *Type_AST) ?*Dependency_Node {
    var dag = self.add_dependency_node(function_type_ast);
    for (function_type_ast.function.args.items) |arg| {
        if (self.add_internal(arg, true)) |domain| {
            dag.add_dependency(domain);
        }
    }
    if (self.add_type(function_type_ast.rhs())) |codomain| {
        dag.add_dependency(codomain);
    }
    for (function_type_ast.function.contexts.items) |ctx| {
        if (self.add_type(ctx.child())) |context_node| {
            dag.add_dependency(context_node);
        }
    }
    return dag;
}

/// Adds an aggregate type (product, sum) to the type set
fn add_aggregate(self: *Self, aggregate_ast: *Type_AST) ?*Dependency_Node {
    var dag = self.add_dependency_node(aggregate_ast);
    for (aggregate_ast.children().items) |term| {
        if (self.add_type(term)) |dependency| {
            if (term.* != .addr_of)
                dag.add_dependency(dependency);
        }
    }
    return dag;
}

/// Adds a function type to the type set
fn add_array(self: *Self, array_type_ast: *Type_AST) ?*Dependency_Node {
    var dag = self.add_dependency_node(array_type_ast);
    if (self.add_type(array_type_ast.child())) |domain| {
        dag.add_dependency(domain);
    }
    return dag;
}

/// Performs A = A U B
pub fn union_from(self: *Self, other: *const Self) void {
    for (other.types.items) |_type| {
        _ = self.add_type(_type.base);
    }
}

/// Retrieves the dependency node from the type set given a type
pub fn get_node(self: *const Self, oldast_: *Type_AST) ?*Dependency_Node {
    const ast = oldast_.expand_identifier();
    for (self.types.items) |dag| {
        if (dag.base.c_types_match(ast)) {
            return dag;
        }
    }
    return null;
}

/// Debug function to print out the type set
pub fn print(self: *Self) void {
    for (self.types.items) |dag| {
        dag.print();
    }
}

fn add_dependency_node(self: *Self, ast: *Type_AST) *Dependency_Node {
    const dag = Dependency_Node.init(ast, self.types.items.len, self.allocator);
    self.types.append(dag) catch unreachable;
    return dag;
}

pub fn len(self: *const Self) usize {
    return self.types.items.len;
}

/// Prints out the graphviz representation for the type set
pub fn graphviz(self: *const Self) void {
    for (self.types.items) |dep| {
        dep.graphviz();
    }
}
