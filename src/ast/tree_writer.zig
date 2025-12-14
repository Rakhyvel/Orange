//! This file represents a struct for an AST walk, and expands complex import pattern ASTs into simpler forms which
//! are easier to work with.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const walker_ = @import("../ast/walker.zig");

indent: usize,

const Self = @This();

pub fn new(indent: usize) Self {
    return Self{ .indent = indent };
}

pub fn print_tree(ast: *ast_.AST) void {
    walker_.walk_ast(ast, new(0)) catch unreachable;
}

pub fn print_type_tree(_type: *Type_AST) void {
    walker_.walk_type(_type, new(0)) catch unreachable;
}

pub fn prefix(self: Self, ast: *ast_.AST) walker_.Error!?Self {
    return self.tree_writer_prefix(ast);
}

pub fn prefix_type(self: Self, _type: *Type_AST) walker_.Error!?Self {
    return self.tree_writer_prefix_type(_type);
}

/// Converts imports to constant declarations
fn tree_writer_prefix(self: Self, ast: *ast_.AST) walker_.Error!?Self {
    for (0..self.indent) |_| {
        std.debug.print(" ", .{});
    }
    std.debug.print("{t}(", .{ast.*});
    switch (ast.*) {
        else => {},
        .access => std.debug.print(".symbol={?s}@{?}", .{
            if (ast.symbol()) |sym| sym.name else null,
            if (ast.symbol()) |sym| sym.scope.uid else null,
        }),
        .identifier => std.debug.print(".data={s}, .symbol={?s}@{?}", .{
            ast.token().data,
            if (ast.symbol()) |sym| sym.name else null,
            if (ast.symbol()) |sym| sym.scope.uid else null,
        }),
        .pattern_symbol => std.debug.print(".data={s}, .symbol={?s}@{?}", .{
            ast.token().data,
            if (ast.symbol()) |sym| sym.name else null,
            if (ast.symbol()) |sym| sym.scope.uid else null,
        }),
        .fn_decl => std.debug.print(".name={?f}", .{ast.fn_decl.name}),
        .method_decl => std.debug.print(".name={f}, .symbol={?*}", .{ ast.method_decl.name, if (ast.symbol()) |sym| sym else null }),
        .binding => std.debug.print(".pattern={f}, .type={f}, .init={?f}", .{ ast.binding.pattern, ast.binding.type, ast.binding.init }),
        .field => std.debug.print(".data={s}", .{ast.token().data}),
    }
    std.debug.print(")\n", .{});
    return Self.new(self.indent + 4);
}

/// Converts imports to constant declarations
fn tree_writer_prefix_type(self: Self, _type: *Type_AST) walker_.Error!?Self {
    for (0..self.indent) |_| {
        std.debug.print(" ", .{});
    }
    std.debug.print(":{t}(", .{_type.*});
    switch (_type.*) {
        else => {},
        .identifier => std.debug.print(".data={s}, .symbol={?s}@{?}", .{
            _type.token().data,
            if (_type.symbol()) |sym| sym.name else null,
            if (_type.symbol()) |sym| sym.scope.uid else null,
        }),
        .annotation => std.debug.print(".name={f}", .{_type.annotation.pattern}),
        .field => std.debug.print(".data={s}", .{_type.token().data}),
    }
    std.debug.print(")\n", .{});
    return Self.new(self.indent + 4);
}
