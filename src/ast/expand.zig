//! This file represents a struct for an AST walk, and walks over the AST and performs simple, local expansions on
//! it.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const core_ = @import("../hierarchy/core.zig");
const errs_ = @import("../util/errors.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Token = @import("../lexer/token.zig");
const walk_ = @import("../ast/walker.zig");

errors: *errs_.Errors,
allocator: std.mem.Allocator,

const Self = @This();

pub fn new(errors: *errs_.Errors, allocator: std.mem.Allocator) Self {
    return Self{
        .errors = errors,
        .allocator = allocator,
    };
}

pub fn prefix(self: Self, ast: *ast_.AST) walk_.Error!?Self {
    return self.expand_prefix(ast);
}

/// Expand ASTs before descending to further children
fn expand_prefix(self: Self, ast: *ast_.AST) walk_.Error!?Self {
    switch (ast.*) {
        else => {},

        // TOOD: A pass that ensures that there are no duplicate fields in product and sum types

        .sub_slice => self.expand_subslice(ast),
        .range => try self.expand_range(ast),
    }

    return self;
}

fn expand_subslice(self: Self, ast: *ast_.AST) void {
    if (ast.sub_slice.lower == null) {
        ast.sub_slice.lower = ast_.AST.create_int(ast.token(), 0, self.allocator);
    }
    if (ast.sub_slice.upper == null) {
        const length = ast_.AST.create_field(Token.init("length", null, "", "", 0, 0), self.allocator);
        ast.sub_slice.upper = ast_.AST.create_select(
            ast.token(),
            ast.sub_slice.super,
            length,
            self.allocator,
        );
    }
}

fn expand_range(self: Self, ast: *ast_.AST) !void {
    var terms = std.array_list.Managed(*ast_.AST).init(self.allocator);
    try terms.append(ast.range.lower);
    try terms.append(ast.range.upper);
    ast.* = ast_.AST.create_struct_value(ast.token(), core_.range_type, terms, self.allocator).*;
}
