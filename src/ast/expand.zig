//! This file represents a struct for an AST walk, and walks over the AST and performs simple, local expansions on
//! it.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const core_ = @import("../hierarchy/core.zig");
const errs_ = @import("../util/errors.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Token = @import("../lexer/token.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
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
        .range => try self.expand_range(ast),
    }

    return self;
}

fn expand_range(self: Self, ast: *ast_.AST) !void {
    var terms = std.array_list.Managed(*ast_.AST).init(self.allocator);
    try terms.append(ast.range.lower);
    try terms.append(ast.range.upper);

    const core_ident = Type_AST.create_type_identifier(Token.init_simple("core"), self.allocator);
    const eq_trait_field = Type_AST.create_field(Token.init_simple("Range"), self.allocator);
    const range_type = Type_AST.create_type_access(ast.token(), core_ident, eq_trait_field, self.allocator);

    ast.* = ast_.AST.create_struct_value(ast.token(), range_type, terms, self.allocator).*;
}
