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
    }

    return self;
}
