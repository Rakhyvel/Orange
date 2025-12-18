//! This file represents a struct for an AST walk, and is used to decorate ASTs with their types.

const std = @import("std");
const errs_ = @import("../util/errors.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const walk_ = @import("../ast/walker.zig");
const unification_ = @import("../types/unification.zig");

ctx: *Compiler_Context,

const Self = @This();

pub fn new(ctx: *Compiler_Context) Self {
    return Self{ .ctx = ctx };
}

pub fn postfix_type(self: Self, _type: *Type_AST) walk_.Error!void {
    const depth_limit: usize = 100;
    var depth: usize = 0;
    while (_type.* == .type_of or _type.* == .domain_of or _type.* == .index) {
        if (depth > depth_limit) {
            self.ctx.errors.add_error(errs_.Error{ .basic = .{ .msg = "recursive type detected", .span = _type.token().span } });
            return error.CompileError;
        }
        switch (_type.*) {
            .type_of => {
                var subst = unification_.Sym_Substitutions.init(self.ctx.allocator());
                defer subst.deinit();
                const typeof_expr = self.ctx.typecheck.typecheck_AST(_type.type_of._expr, null, &subst) catch |e| switch (e) {
                    error.UnexpectedTypeType => {
                        self.ctx.errors.add_error(errs_.Error{ .unexpected_type_type = .{ .expected = null, .span = _type.type_of._expr.token().span } });
                        return error.CompileError;
                    },
                    error.CompileError => return error.CompileError,
                    error.OutOfMemory => return error.OutOfMemory,
                };
                _type.* = typeof_expr.*;
            },
            .domain_of => {
                var child = _type.domain_of._child;
                try self.postfix_type(child);
                const domain_ptr = child.expand_identifier().enum_type.get_ctor(_type.domain_of.ctor_name) orelse std.debug.panic("{s} not in {f}\n", .{ _type.domain_of.ctor_name, child });
                _type.* = domain_ptr.*;
            },
            .index => {
                var child = _type.child();
                try self.postfix_type(child);
                child = child.expand_identifier();
                if (child.* == .array_of) {
                    _type.* = child.child().*;
                } else if (child.* == .tuple_type) {
                    if (_type.index.idx.int.data >= child.children().items.len) {
                        break;
                    }
                    _type.* = child.children().items[@intCast(_type.index.idx.int.data)].*;
                } else {
                    _type.* = Type_AST.create_poisoned_type(_type.token(), self.ctx.allocator()).*;
                }
            },
            else => unreachable,
        }
        depth += 1;
    }
}
