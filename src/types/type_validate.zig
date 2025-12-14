//! This file contains the semantic validation logic for types.
const std = @import("std");
const Const_Eval = @import("../semantic/const_eval.zig");
const Type_Decorate = @import("../ast/type_decorate.zig");
const generic_apply_ = @import("../ast/generic_apply.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const Type_AST = @import("type.zig").Type_AST;
const Symbol = @import("../symbol/symbol.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const walk_ = @import("../ast/walker.zig");
const unification_ = @import("../types/unification.zig");

const Validate_Error_Enum = error{ OutOfMemory, CompileError };
const Self: type = @This();

ctx: *Compiler_Context,
visited: std.AutoArrayHashMap(*Type_AST, bool),

pub fn init(ctx: *Compiler_Context) Self {
    return Self{ .ctx = ctx, .visited = std.AutoArrayHashMap(*Type_AST, bool).init(ctx.allocator()) };
}

pub fn validate_type(self: *Self, @"type": *Type_AST) Validate_Error_Enum!void {
    if (self.visited.contains(@"type")) return;
    try self.visited.put(@"type", true);

    switch (@"type".*) {
        .generic_apply => {
            try self.validate_type(@"type".lhs());
            try generic_apply_.instantiate(@"type", self.ctx);
        },

        .identifier => {
            const type_symbol = @"type".symbol().?;
            if (!(type_symbol.is_type() or type_symbol.kind == .context)) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .msg = "expected a type", .span = @"type".token().span } });
                return error.CompileError;
            }
        },

        .access => {
            const type_symbol = @"type".symbol().?;
            if (type_symbol.init_typedef()) |typ| {
                try self.validate_type(typ);
            } else if (@"type".lhs().symbol().?.decl.?.* == .type_param_decl) {
                if (@"type".associated_type_from_constraint()) |assoc_type| try self.validate_type(assoc_type);
            }
        },

        .array_of => {
            var subst = unification_.Substitutions.init(self.ctx.allocator());
            defer subst.deinit();
            _ = self.ctx.typecheck.typecheck_AST(@"type".array_of.len, prelude_.int_type, &subst) catch return error.CompileError;
            try walk_.walk_ast(@"type".array_of.len, Const_Eval.new(self.ctx));
            if (@"type".array_of.len.* != .int) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = @"type".token().span, .msg = "not a constant integer" } });
                return error.CompileError;
            }
            try self.validate_type(@"type".child());
        },

        .untagged_sum_type => {
            if (@"type".child().expand_identifier().* != .enum_type) { // TODO: What if the identifier is cyclic?
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = @"type".token().span, .msg = "not an enum type" } });
                return error.CompileError;
            }
            try self.validate_type(@"type".child());
        },

        .dyn_type => {
            if ((@"type".child().* != .identifier and @"type".child().* != .access) or @"type".child().symbol().?.kind != .trait) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = @"type".child().token().span, .msg = "not a trait" } });
                return error.CompileError;
            }
        },

        .type_of => {
            try walk_.walk_type(@"type", Type_Decorate.new(self.ctx));
        },

        .domain_of => {
            try self.validate_type(@"type".child());
            try walk_.walk_type(@"type", Type_Decorate.new(self.ctx));
        },

        .annotation,
        .addr_of,
        .index,
        => {
            try self.validate_type(@"type".child());
        },

        .function => {
            for (@"type".function.args.items) |arg| {
                try self.validate_type(arg);
            }
            try self.validate_type(@"type".rhs());
        },

        .enum_type,
        .struct_type,
        .tuple_type,
        => {
            for (@"type".children().items) |child| {
                try self.validate_type(child);
            }
        },

        else => {},
    }
}

pub fn detect_cycle(self: *Self, ty: *Type_AST, append_me: ?*Symbol) bool {
    _ = append_me;
    var visiting = std.AutoArrayHashMap(*Type_AST, bool).init(self.ctx.allocator());
    defer visiting.deinit();

    var visited = std.AutoArrayHashMap(*Type_AST, bool).init(self.ctx.allocator());
    defer visited.deinit();

    var path = std.AutoArrayHashMap(*Symbol, bool).init(self.ctx.allocator());
    defer path.deinit();

    return dfs(ty, &visiting, &path, &visited, false);
}

fn dfs(
    ty: *Type_AST,
    visiting: *std.AutoArrayHashMap(*Type_AST, bool),
    path: *std.AutoArrayHashMap(*Symbol, bool),
    visited: *std.AutoArrayHashMap(*Type_AST, bool),
    has_indirection: bool,
) bool {
    if (visited.contains(ty)) return false;

    if (visiting.contains(ty) and (ty.* == .identifier or ty.* == .access or ty.* == .generic_apply) and ty.symbol().?.decl.?.* == .type_alias) return true; // unsafe cycle detected
    if (visiting.contains(ty) and (ty.* == .identifier or ty.* == .access or ty.* == .generic_apply)) return !has_indirection; // unsafe cycle detected

    visiting.put(ty, true) catch unreachable;

    switch (ty.*) {
        .identifier, .access, .generic_apply => if (ty.symbol()) |sym| {
            if (path.contains(sym)) {
                if (sym.decl.?.* == .type_alias) {
                    return true; // invalid alias cycle
                }
            } else {
                path.put(sym, true) catch unreachable;
                if (ty.symbol().?.init_typedef()) |inner| {
                    if (dfs(inner, visiting, path, visited, has_indirection)) return true;
                }
                _ = path.swapRemove(sym);
            }
        },
        .struct_type, .tuple_type, .enum_type => {
            for (ty.children().items) |child| {
                if (dfs(child, visiting, path, visited, has_indirection)) return true;
            }
        },
        .array_of, .index, .addr_of, .annotation => {
            if (dfs(ty.child(), visiting, path, visited, has_indirection or ty.* == .addr_of)) return true;
        },
        else => {},
    }

    _ = visiting.swapRemove(ty);
    visited.put(ty, true) catch unreachable;
    return false;
}
