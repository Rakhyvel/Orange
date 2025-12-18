//! This file contains various semantic checks relating to pattern-matching.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const defaults_ = @import("defaults.zig");
const errs_ = @import("../util/errors.zig");
const Span = @import("../util/span.zig");
const typing_ = @import("typing.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const unification_ = @import("../types/unification.zig");

const Validate_Error_Enum = error{CompileError};

const Self: type = @This();

ctx: *Compiler_Context,
map: std.AutoArrayHashMap(*ast_.AST, *Type_AST),

pub fn init(ctx: *Compiler_Context) Self {
    return Self{
        .ctx = ctx,
        .map = std.AutoArrayHashMap(*ast_.AST, *Type_AST).init(ctx.allocator()),
    };
}

/// Validates that `pattern` is valid given a match's `expr`
pub fn assert_pattern_matches(
    self: *Self,
    pattern: *ast_.AST,
    expr_type: *Type_AST,
    subst: *unification_.Sym_Substitutions,
) Validate_Error_Enum!void {
    switch (pattern.*) {
        .unit_value,
        .int,
        .string,
        .float,
        .true,
        .false,
        .block,
        .select,
        => _ = self.ctx.typecheck.typecheck_AST(pattern, expr_type, subst) catch return error.CompileError,

        .enum_value => {
            if (pattern.enum_value.base == null) {
                // Infer that the base type is `expected`
                pattern.enum_value.base = expr_type;
            }

            const expanded_base: *Type_AST = pattern.enum_value.base.?.expand_identifier();
            if (expanded_base.* != .enum_type) {
                return typing_.throw_wrong_from("enum", "enum value", expanded_base, pattern.token().span, &self.ctx.errors);
            }

            const pos = expanded_base.get_pos(pattern.token().data);
            if (pos == null and expanded_base.* == .enum_type) {
                self.ctx.errors.add_error(errs_.Error{ .member_not_in_type = .{ .span = pattern.token().span, .identifier = pattern.token().data, .name = "enum", .type = expanded_base } });
                return error.CompileError;
            }
            pattern.set_pos(expanded_base.get_pos(pattern.token().data));

            pattern.enum_value.domain = expanded_base.children().items[pattern.pos().?];
            if (pattern.enum_value.init == null) {
                // This may be usurped by a .call node
                pattern.enum_value.init = pattern.enum_value.domain.?.annotation.init orelse
                    try defaults_.generate_default(pattern.enum_value.domain.?.child(), pattern.token().span, &self.ctx.errors, self.ctx.allocator());
            }
            _ = self.assert_pattern_matches(pattern.enum_value.init.?, pattern.enum_value.domain.?.child(), subst) catch return error.CompileError;
        },

        .tuple_value => {
            const expanded_expr_type = expr_type.expand_identifier();
            if (expanded_expr_type.* != .tuple_type or expanded_expr_type.children().items.len != pattern.children().items.len) {
                const got = self.ctx.typecheck.typecheck_AST(pattern, null, subst) catch return error.CompileError;
                return typing_.throw_unexpected_type(pattern.token().span, expr_type, got, &self.ctx.errors);
            }
            for (pattern.children().items, expanded_expr_type.children().items) |term, expanded_term| {
                try self.assert_pattern_matches(term, expanded_term, subst);
            }
        },

        .array_value => {
            const expanded_expr_type = expr_type.expand_identifier();
            if (expanded_expr_type.* != .array_of or expanded_expr_type.array_of.len.int.data != pattern.children().items.len) {
                const got = self.ctx.typecheck.typecheck_AST(pattern, null, subst) catch return error.CompileError;
                return typing_.throw_unexpected_type(pattern.token().span, expr_type, got, &self.ctx.errors);
            }
            const elem_type = expanded_expr_type.child();
            for (pattern.children().items) |term| {
                try self.assert_pattern_matches(term, elem_type, subst);
            }
        },

        .pattern_symbol => {},

        else => std.debug.panic("compiler error: unimplemented assert_pattern_matches() for {s}", .{@tagName(pattern.*)}),
    }
    self.ctx.typecheck.assert_typeof(pattern, expr_type);
    _ = pattern.assert_ast_valid();
}

/// Checks that a match's mappings cover all possible cases
///
/// Currently only checks that all sums are covered.
/// HUGE TODO: Figure out how to do this fr for products
pub fn exhaustive_check(
    self: *Self,
    _type: *Type_AST,
    mappings: *std.array_list.Managed(*ast_.AST),
    match_span: Span,
) Validate_Error_Enum!void {
    if (_type.* == .enum_type) {
        var total_sum_ids = std.array_list.Managed(usize).init(self.ctx.allocator());
        defer total_sum_ids.deinit();

        // Append all sum IDs to the ids list
        for (_type.children().items, 0..) |_, i| {
            total_sum_ids.append(i) catch unreachable;
        }
        // For each sum ID, remove it if it's covered
        // Yeah I know this sucks. Figure out something better!
        for (mappings.items) |m| {
            exhaustive_check_sub(m.lhs(), &total_sum_ids);
        }
        // If there were any IDs that weren't removed, they weren't covered with a pattern match
        if (total_sum_ids.items.len > 0) {
            var forgotten_sum_variants = std.array_list.Managed(*Type_AST).init(self.ctx.allocator()); // Not deallocated, lives until error emission
            for (total_sum_ids.items) |id| {
                forgotten_sum_variants.append(_type.children().items[id]) catch unreachable;
            }
            self.ctx.errors.add_error(errs_.Error{ .non_exhaustive_sum = .{ .span = match_span, .forgotten = forgotten_sum_variants } });
            return error.CompileError;
        }
    }
}

fn exhaustive_check_sub(ast: *ast_.AST, ids: *std.array_list.Managed(usize)) void {
    switch (ast.*) {
        .select, .enum_value => {
            for (ids.items, 0..) |item, i| {
                if (item == ast.pos().?) {
                    _ = ids.swapRemove(i);
                    break;
                }
            }
        },
        .pattern_symbol => ids.clearRetainingCapacity(),
        else => {},
    }
}
