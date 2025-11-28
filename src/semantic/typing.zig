//! This file contains the semantic validation logic for types.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const args_ = @import("args.zig");
const errs_ = @import("../util/errors.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Span = @import("../util/span.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

const Validate_Error_Enum = error{CompileError};

pub fn type_check(span: Span, got: *Type_AST, expected: ?*Type_AST, errors: *errs_.Errors) Validate_Error_Enum!void {
    if (expected != null and !got.types_match(expected.?)) {
        return throw_unexpected_type(span, expected.?, got, errors);
    }
}

/// Checks that a type is equal to unit, throws an error if it is not.
pub fn middle_statement_check(span: Span, got: *Type_AST, errors: *errs_.Errors) Validate_Error_Enum!void {
    if (!prelude_.unit_type.types_match(got) and !got.types_match(prelude_.void_type)) {
        return throw_unexpected_type(span, prelude_.unit_type, got, errors);
    }
}

pub fn type_check_int(
    ast: *ast_.AST,
    expected: ?*Type_AST, // This should NOT be expanded < it is, though...
    errors: *errs_.Errors,
) Validate_Error_Enum!*Type_AST {
    const expanded_expected = if (expected != null) expected.?.expand_identifier() else null;

    if (expanded_expected == null or prelude_.unit_type.types_match(expanded_expected.?)) {
        return prelude_.int_type;
    }

    if (expanded_expected != null) { //and !try checked_types_match(prelude_.unit_type, expanded_expected.?, errors)
        const info = prelude_.info_from_ast(expanded_expected.?);
        if (info != null and info.?.bounds != null) {
            if (ast.int.data < info.?.bounds.?.lower or
                ast.int.data > info.?.bounds.?.upper)
            {
                // This error is thrown because the `expanded_expected` is out of bounds of the expected type
                errors.add_error(errs_.Error{ .integer_out_of_bounds = .{
                    .span = ast.token().span,
                    .expected = expected.?,
                    .value = ast.int.data,
                } });
                return error.CompileError;
            }
        } else {
            // This error is thrown because the `expanded_expected` is not an integer primitive type
            return throw_unexpected_type(ast.token().span, expanded_expected.?, prelude_.int_type, errors);
        }
    }
    return expected orelse prelude_.int_type;
}

pub fn type_check_float(ast: *ast_.AST, expected: ?*Type_AST, errors: *errs_.Errors) Validate_Error_Enum!*Type_AST {
    if (expected != null and !expected.?.can_represent_float()) {
        return throw_unexpected_type(ast.token().span, expected.?, prelude_.float_type, errors);
    }
    return expected orelse prelude_.float_type;
}

pub fn type_check_eq(span: Span, got: *Type_AST, errors: *errs_.Errors) Validate_Error_Enum!void {
    if (!got.is_eq_type()) {
        errors.add_error(errs_.Error{ .expected_builtin_typeclass = .{ .span = span, .expected = "equalable", .got = got } });
        return error.CompileError;
    }
}

pub fn type_check_ord(span: Span, got: *Type_AST, errors: *errs_.Errors) Validate_Error_Enum!void {
    if (!got.is_ord_type()) {
        errors.add_error(errs_.Error{ .expected_builtin_typeclass = .{ .span = span, .expected = "orderable", .got = got } });
        return error.CompileError;
    }
}

pub fn type_check_arithmetic(span: Span, got: *Type_AST, errors: *errs_.Errors) Validate_Error_Enum!void {
    if (!got.is_num_type()) {
        errors.add_error(errs_.Error{ .expected_builtin_typeclass = .{ .span = span, .expected = "arithmetic", .got = got } });
        return error.CompileError;
    }
}

pub fn type_check_integral(span: Span, got: *Type_AST, errors: *errs_.Errors) Validate_Error_Enum!void {
    if (!got.is_int_type()) {
        errors.add_error(errs_.Error{ .expected_builtin_typeclass = .{ .span = span, .expected = "integer", .got = got } });
        return error.CompileError;
    }
}

pub fn type_check_bits(span: Span, got: *Type_AST, errors: *errs_.Errors) Validate_Error_Enum!void {
    if (!got.is_bits_type()) {
        errors.add_error(errs_.Error{ .expected_builtin_typeclass = .{ .span = span, .expected = "bits", .got = got } });
        return error.CompileError;
    }
}

pub fn throw_unexpected_type(span: Span, expected: *Type_AST, got: *Type_AST, errors: *errs_.Errors) Validate_Error_Enum {
    errors.add_error(errs_.Error{ .unexpected_type = .{ .span = span, .expected = expected, .got = got } });
    return error.CompileError;
}

pub fn throw_unexpected_void_type(span: Span, errors: *errs_.Errors) Validate_Error_Enum {
    errors.add_error(errs_.Error{ .basic = .{ .span = span, .msg = "comptime cannot be type `Void`" } });
    return error.CompileError;
}

fn throw_not_selectable(_type: *Type_AST, span: Span, errors: *errs_.Errors) Validate_Error_Enum {
    errors.add_error(errs_.Error{ .not_selectable = .{ .span = span, ._type = _type } });
    return error.CompileError;
}

pub fn throw_wrong_from(
    from_name: []const u8,
    operator_name: []const u8,
    got: *Type_AST,
    span: Span,
    errors: *errs_.Errors,
) Validate_Error_Enum {
    errors.add_error(errs_.Error{ .wrong_from = .{
        .span = span,
        .operator = operator_name,
        .from = from_name,
        .got = got,
    } });
    return error.CompileError;
}

pub fn coalesce_operator(lhs_expanded_type: *Type_AST, ast: *ast_.AST, span: Span, errors: *errs_.Errors) Validate_Error_Enum!void {
    std.debug.assert(ast.* == .@"orelse" or ast.* == .@"catch");
    const expected_sum_from: Type_AST.Sum_From = if (ast.* == .@"orelse") .optional else .@"error";
    if (lhs_expanded_type.* != .enum_type or lhs_expanded_type.enum_type.from != expected_sum_from) {
        errors.add_error(errs_.Error{ .wrong_from = .{
            .span = span,
            .operator = @tagName(ast.*),
            .from = @tagName(expected_sum_from),
            .got = lhs_expanded_type,
        } });
        return error.CompileError;
    }
}

pub fn find_select_pos(_type: *Type_AST, field: []const u8, span: Span, errors: *errs_.Errors) Validate_Error_Enum!usize {
    if (_type.* != .struct_type and _type.* != .enum_type and _type.* != .untagged_sum_type and _type.* != .context_type) {
        return throw_not_selectable(_type, span, errors);
    }
    for (_type.children().items, 0..) |term, i| {
        if (term.* != .annotation) {
            return throw_not_selectable(_type, span, errors);
        }
        if (std.mem.eql(u8, term.annotation.pattern.token().data, field)) {
            return i;
        }
    } else {
        errors.add_error(errs_.Error{ .member_not_in_type = .{ .span = span, .identifier = field, .name = "struct", .type = _type } });
        return error.CompileError;
    }
}
