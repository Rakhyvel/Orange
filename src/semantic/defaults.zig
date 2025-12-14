//! This file contains the logic for generating default values for types.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const errs_ = @import("../util/errors.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Span = @import("../util/span.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

const Validate_Error_Enum = error{CompileError};

pub fn generate_default(_type: *Type_AST, span: Span, errors: *errs_.Errors, allocator: std.mem.Allocator) Validate_Error_Enum!*ast_.AST {
    switch (_type.*) {
        .identifier => {
            const expanded_type = _type.expand_identifier();
            if (expanded_type == _type) {
                const primitive_info = prelude_.info_from_name(_type.token().data) orelse {
                    errors.add_error(errs_.Error{ .no_default = .{ .span = span, ._type = _type } });
                    return error.CompileError;
                };
                if (primitive_info.default_value != null) {
                    return primitive_info.default_value.?;
                } else {
                    errors.add_error(errs_.Error{ .no_default = .{ .span = span, ._type = _type } });
                    return error.CompileError;
                }
            } else {
                return try generate_default(expanded_type, span, errors, allocator);
            }
        },
        .function => {
            errors.add_error(errs_.Error{ .no_default = .{ .span = span, ._type = _type } });
            return error.CompileError;
        },
        .dyn_type, .addr_of => return ast_.AST.create_int(_type.token(), 0, allocator),
        .unit_type => return ast_.AST.create_unit_value(_type.token(), allocator),
        .enum_type => {
            var retval = ast_.AST.create_enum_value(_type.token(), allocator);
            if (_type.enum_type.from == .optional) {
                retval.set_pos(1);
            } else {
                retval.set_pos(0); // if nothing's wrong, then everything's `.ok`!
            }
            retval.enum_value.base = _type;
            retval.common()._token = _type.children().items[retval.pos().?].annotation.pattern.token();
            const proper_term: *Type_AST = _type.children().items[retval.pos().?];
            retval.enum_value.init = try generate_default(proper_term, span, errors, allocator);
            return retval;
        },
        .untagged_sum_type => {
            return try generate_default(_type.child(), span, errors, allocator);
        },
        .struct_type => {
            var value_terms = std.array_list.Managed(*ast_.AST).init(allocator);
            errdefer value_terms.deinit();
            for (_type.children().items) |term| {
                const default_term = try generate_default(term, span, errors, allocator);
                value_terms.append(default_term) catch unreachable;
            }
            return ast_.AST.create_struct_value(_type.token(), _type, value_terms, allocator);
        },
        .tuple_type => {
            var value_terms = std.array_list.Managed(*ast_.AST).init(allocator);
            errdefer value_terms.deinit();
            for (_type.children().items) |term| {
                const default_term = try generate_default(term, span, errors, allocator);
                value_terms.append(default_term) catch unreachable;
            }
            return ast_.AST.create_tuple_value(_type.token(), value_terms, allocator);
        },
        .annotation => if (_type.annotation.init != null) {
            return _type.annotation.init.?;
        } else {
            return generate_default(_type.child(), span, errors, allocator);
        },
        .as_trait => return generate_default(_type.lhs(), span, errors, allocator),
        .array_of => {
            var value_terms = std.array_list.Managed(*ast_.AST).init(allocator);
            const child = try generate_default(_type.child(), span, errors, allocator);
            for (0..@intCast(_type.array_of.len.int.data)) |_| {
                value_terms.append(child) catch unreachable;
            }
            return ast_.AST.create_array_value(_type.token(), value_terms, allocator);
        },

        .generic_apply => return generate_default(_type.generic_apply._symbol.?.init_typedef().?, span, errors, allocator),

        .poison,
        .anyptr_type,
        .access,
        .type_of,
        .domain_of,
        .index,
        .context_type,
        .eq_constraint,
        .field,
        => std.debug.panic("compiler error: unimplemented generate_default() for: AST.{s}", .{@tagName(_type.*)}),
    }
}
