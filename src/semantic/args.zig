//! This file contains various semantic checks relating to arguments and fields, including default arguments/fields, and
//! named arguments/fields.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const errs_ = @import("../util/errors.zig");
const Span = @import("../util/span.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

const Validate_Error_Enum = error{CompileError};

pub const Validate_Args_Thing = enum {
    function,
    method,
    context,
    @"struct",
    tuple,
    array,

    fn thing_name(self: @This()) []const u8 {
        return @tagName(self);
    }

    fn takes_name(self: @This()) []const u8 {
        return switch (self) {
            .context, .function, .method => "parameter",
            .@"struct" => "field",
            .tuple, .array => "element",
        };
    }

    fn given_name(self: @This()) []const u8 {
        return switch (self) {
            .context, .function, .method => "argument",
            .@"struct" => "value",
            .tuple, .array => "element",
        };
    }
};

// This has to be ok to do twice for the same args, because stamps have to do it internally.
pub fn default_args(
    thing: Validate_Args_Thing,
    asts: std.array_list.Managed(*ast_.AST), // The args for a call, or the terms for a struct
    call_span: Span,
    expected: *const std.array_list.Managed(*Type_AST),
    errors: *errs_.Errors,
    allocator: std.mem.Allocator,
) Validate_Error_Enum!std.array_list.Managed(*ast_.AST) {
    if (try args_are_named(asts, errors) and expected.items.len > 0) {
        return named_args(thing, asts, call_span, expected, errors, allocator) catch |err| switch (err) {
            error.NoDefault => error.CompileError,
            error.CompileError => error.CompileError,
        };
    } else {
        return positional_args(thing, asts, call_span, expected, errors, allocator) catch |err| switch (err) {
            error.NoDefault => error.CompileError,
        };
    }
}

/// Determines if there are named arguments in an argument list. If there are no arguments, there are
/// no named arguments.
///
/// Throws `error.CompileError` if there is a mix of positional and named arguments.
fn args_are_named(
    asts: std.array_list.Managed(*ast_.AST),
    errors: *errs_.Errors,
) Validate_Error_Enum!bool {
    if (asts.items.len == 0) {
        return false;
    }

    var has_named_arg = false;
    var has_pos_arg = false;
    for (asts.items) |term| {
        if (term.* == .assign) {
            has_named_arg = true;
        } else {
            has_pos_arg = true;
        }
    }
    std.debug.assert(has_named_arg or has_pos_arg);
    if (has_named_arg and has_pos_arg) {
        const arg_span = asts.items[0].token().span;
        errors.add_error(errs_.Error{ .basic = .{ .span = arg_span, .msg = "mixed positional and named arguments are not allowed" } });
        return error.CompileError;
    } else {
        return has_named_arg;
    }
}

/// Accepts a list of AST arguments, the expected parameter type, and if the ASTs list isn't long enough for the parameters, prepends the
/// default values for each missing argument.
///
/// Returns NoDefault when a default value cannot be created
fn positional_args(
    thing: Validate_Args_Thing,
    asts: std.array_list.Managed(*ast_.AST),
    call_span: Span,
    expected: *const std.array_list.Managed(*Type_AST),
    errors: *errs_.Errors,
    allocator: std.mem.Allocator,
) error{NoDefault}!std.array_list.Managed(*ast_.AST) {
    // TODO: Too long
    var filled_args = std.array_list.Managed(*ast_.AST).init(allocator);
    errdefer filled_args.deinit();

    for (expected.items, 0..) |term, i| {
        // ast is struct, append ast's corresponding term
        if (asts.items.len > 1 and i < asts.items.len) {
            filled_args.append(asts.items[i]) catch unreachable;
        }
        // ast is unit or ast isn't a struct and i > 0 or ast is a struct and off the edge of ast's terms
        // try to fill with the default
        else if (asts.items.len == 0 or (asts.items.len <= 1 and i > 0) or (asts.items.len > 1 and i >= asts.items.len)) {
            if (term.* == .annotation and term.annotation.init != null) {
                filled_args.append(term.annotation.init.?) catch unreachable;
            } else {
                errors.add_error(errs_.Error{ .mismatch_arity = .{
                    .span = call_span,
                    .takes = expected.items.len,
                    .given = asts.items.len,
                    .thing_name = thing.thing_name(),
                    .takes_name = thing.takes_name(),
                    .given_name = thing.given_name(),
                } });
                return error.NoDefault;
            }
        }
        // ast is not struct, i != 0, append ast as first term
        else {
            filled_args.append(asts.items[0]) catch unreachable;
        }
    }

    if (filled_args.items.len < asts.items.len) {
        // Add the rest, for extern variadic function calls
        for (filled_args.items.len..asts.items.len) |i| {
            filled_args.append(asts.items[i]) catch unreachable;
        }
    }
    return filled_args;
}

fn named_args(
    thing: Validate_Args_Thing,
    asts: std.array_list.Managed(*ast_.AST),
    call_span: Span,
    expected: *const std.array_list.Managed(*Type_AST),
    errors: *errs_.Errors,
    allocator: std.mem.Allocator,
) (Validate_Error_Enum || error{NoDefault})!std.array_list.Managed(*ast_.AST) {
    // FIXME: High cyclo
    std.debug.assert(asts.items.len > 0);
    // Maps assign.lhs name to assign.rhs
    var arg_name_to_val_map = std.StringArrayHashMap(*ast_.AST).init(allocator);
    defer arg_name_to_val_map.deinit();

    // Associate argument names with their values
    for (asts.items) |term| {
        put_assign(term, &arg_name_to_val_map, errors) catch |err| switch (err) {
            error.CompileError => return error.NoDefault,
            else => return err,
        };
    }

    // Construct positional args in the order specified by `expected`
    var filled_args = std.array_list.Managed(*ast_.AST).init(allocator);
    errdefer filled_args.deinit();

    var args_used: usize = 0;

    for (expected.items) |term| {
        if (term.* != .annotation) {
            errors.add_error(errs_.Error{ .basic = .{
                .span = asts.items[0].token().span,
                .msg = "expected type does not accept named fields",
            } });
            return error.NoDefault;
        }
        const arg = arg_name_to_val_map.get(term.annotation.pattern.token().data);
        if (arg == null) {
            if (term.annotation.init != null) {
                filled_args.append(term.annotation.init.?) catch unreachable;
            } else {
                // Value is not provided, and there is no default init, ERROR!
                errors.add_error(errs_.Error{ .mismatch_arity = .{
                    .span = call_span,
                    .takes = expected.items.len,
                    .given = arg_name_to_val_map.keys().len,
                    .thing_name = thing.thing_name(),
                    .takes_name = thing.takes_name(),
                    .given_name = thing.given_name(),
                } });
                return error.NoDefault;
            }
        } else {
            filled_args.append(arg.?) catch unreachable;
        }
        args_used += 1;
    }

    if (args_used < arg_name_to_val_map.keys().len) {
        errors.add_error(errs_.Error{ .mismatch_arity = .{
            .span = call_span,
            .takes = expected.items.len,
            .given = arg_name_to_val_map.keys().len,
            .thing_name = thing.thing_name(),
            .takes_name = thing.takes_name(),
            .given_name = thing.given_name(),
        } });
        return error.NoDefault;
    }

    return filled_args;
}

fn put_assign(ast: *ast_.AST, arg_map: *std.StringArrayHashMap(*ast_.AST), errors: *errs_.Errors) Validate_Error_Enum!void {
    if (ast.lhs().* != .enum_value) {
        errors.add_error(errs_.Error{ .expected_basic_token = .{ .expected = "an named argument", .got = ast.lhs().token() } });
        return error.CompileError;
    }
    try put_ast_map(ast.rhs(), ast.lhs().enum_value.get_name(), ast.token().span, arg_map, errors);
}

/// Puts an ast into a String->AST map, if a given name isn't already in the map.
pub fn put_ast_map(
    ast: *ast_.AST,
    name: []const u8,
    span: Span,
    map: *std.StringArrayHashMap(*ast_.AST),
    errors: *errs_.Errors,
) Validate_Error_Enum!void {
    if (map.get(name)) |_| {
        errors.add_error(errs_.Error{ .duplicate = .{
            .span = span,
            .identifier = name,
            .first = null,
        } });
    } else {
        map.put(name, ast) catch unreachable;
    }
}

/// Checks to see if an argument is meant for a method with a value receiver, and if so, implicitly takes the
/// address of the argument.
pub fn implicit_ref(
    args: *std.array_list.Managed(*ast_.AST),
    expected: *const std.array_list.Managed(*Type_AST),
    allocator: std.mem.Allocator,
) void {
    if (expected.items.len == 0) return;
    const param_type = expected.items[0];
    if (param_type.* == .annotation and
        param_type.annotation.pattern.* == .receiver and
        param_type.child().* == .addr_of and
        param_type.annotation.pattern.receiver.kind == .value)
    {
        const old_arg = args.items[0];
        args.items[0] = ast_.AST.create_addr_of(old_arg.token(), old_arg, false, false, allocator);
    }
}

/// Validates that the number of arguments matches the number of parameters
pub fn validate_args_arity(
    thing: Validate_Args_Thing,
    args: *std.array_list.Managed(*ast_.AST),
    expected: *const std.array_list.Managed(*Type_AST),
    variadic: bool,
    span: Span,
    errors: *errs_.Errors,
) Validate_Error_Enum!void {
    const expected_length = expected.items.len;
    if (variadic) {
        if (args.items.len < expected_length) {
            errors.add_error(errs_.Error{ .mismatch_arity = .{
                .span = span,
                .takes = expected_length,
                .given = args.items.len,
                .thing_name = thing.thing_name(),
                .takes_name = thing.takes_name(),
                .given_name = thing.given_name(),
            } });
            return error.CompileError;
        }
    } else if (args.items.len != expected_length) {
        errors.add_error(errs_.Error{ .mismatch_arity = .{
            .span = span,
            .takes = expected_length,
            .given = args.items.len,
            .thing_name = thing.thing_name(),
            .takes_name = thing.takes_name(),
            .given_name = thing.given_name(),
        } });
        return error.CompileError;
    }
}

pub fn validate_requested_contexts(contexts: []const *Type_AST, errors: *errs_.Errors) Validate_Error_Enum!void {
    const core_ = @import("../hierarchy/core.zig");

    for (contexts) |ctx| {
        if (!ctx.child().types_match(core_.allocating_context) and
            !ctx.child().types_match(core_.io_context))
        {
            errors.add_error(errs_.Error{ .basic = .{
                .span = ctx.token().span,
                .msg = "can't request this context",
            } });
            return error.CompileError;
        }
    }
}
