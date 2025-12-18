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

const Self = @This();

thing: Validate_Args_Thing,
args: *std.array_list.Managed(*ast_.AST),
span: Span,
expected: *const std.array_list.Managed(*Type_AST),
errors: *errs_.Errors,
variadic: bool = false,
allocator: std.mem.Allocator,

pub fn init(thing: Validate_Args_Thing, args: *std.array_list.Managed(*ast_.AST), span: Span, expected: *const std.array_list.Managed(*Type_AST), errors: *errs_.Errors, allocator: std.mem.Allocator) Self {
    return Self{
        .thing = thing,
        .args = args,
        .span = span,
        .expected = expected,
        .errors = errors,
        .allocator = allocator,
    };
}

pub fn set_variadic(self: *Self, variadic: bool) void {
    self.variadic = variadic;
}

// This has to be ok to do twice for the same args, because stamps have to do it internally.
pub fn default_args(self: *Self) Validate_Error_Enum!std.array_list.Managed(*ast_.AST) {
    if (try self.args_are_named() and self.expected.items.len > 0) {
        return self.named_args() catch |err| switch (err) {
            error.NoDefault => error.CompileError,
            error.CompileError => error.CompileError,
        };
    } else {
        return self.positional_args() catch |err| switch (err) {
            error.NoDefault => error.CompileError,
        };
    }
}

/// Determines if there are named arguments in an argument list. If there are no arguments, there are
/// no named arguments.
///
/// Throws `error.CompileError` if there is a mix of positional and named arguments.
fn args_are_named(self: *Self) Validate_Error_Enum!bool {
    if (self.args.items.len == 0) {
        return false;
    }

    var has_named_arg = false;
    var has_pos_arg = false;
    for (self.args.items, 0..) |term, i| {
        if (term.* == .assign) {
            has_named_arg = true;
        } else if (self.thing != .method or i != 0) {
            has_pos_arg = true;
        }
    }
    if (!has_pos_arg and !has_named_arg) {
        has_pos_arg = true;
    }
    if (has_named_arg and has_pos_arg) {
        const arg_span = self.args.items[0].token().span;
        self.errors.add_error(errs_.Error{ .basic = .{ .span = arg_span, .msg = "mixed positional and named arguments are not allowed" } });
        return error.CompileError;
    } else {
        return has_named_arg;
    }
}

/// Accepts a list of AST arguments, the expected parameter type, and if the ASTs list isn't long enough for the parameters, prepends the
/// default values for each missing argument.
///
/// Returns NoDefault when a default value cannot be created
fn positional_args(self: *Self) error{NoDefault}!std.array_list.Managed(*ast_.AST) {
    // TODO: Too long
    var filled_args = std.array_list.Managed(*ast_.AST).init(self.allocator);
    errdefer filled_args.deinit();

    for (self.expected.items, 0..) |term, i| {
        // ast is struct, append ast's corresponding term
        if (self.args.items.len > 1 and i < self.args.items.len) {
            filled_args.append(self.args.items[i]) catch unreachable;
        }
        // ast is unit or ast isn't a struct and i > 0 or ast is a struct and off the edge of ast's terms
        // try to fill with the default
        else if (self.args.items.len == 0 or (self.args.items.len <= 1 and i > 0) or (self.args.items.len > 1 and i >= self.args.items.len)) {
            if (term.* == .annotation and term.annotation.init != null) {
                filled_args.append(term.annotation.init.?) catch unreachable;
            } else {
                self.errors.add_error(errs_.Error{ .mismatch_arity = .{
                    .span = self.span,
                    .takes = self.expected.items.len,
                    .given = self.args.items.len,
                    .thing_name = self.thing.thing_name(),
                    .takes_name = self.thing.takes_name(),
                    .given_name = self.thing.given_name(),
                } });
                return error.NoDefault;
            }
        }
        // ast is not struct, i != 0, append ast as first term
        else {
            filled_args.append(self.args.items[0]) catch unreachable;
        }
    }

    if (filled_args.items.len < self.args.items.len) {
        // Add the rest, for extern variadic function calls
        for (filled_args.items.len..self.args.items.len) |i| {
            filled_args.append(self.args.items[i]) catch unreachable;
        }
    }
    return filled_args;
}

fn named_args(self: *Self) (Validate_Error_Enum || error{NoDefault})!std.array_list.Managed(*ast_.AST) {
    // FIXME: High cyclo
    std.debug.assert(self.args.items.len > 0);

    var param_name_map = std.StringArrayHashMap(*Type_AST).init(self.allocator);
    defer param_name_map.deinit();

    var arg_name_map = std.StringArrayHashMap(*ast_.AST).init(self.allocator);
    defer arg_name_map.deinit();

    for (self.expected.items) |param| {
        if (param.* != .annotation) {
            self.errors.add_error(errs_.Error{ .basic = .{
                .span = self.args.items[0].token().span,
                .msg = "expected type does not accept named fields",
            } });
            return error.NoDefault;
        }
        const param_name = param.annotation.pattern.token().data;
        param_name_map.put(param_name, param) catch unreachable;
    }

    for (self.args.items) |arg| {
        if (arg.* != .assign) {
            arg_name_map.put("self", arg) catch unreachable;
            continue;
        }
        if (arg.lhs().* != .enum_value) {
            self.errors.add_error(errs_.Error{ .expected_basic_token = .{ .expected = "an named argument", .got = arg.lhs().token() } });
            return error.CompileError;
        }
        const arg_name = arg.lhs().enum_value.get_name();
        const param = param_name_map.get(arg_name) orelse {
            self.errors.add_error(errs_.Error{ .no_such_named = .{
                .span = arg.token().span,
                .thing_name = self.thing.thing_name(),
                .takes_name = self.thing.takes_name(),
                .field_name = arg_name,
            } });
            return error.NoDefault;
        };
        const param_name = param.annotation.pattern.token().data;
        if (arg_name_map.contains(param_name)) {
            self.errors.add_error(errs_.Error{ .duplicate = .{
                .span = arg.token().span,
                .thing = "item",
                .identifier = arg_name,
                .first = null,
            } });
            return error.NoDefault;
        }
        arg_name_map.put(param_name, arg.rhs()) catch unreachable;
    }

    var filled_args = std.array_list.Managed(*ast_.AST).init(self.allocator);
    errdefer filled_args.deinit();
    for (self.expected.items) |param| {
        const param_name = param.annotation.pattern.token().data;
        if (arg_name_map.get(param_name)) |arg| {
            filled_args.append(arg) catch unreachable;
        } else {
            if (param.annotation.init) |default_value| {
                filled_args.append(default_value) catch unreachable;
            } else {
                self.errors.add_error(errs_.Error{ .unspecified_required = .{
                    .span = self.span,
                    .takes_name = self.thing.takes_name(),
                    .field_name = param_name,
                } });
                return error.NoDefault;
            }
        }
    }

    return filled_args;
}

/// Checks to see if an argument is meant for a method with a value receiver, and if so, implicitly takes the
/// address of the argument.
pub fn implicit_ref(self: *Self) void {
    if (self.expected.items.len == 0) return;
    const param_type = self.expected.items[0];
    if (param_type.* == .annotation and
        param_type.annotation.pattern.* == .receiver and
        param_type.child().* == .addr_of and
        param_type.annotation.pattern.receiver.kind == .value)
    {
        const old_arg = self.args.items[0];
        self.args.items[0] = ast_.AST.create_addr_of(old_arg.token(), old_arg, false, false, self.allocator);
    }
}

/// Validates that the number of arguments matches the number of parameters
pub fn validate_args_arity(self: *Self) Validate_Error_Enum!void {
    const expected_length = self.expected.items.len;
    if (self.variadic) {
        if (self.args.items.len < expected_length) {
            self.errors.add_error(errs_.Error{ .mismatch_arity = .{
                .span = self.span,
                .takes = expected_length,
                .given = self.args.items.len,
                .thing_name = self.thing.thing_name(),
                .takes_name = self.thing.takes_name(),
                .given_name = self.thing.given_name(),
            } });
            return error.CompileError;
        }
    } else if (self.args.items.len != expected_length) {
        self.errors.add_error(errs_.Error{ .mismatch_arity = .{
            .span = self.span,
            .takes = expected_length,
            .given = self.args.items.len,
            .thing_name = self.thing.thing_name(),
            .takes_name = self.thing.takes_name(),
            .given_name = self.thing.given_name(),
        } });
        return error.CompileError;
    }
}

pub fn validate_requested_contexts(contexts: []const *Type_AST, errors: *errs_.Errors) Validate_Error_Enum!void {
    const core_ = @import("../hierarchy/core.zig");

    for (contexts) |ctx| {
        if (!ctx.child().types_match(core_.allocating_context) and
            !ctx.child().types_match(core_.io_context) and
            !ctx.child().types_match(core_.args_context) and
            !ctx.child().types_match(core_.file_io_context))
        {
            errors.add_error(errs_.Error{ .basic = .{
                .span = ctx.token().span,
                .msg = "can't request this context",
            } });
            return error.CompileError;
        }
    }
}
