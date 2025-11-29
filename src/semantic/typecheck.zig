//! This file contains logic for validating an AST.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const args_ = @import("args.zig");
const Const_Eval = @import("../semantic/const_eval.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const defaults_ = @import("defaults.zig");
const errs_ = @import("../util/errors.zig");
const poison_ = @import("../ast/poison.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const typing_ = @import("typing.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const walk_ = @import("../ast/walker.zig");

const Validate_Error_Enum = error{
    // Returned when there is a type check compile error
    CompileError,
    // Returned when a type is encountered, which may or may not be expected
    UnexpectedTypeType,
    // General purpose out of memory error
    OutOfMemory,
};

const Self: type = @This();

ctx: *Compiler_Context,
map: std.AutoArrayHashMap(*ast_.AST, *Type_AST),

pub fn init(ctx: *Compiler_Context) Self {
    return Self{
        .ctx = ctx,
        .map = std.AutoArrayHashMap(*ast_.AST, *Type_AST).init(ctx.allocator()),
    };
}

pub fn typeof(self: *const Self, ast: *ast_.AST) *Type_AST {
    if ((ast.common().validation_state != .valid and ast.common().validation_state != .invalid)) {
        std.debug.panic("type for ast {f} has not been constructed", .{ast});
    }
    return self.map.get(ast) orelse poison_.poisoned_type;
}

pub fn assert_typeof(self: *Self, ast: *ast_.AST, _type: *Type_AST) void {
    ast.common().validation_state = .valid;
    self.map.put(ast, _type) catch unreachable;
}

/// Recursively validates an AST, returning a valid AST if successful, or the poisoned AST if not.
///
/// Interpretation occurs during the validation of comptime AST nodes. Module compilation is possible during this phase
/// as well, when validating the build script.
///
/// ### Parameters
/// - `ast`: The AST to validate.
/// - `old_expected_type`: Should be null if `ast` can be any type
/// - `compiler`: The compiler instance.
pub fn typecheck_AST(self: *Self, ast: *ast_.AST, expected: ?*Type_AST) Validate_Error_Enum!*Type_AST {
    // TODO: Bit long
    if (ast.common().validation_state == .validating) {
        // std.debug.print("{}\n", .{ast});
        self.ctx.errors.add_error(errs_.Error{ .recursive_definition = .{
            .span = ast.token().span,
            .symbol_name = null,
        } });
        return error.CompileError;
    } else if (ast.common().validation_state != .unvalidated) {
        return self.typeof(ast);
    }

    var expected_type = expected;

    // std.debug.print("{}: {?}\n", .{ ast, expected_type });
    ast.common().validation_state = .validating;

    if (expected_type != null and expected_type.?.* == .poison) {
        expected_type = null;
    }

    if (expected_type) |_| {
        const expanded = expected_type.?.expand_identifier();
        if (expected_type.?.* == .type_of) {
            expected_type.?.* = expanded.*;
        }
        if (expected_type.?.* == .annotation) {
            expected_type = expected_type.?.child();
        }
        if (expected_type.?.* == .access) {
            expected_type = expected_type.?.symbol().?.init_typedef();
        }
    }

    const actual_type = self.typecheck_AST_internal(ast, expected_type) catch |e| {
        ast.common().validation_state = .invalid;
        return e;
    };

    if (expected_type) |_| {
        if (!prelude_.unit_type.types_match(expected_type.?) or (actual_type.* == .enum_type and actual_type.enum_type.from == .@"error"))
            typing_.type_check(ast.token().span, actual_type, expected_type.?, &self.ctx.errors) catch |e| {
                ast.common().validation_state = .invalid;
                return e;
            };
    }

    self.map.put(ast, actual_type) catch unreachable;

    if (actual_type.* != .poison) {
        ast.common().validation_state = .invalid;
    } else {
        ast.common().validation_state = .valid;
    }

    return actual_type;
}

var depth: usize = 0;
fn typecheck_AST_internal(self: *Self, ast: *ast_.AST, expected: ?*Type_AST) Validate_Error_Enum!*Type_AST {
    // TODO: Ugh this function is too long
    depth += 1;
    // std.debug.print("[{}] {f}: {?f}\n", .{ depth, ast, expected });
    switch (ast.*) {
        // Nop, always "valid"
        .poison => return poison_.poisoned_type,

        // Pattern symbols, traits, and impls are validated elsewhere, the AST doesn't need to be re-validated.
        .module,
        .trait,
        .impl,
        .enum_decl,
        .struct_decl,
        .type_alias,
        .unit_value,
        => return prelude_.unit_type,

        .pattern_symbol => return ast.symbol().?.type(),

        .int => return try typing_.type_check_int(ast, expected, &self.ctx.errors),

        .float => return try typing_.type_check_float(ast, expected, &self.ctx.errors),

        .string => return prelude_.string_type,

        .identifier => {
            // look up symbol, that's the type
            const symbol = ast.symbol().?;
            if (symbol.validation_state == .invalid) {
                return error.CompileError;
            }
            try self.ctx.validate_symbol.validate_symbol(symbol);
            if (symbol.is_type() or symbol.kind == .context) {
                if (expected != null) {
                    self.ctx.errors.add_error(errs_.Error{ .unexpected_type_type = .{ .expected = expected, .span = ast.token().span } });
                    return error.CompileError;
                }
                return error.UnexpectedTypeType;
            }
            if (symbol.decl.?.num_generic_params() > 0) {
                self.ctx.errors.add_error(errs_.Error{ .unapplied_generic = .{
                    .span = ast.token().span,
                    .symbol_name = symbol.name,
                    .num_generics = symbol.decl.?.num_generic_params(),
                } });
                return error.CompileError;
            }
            return symbol.type();
        },
        .access => {
            _ = self.typecheck_AST(ast.lhs(), null) catch |e| switch (e) {
                error.UnexpectedTypeType => {}, // This is expected
                else => return e,
            };

            // look up symbol, that's the type
            const symbol = ast.symbol().?;
            if (symbol.validation_state == .invalid) {
                return error.CompileError;
            }
            try self.ctx.validate_symbol.validate_symbol(symbol);

            if (symbol.decl.?.num_generic_params() > 0) {
                self.ctx.errors.add_error(errs_.Error{ .unapplied_generic = .{
                    .span = ast.token().span,
                    .symbol_name = symbol.name,
                    .num_generics = symbol.decl.?.num_generic_params(),
                } });
                return error.CompileError;
            }

            if (symbol.is_type()) {
                if (expected != null) {
                    self.ctx.errors.add_error(errs_.Error{ .unexpected_type_type = .{ .expected = expected, .span = ast.token().span } });
                    return error.CompileError;
                } else {
                    return prelude_.unit_type;
                }
            } else {
                return symbol.type();
            }
        },
        .true, .false => return prelude_.bool_type,
        .not => {
            _ = self.typecheck_AST(ast.expr(), prelude_.bool_type) catch return error.CompileError;
            return prelude_.bool_type;
        },
        .negate => {
            const expr_type = self.typecheck_AST(ast.expr(), expected) catch return error.CompileError;
            try typing_.type_check_arithmetic(ast.token().span, expr_type, &self.ctx.errors);
            return expr_type;
        },
        .dereference => {
            const expr_span = ast.expr().token().span;
            const expr_expected_type: ?*Type_AST =
                if (expected != null)
                    Type_AST.create_addr_of_type(ast.token(), expected.?, false, false, self.ctx.allocator())
                else
                    null;

            const expr_type = self.typecheck_AST(ast.expr(), expr_expected_type) catch return error.CompileError;
            const expanded_expr_type = expr_type.expand_identifier();
            if (expanded_expr_type.* != .addr_of) {
                return typing_.throw_wrong_from("address", "dereference", expanded_expr_type, expr_span, &self.ctx.errors);
            } else {
                return expanded_expr_type.child();
            }
        },
        .@"try" => {
            const expr_span = ast.expr().token().span;
            const expr_type = self.typecheck_AST(ast.expr(), null) catch return error.CompileError;
            var expanded_expr_type = expr_type.expand_identifier();
            if (expanded_expr_type.* != .enum_type or expanded_expr_type.enum_type.from != .@"error") {
                return typing_.throw_wrong_from("error", "try", expanded_expr_type, expr_span, &self.ctx.errors);
            }
            const expanded_function_codomain = ast.symbol().?.type().rhs().expand_identifier();
            if (expanded_function_codomain.* != .enum_type or expanded_function_codomain.enum_type.from != .@"error") {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{
                    .span = ast.token().span,
                    .msg = "enclosing function around try expression does not return an error",
                } });
                return error.CompileError;
            } else {
                // err must match
                const expr_error_type = expanded_expr_type.get_err_type().child();
                const function_error_type = expanded_function_codomain.get_err_type().child();
                if (!expr_error_type.types_match(function_error_type)) {
                    return typing_.throw_unexpected_type(expr_span, expanded_expr_type, expanded_function_codomain, &self.ctx.errors);
                }
            }
            return expanded_expr_type.get_ok_type().child();
        },
        .default => {
            try self.ctx.validate_type.validate_type(ast.default._type);
            return ast.default._type;
        },
        .size_of => {
            try self.ctx.validate_type.validate_type(ast.size_of._type);
            return prelude_.int_type;
        },
        .@"comptime" => {
            const expr_type = self.typecheck_AST(ast.expr(), expected) catch return error.CompileError;
            const expr_expanded_type = expr_type.expand_identifier();
            if (expr_expanded_type.types_match(prelude_.void_type)) {
                return typing_.throw_unexpected_void_type(ast.expr().token().span, &self.ctx.errors);
            }
            return expr_type;
        },
        .assign => {
            try self.validate_L_Value(ast.lhs());
            const lhs_type = self.typecheck_AST(ast.lhs(), null) catch return error.CompileError;
            const rhs_expected: ?*Type_AST = if (ast.lhs().* == .identifier and std.mem.eql(u8, ast.lhs().token().data, "_")) null else lhs_type;
            _ = self.typecheck_AST(ast.rhs(), rhs_expected) catch return error.CompileError;
            try self.assert_mutable(ast.lhs());
            return prelude_.unit_type;
        },
        .@"or", .@"and" => {
            _ = self.typecheck_AST(ast.lhs(), prelude_.bool_type) catch return error.CompileError;
            _ = self.typecheck_AST(ast.rhs(), prelude_.bool_type) catch return error.CompileError;
            return prelude_.bool_type;
        },
        .add, .sub, .mult, .div => {
            // These operators are open, since they allow for polymorphism between their operands.
            const lhs_type = try self.binary_operator_open(ast, expected);
            const expanded_lhs_type = lhs_type.expand_identifier();
            try typing_.type_check_arithmetic(ast.token().span, expanded_lhs_type, &self.ctx.errors);
            return expanded_lhs_type;
        },
        .mod => {
            const lhs_type = try self.binary_operator_open(ast, null);
            try typing_.type_check_integral(ast.token().span, lhs_type, &self.ctx.errors);
            return lhs_type;
        },
        .equal, .not_equal => {
            const lhs_type = try self.binary_operator_open(ast, null);
            try typing_.type_check_eq(ast.token().span, lhs_type, &self.ctx.errors); // TODO: Return a constraint type thats T: Eq, and let the top-level-function try and unify it
            return prelude_.bool_type;
        },
        .greater, .lesser, .greater_equal, .lesser_equal => {
            const lhs_type = try self.binary_operator_open(ast, null);
            try typing_.type_check_ord(ast.token().span, lhs_type, &self.ctx.errors);
            return prelude_.bool_type;
        },
        .@"catch", .@"orelse" => {
            const lhs_span = ast.lhs().token().span;
            const lhs_type = self.typecheck_AST(ast.lhs(), null) catch return error.CompileError;
            _ = self.typecheck_AST(ast.rhs(), expected) catch return error.CompileError;
            var expanded_lhs_type = lhs_type.expand_identifier();
            try typing_.coalesce_operator(expanded_lhs_type, ast, lhs_span, &self.ctx.errors);
            return expanded_lhs_type.get_nominal_type().child();
        },
        .bit_and, .bit_or, .bit_xor => {
            var retval: ?*Type_AST = null;
            for (0..ast.children().items.len) |i| {
                const arg_type = self.typecheck_AST(ast.children().items[i], expected orelse prelude_.word64_type) catch return error.CompileError;
                if (retval == null) {
                    retval = arg_type;
                }
                try typing_.type_check_bits(ast.children().items[i].token().span, arg_type, &self.ctx.errors);
            }
            return retval.?;
        },
        .bit_not => {
            const expr_type = self.typecheck_AST(ast.expr(), expected orelse prelude_.word64_type) catch return error.CompileError;
            try typing_.type_check_bits(ast.token().span, expr_type, &self.ctx.errors);
            return expr_type;
        },
        .left_shift, .right_shift => {
            const lhs_type = self.typecheck_AST(ast.lhs(), expected orelse prelude_.word64_type) catch return error.CompileError;
            _ = self.typecheck_AST(ast.rhs(), lhs_type) catch return error.CompileError;
            try typing_.type_check_bits(ast.token().span, lhs_type, &self.ctx.errors);
            return lhs_type;
        },
        .call => {
            const lhs_span = ast.lhs().token().span;
            var lhs_type = self.typecheck_AST(ast.lhs(), null) catch return error.CompileError;
            const expanded_lhs_type = lhs_type.expand_identifier();
            if (expanded_lhs_type.* != .function) {
                return typing_.throw_wrong_from(
                    "function",
                    "call",
                    expanded_lhs_type,
                    lhs_span,
                    &self.ctx.errors,
                );
            }

            try self.validate_context(expanded_lhs_type, ast);

            const domain = expanded_lhs_type.function.args;
            const codomain = expanded_lhs_type.rhs();
            const variadic = expanded_lhs_type.function.variadic;
            var args_validator = args_.init(.function, ast.children(), ast.token().span, &domain, &self.ctx.errors, self.ctx.allocator());
            args_validator.set_variadic(variadic);
            ast.set_children(try args_validator.default_args());
            args_validator.implicit_ref();
            try args_validator.validate_args_arity();
            _ = try self.validate_args_type(ast.children(), &domain, variadic);
            return codomain;
        },
        .index => {
            const lhs_span = ast.lhs().token().span; // Used for error reporting
            const lhs_type = self.typecheck_AST(ast.lhs(), null) catch return error.CompileError;
            if (ast.children().items.len > 1) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "too many indices" } });
                return error.CompileError;
            }
            _ = self.typecheck_AST(ast.children().items[0], prelude_.int_type) catch return error.CompileError;

            const expanded_lhs_type = try self.implicit_dereference(ast, lhs_type.expand_identifier());

            if (expanded_lhs_type.* == .struct_type) {
                if (expanded_lhs_type.struct_type.was_slice) {
                    return expanded_lhs_type.children().items[0].child().child();
                } else {
                    self.ctx.errors.add_error(errs_.Error{ .not_indexable = .{ .span = lhs_span, ._type = expanded_lhs_type } });
                    return error.CompileError;
                }
            } else if (expanded_lhs_type.* == .tuple_type) {
                // TODO: comptime expand the index
                try walk_.walk_ast(ast.children().items[0], Const_Eval.new(self.ctx));
                if (ast.children().items[0].* != .int) {
                    self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "not a constant integer" } });
                    return error.CompileError;
                }
                const pos = ast.children().items[0].int.data;
                ast.* = ast_.AST.create_select(ast.token(), ast.lhs(), ast.children().items[0], self.ctx.allocator()).*;
                ast.set_pos(@as(usize, @intCast(pos)));
                return self.typecheck_AST(ast, expected);
            } else if (expanded_lhs_type.* == .array_of) {
                return expanded_lhs_type.child();
            } else if (expanded_lhs_type.* == .addr_of and expanded_lhs_type.addr_of.multiptr) {
                return expanded_lhs_type.child();
            } else {
                self.ctx.errors.add_error(errs_.Error{ .not_indexable = .{ .span = lhs_span, ._type = expanded_lhs_type } });
                return error.CompileError;
            }
        },
        .generic_apply => {
            // look up symbol, that's the type
            const symbol = ast.symbol().?;
            if (symbol.validation_state == .invalid) {
                return error.CompileError;
            }
            try self.ctx.validate_symbol.validate_symbol(symbol);
            if (symbol.is_type() or symbol.kind == .context) {
                if (expected != null) {
                    self.ctx.errors.add_error(errs_.Error{ .unexpected_type_type = .{ .expected = expected, .span = ast.token().span } });
                    return error.CompileError;
                }
                return error.UnexpectedTypeType;
            }
            return symbol.type();
        },
        .select => {
            const lhs_type = self.typecheck_AST(ast.lhs(), null) catch return error.CompileError;
            const expanded_lhs_type = try self.implicit_dereference(ast, lhs_type.expand_identifier());

            if (ast.pos() == null) {
                ast.set_pos(try typing_.find_select_pos(expanded_lhs_type, ast.rhs().token().data, ast.token().span, &self.ctx.errors));
            }

            var select_lhs_type = expanded_lhs_type;
            if (ast.pos().? < 0 or ast.pos().? >= select_lhs_type.children().items.len) {
                self.ctx.errors.add_error(errs_.Error{ .bad_index = .{
                    .span = ast.token().span,
                    ._type = lhs_type,
                    .index = ast.pos().?,
                    .length = select_lhs_type.children().items.len,
                } });
                return error.CompileError;
            }
            var retval = select_lhs_type.children().items[ast.pos().?];
            while (retval.* == .annotation) {
                retval = retval.child();
            }
            return retval;
        },

        .invoke => {
            // true_lhs_type is lhs's type
            const true_lhs_type = self.typecheck_AST(ast.lhs(), null) catch return error.CompileError;
            // method_decl is the method_decl AST of the method being invoked
            var method_decl: ?*ast_.AST = undefined;
            if (true_lhs_type.expand_identifier().* == .dyn_type) {
                // The receiver is a dynamic type
                const trait_symbol = true_lhs_type.expand_identifier().child().symbol().?;
                const trait = trait_symbol.decl.?;
                method_decl = trait.trait.find_method(ast.rhs().token().data);

                if (method_decl != null and !method_decl.?.method_decl.is_virtual) {
                    self.ctx.errors.add_error(errs_.Error{
                        .invoke_not_virtual = .{
                            .span = ast.token().span,
                            .method_name = ast.rhs().token().data,
                            .trait_name = trait_symbol.name,
                        },
                    });
                    return error.CompileError;
                }
            } else {
                // The receiver is a regular type. STRIP AWAY ADDRs!
                const lhs_type = if (true_lhs_type.* == .addr_of) true_lhs_type.child() else true_lhs_type;
                try self.ctx.validate_type.validate_type(lhs_type);
                method_decl = try ast.scope().?.lookup_impl_member(lhs_type, ast.rhs().token().data, self.ctx);
            }
            if (method_decl == null) {
                self.ctx.errors.add_error(errs_.Error{
                    .type_not_impl_method = .{
                        .span = ast.token().span,
                        .method_name = ast.rhs().token().data,
                        ._type = true_lhs_type, // TODO: Strip away addr_of's
                    },
                });
                return error.CompileError;
            }
            // std.debug.assert(method_decl.?.method_decl.impl.?.num_generic_params() == 0); // must be an instatiated impl
            const method_decl_type = self.typecheck_AST(method_decl.?, null) catch return error.CompileError;
            ast.invoke.method_decl = method_decl.?;
            const domain: std.array_list.Managed(*Type_AST) = method_decl.?.decl_type().function.args;
            const expanded_true_lhs_type = true_lhs_type.expand_identifier();
            if (method_decl.?.method_decl.receiver != null and !ast.invoke.prepended) {
                const receiver_kind: ?ast_.Receiver_Kind = method_decl.?.method_decl.receiver.?.receiver.kind;
                // Trait method takes a receiver...
                // Prepend invoke lhs to args if there is a receiver
                if (expanded_true_lhs_type.* == .dyn_type or expanded_true_lhs_type.* == .addr_of) {
                    // lhs type is dynamic or an address...
                    if (!expanded_true_lhs_type.mut() and receiver_kind == .mut_addr_of) {
                        // Receiver is immutable when it should be mutable
                        self.ctx.errors.add_error(errs_.Error{ .invoke_receiver_mismatch = .{
                            .lhs_type = true_lhs_type,
                            .method_name = method_decl.?.method_decl.name.token().data,
                            .method_receiver = receiver_kind.?,
                            .receiver_span = ast.lhs().token().span,
                        } });
                        return error.CompileError;
                    }
                    ast.children().insert(0, ast.lhs()) catch unreachable; // prepend lhs to children as a receiver
                    ast.invoke.prepended = true;
                } else {
                    // lhs type is not dynamic and not an address
                    const addr_of = ast_.AST.create_addr_of(ast.lhs().token(), ast.lhs(), receiver_kind.? == .mut_addr_of, false, self.ctx.allocator());
                    ast.children().insert(0, addr_of) catch unreachable; // prepend lhs to children as a receiver
                    ast.invoke.prepended = true;
                }
            } else if (ast.invoke.prepended) {
                ast.set_lhs(ast.children().items[0]);
            }

            try self.validate_context(method_decl_type, ast);

            var args_validator = args_.init(.method, ast.children(), ast.token().span, &domain, &self.ctx.errors, self.ctx.allocator());
            ast.set_children(try args_validator.default_args());
            try args_validator.validate_args_arity();
            _ = try self.validate_args_type(ast.children(), &domain, false);

            return ast.invoke.method_decl.?.method_decl.ret_type;
        },
        .dyn_value => {
            var expr_type = self.typecheck_AST(ast.expr(), null) catch return error.CompileError;
            while (expr_type.* == .addr_of) {
                ast.set_expr(ast_.AST.create_dereference(ast.token(), ast.expr(), self.ctx.allocator()));
                expr_type = self.typecheck_AST(ast.expr(), null) catch return error.CompileError;
            }
            try self.ctx.validate_type.validate_type(ast.dyn_value.dyn_type);

            if (expr_type.* == .identifier and expr_type.symbol() != null and expr_type.symbol().?.decl.?.* == .type_param_decl) {
                const type_param_decl = expr_type.symbol().?.decl.?;
                var found_constraint = false;
                // Check to see if the type parameter has the constraint type that we're looking for
                for (type_param_decl.type_param_decl.constraints.items) |constraint| {
                    const constraint_trait = constraint.symbol().?;
                    const dyn_trait = ast.dyn_value.dyn_type.child().symbol().?;
                    if (constraint_trait == dyn_trait) {
                        found_constraint = true;
                        break;
                    }
                }
                if (!found_constraint) {
                    self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                        .span = ast.token().span,
                        .trait_name = ast.dyn_value.dyn_type.child().symbol().?.name,
                        ._type = expr_type,
                    } });
                    return error.CompileError;
                }
            } else {
                const impl = ast.scope().?.impl_trait_lookup(expr_type, ast.dyn_value.dyn_type.child().symbol().?);
                if (impl.ast == null) {
                    self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                        .span = ast.token().span,
                        .trait_name = ast.dyn_value.dyn_type.child().symbol().?.name,
                        ._type = expr_type,
                    } });
                    return error.CompileError;
                }
                ast.dyn_value.impl = impl.ast;
            }

            return ast.dyn_value.dyn_type;
        },
        .context_value => {
            try self.ctx.validate_type.validate_type(ast.context_value.parent);
            const expanded_expected: *Type_AST = if (expected == null) ast.context_value.parent.expand_identifier() else expected.?.expand_identifier();
            if (expanded_expected.* == .context_type) {
                // Expecting ast to be a product value of some product type
                var args_validator = args_.init(.context, ast.children(), ast.token().span, expanded_expected.children(), &self.ctx.errors, self.ctx.allocator());
                ast.set_children(try args_validator.default_args());
                try args_validator.validate_args_arity();
                _ = try self.validate_args_type(ast.children(), expanded_expected.children(), false);
                return ast.context_value.parent;
            } else if (ast.context_value.parent.expand_identifier().* != .context_type) {
                // Parent wasn't even a context type!
                return typing_.throw_wrong_from("context", "context value", ast.context_value.parent, ast.token().span, &self.ctx.errors);
            } else if (!prelude_.unit_type.types_match(expanded_expected)) {
                // It's ok to assign this to a unit type, something like `_ = (1, 2, 3)`
                // expecting something that is not a type nor a product is not ok!
                // poison `got`, so that it doesn't print anything for the `got` section, wouldn't make sense anyway
                return typing_.throw_unexpected_type(ast.token().span, expanded_expected, ast.context_value.parent, &self.ctx.errors);
            } else {
                return prelude_.unit_type;
            }
        },
        .struct_value => {
            try self.ctx.validate_type.validate_type(ast.struct_value.parent);
            const expanded_expected: *Type_AST = if (expected == null) ast.struct_value.parent.expand_identifier() else expected.?.expand_identifier();
            if (expanded_expected.* == .struct_type) {
                // Expecting ast to be a product value of some product type
                var args_validator = args_.init(.@"struct", ast.children(), ast.token().span, expanded_expected.children(), &self.ctx.errors, self.ctx.allocator());
                ast.set_children(try args_validator.default_args());
                try args_validator.validate_args_arity();
                _ = try self.validate_args_type(ast.children(), expanded_expected.children(), false);
                return ast.struct_value.parent;
            } else if (ast.struct_value.parent.expand_identifier().* != .struct_type) {
                // Parent wasn't even a struct type!
                return typing_.throw_wrong_from("struct", "struct value", ast.struct_value.parent, ast.token().span, &self.ctx.errors);
            } else if (!prelude_.unit_type.types_match(expanded_expected)) {
                // It's ok to assign this to a unit type, something like `_ = (1, 2, 3)`
                // expecting something that is not a type nor a product is not ok!
                // poison `got`, so that it doesn't print anything for the `got` section, wouldn't make sense anyway
                return typing_.throw_unexpected_type(ast.token().span, expanded_expected, ast.struct_value.parent, &self.ctx.errors);
            } else {
                return prelude_.unit_type;
            }
        },
        .tuple_value => {
            // TODO: This could construct the type better...
            const expanded_expected: ?*Type_AST = if (expected == null) null else expected.?.expand_identifier();
            var terms: std.array_list.Managed(*Type_AST) = undefined;
            if (expanded_expected == null) {
                terms = std.array_list.Managed(*Type_AST).init(self.ctx.allocator());
                // Not expecting anything
                for (0..ast.children().items.len) |i| {
                    const term_type = self.typecheck_AST(ast.children().items[i], null) catch return error.CompileError;
                    terms.append(term_type) catch unreachable;
                }
            } else if (expanded_expected != null and expanded_expected.?.* == .tuple_type) {
                // Expecting ast to be a product value of some product type
                var args_validator = args_.init(.tuple, ast.children(), ast.token().span, expanded_expected.?.children(), &self.ctx.errors, self.ctx.allocator());
                try args_validator.validate_args_arity();
                _ = try self.validate_args_type(ast.children(), expanded_expected.?.children(), false);
                return expected.?;
            } else {
                // It's ok to assign this to a unit type, something like `_ = (1, 2, 3)`
                // expecting something that is not a type nor a product is not ok!
                // poison `got`, so that it doesn't print anything for the `got` section, wouldn't make sense anyway
                return typing_.throw_unexpected_type(ast.token().span, expected.?, poison_.poisoned_type, &self.ctx.errors);
            }
            return Type_AST.create_tuple_type(ast.token(), terms, self.ctx.allocator());
        },
        .array_value => {
            const expanded_expanded = get_expanded_expected(expected);
            if (expanded_expanded != null and expanded_expanded.?.* != .array_of) {
                return typing_.throw_wrong_from("array", "array value", expanded_expanded.?, ast.token().span, &self.ctx.errors);
            }
            const elem_type = if (expanded_expanded == null) null else expanded_expanded.?.array_of._child;
            if (ast.children().items.len != 0) {
                const first_type = self.typecheck_AST(ast.children().items[0], elem_type) catch return error.CompileError;
                for (ast.children().items[1..]) |child| {
                    _ = self.typecheck_AST(child, first_type) catch return error.CompileError;
                }
                return Type_AST.create_array_of(ast.token(), first_type, ast_.AST.create_int(ast.token(), ast.children().items.len, self.ctx.allocator()), self.ctx.allocator());
            } else if (expanded_expanded != null) {
                return expanded_expanded.?;
            } else {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot infer type for empty array" } });
                return error.CompileError;
            }
        },
        .as => {
            const child_type = self.typecheck_AST(ast.expr(), null) catch return error.CompileError;
            if (!child_type.convertible_to(ast.type())) {
                self.ctx.errors.add_error(errs_.Error{ .non_convertible = .{ .span = ast.token().span, .from = child_type, .to = ast.type() } });
                return error.CompileError;
            }
            return ast.type();
        },
        .addr_of => {
            // FIXME: High cyclo
            if (expected == null) {
                // Not expecting anything, just validate expr
                const child_type = self.typecheck_AST(ast.expr(), null) catch return error.CompileError;
                try self.validate_L_Value(ast.expr());
                return Type_AST.create_addr_of_type(ast.token(), child_type, ast.addr_of.mut, ast.addr_of.multiptr, self.ctx.allocator());
            } else {
                // Address value, expected must be an address, inner must match with expected's inner
                const expanded_expected = expected.?.expand_identifier();
                if (expanded_expected.* == .dyn_type) {
                    ast.* = ast_.AST.create_dyn_value(
                        ast.token(),
                        expanded_expected,
                        ast.expr(),
                        ast.scope().?,
                        ast.addr_of.mut,
                        self.ctx.allocator(),
                    ).*;
                    return self.typecheck_AST(ast, expected);
                } else if (expanded_expected.* == .anyptr_type) {
                    ast.addr_of.anytptr = true;
                    return expanded_expected;
                } else if (expanded_expected.* != .addr_of) {
                    // Didn't expect an address type. Validate expr and report error
                    return typing_.throw_unexpected_type(ast.token().span, expected.?, poison_.poisoned_type, &self.ctx.errors);
                }
                _ = expanded_expected.child().expand_identifier();

                // Everythings Ok.
                const child_type = self.typecheck_AST(ast.expr(), expanded_expected.child()) catch return error.CompileError;
                if (ast.addr_of.mut) {
                    try self.assert_mutable(ast.expr());
                }
                return Type_AST.create_addr_of_type(ast.token(), child_type, ast.addr_of.mut, ast.addr_of.multiptr, self.ctx.allocator());
            }
        },
        .slice_of => {
            const expr_type = self.typecheck_AST(ast.expr(), null) catch return error.CompileError;

            // ast.expr() must be homotypical tuple type of expected
            if (expr_type.* != .array_of) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{
                    .span = ast.token().span,
                    .msg = "attempt to take slice-of something that is not an array",
                } });
                return error.CompileError;
            }

            if (ast.slice_of.mut) {
                try self.assert_mutable(ast.expr());
            }
            const retval = Type_AST.create_slice_type(expr_type.child(), ast.slice_of.mut, self.ctx.allocator());
            ast.* = ast_.AST.create_slice_value(ast.expr(), ast.slice_of.mut, expr_type, self.ctx.allocator()).*;
            _ = self.typecheck_AST(ast, null) catch return error.CompileError;
            return retval;
        },
        .sub_slice => {
            const super_type = self.typecheck_AST(ast.sub_slice.super, null) catch return error.CompileError;
            const expanded_super_type = super_type.expand_identifier();
            _ = self.typecheck_AST(ast.sub_slice.lower.?, null) catch return error.CompileError; // lower and upper should exist
            _ = self.typecheck_AST(ast.sub_slice.upper.?, null) catch return error.CompileError; // they are set in expand phase
            if (expanded_super_type.* != .struct_type or !expanded_super_type.struct_type.was_slice) {
                self.ctx.errors.add_error(errs_.Error{ .not_subsliceable = .{
                    .span = ast.token().span,
                    ._type = expanded_super_type,
                } });
                return error.CompileError;
            }
            return expanded_super_type;
        },
        .variant_tag => {
            const expr_type: *Type_AST = self.typecheck_AST(ast.expr(), null) catch return error.CompileError;
            const expanded_expr_type = expr_type.expand_identifier();
            if (expanded_expr_type.* != .enum_type) {
                return typing_.throw_wrong_from("enum", "enum value", expanded_expr_type, ast.token().span, &self.ctx.errors);
            }
            return prelude_.word64_type;
        },
        .variant_name => {
            const expr_type: *Type_AST = self.typecheck_AST(ast.expr(), null) catch return error.CompileError;
            const expanded_expr_type = expr_type.expand_identifier();
            if (expanded_expr_type.* != .enum_type) {
                return typing_.throw_wrong_from("enum", "enum value", expanded_expr_type, ast.token().span, &self.ctx.errors);
            }
            return prelude_.string_type;
        },
        .enum_value => {
            if (ast.enum_value.base == null and expected == null) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot infer type for initializer" } });
                return error.CompileError;
            } else if (ast.enum_value.base == null) {
                // Infer that the base type is `expected`
                ast.enum_value.base = expected;
            }

            const expanded_base: *Type_AST = ast.enum_value.base.?.expand_identifier();
            if (expanded_base.* != .enum_type) {
                return typing_.throw_wrong_from("enum", "enum value", expanded_base, ast.token().span, &self.ctx.errors);
            }

            const pos = expanded_base.get_pos(ast.token().data);
            if (pos == null and expanded_base.* == .enum_type) {
                self.ctx.errors.add_error(errs_.Error{ .member_not_in_type = .{ .span = ast.token().span, .identifier = ast.token().data, .name = "enum", .type = expanded_base } });
                return error.CompileError;
            }
            ast.set_pos(expanded_base.get_pos(ast.token().data));

            ast.enum_value.domain = expanded_base.children().items[ast.pos().?];
            if (ast.enum_value.init == null) {
                // This may be usurped by a .call node
                ast.enum_value.init = ast.enum_value.domain.?.annotation.init orelse
                    try defaults_.generate_default(ast.enum_value.domain.?.child(), ast.token().span, &self.ctx.errors, self.ctx.allocator());
            }
            _ = self.typecheck_AST(ast.enum_value.init.?, ast.enum_value.domain.?.child()) catch return error.CompileError;
            return ast.enum_value.base.?;
        },
        .@"if" => {
            // FIXME: High cyclo
            if (ast.@"if".let) |let| {
                _ = self.typecheck_AST(let, null) catch return error.CompileError;
            }

            _ = self.typecheck_AST(ast.@"if".condition, prelude_.bool_type) catch return error.CompileError;

            // expecting a type
            var expected_type: ?*Type_AST = undefined;
            var expanded_expected: *Type_AST = undefined;
            var is_result_optional: bool = false;
            if (expected == null) {
                expected_type = null;
            } else {
                expanded_expected = expected.?.expand_identifier();
                is_result_optional = expanded_expected.* == .enum_type and
                    expanded_expected.enum_type.from == .optional and
                    !expanded_expected.types_match(prelude_.void_type);
                if (ast.else_block() != null) {
                    expected_type = expected.?;
                } else if (is_result_optional) {
                    expected_type = expanded_expected.get_some_type();
                } else {
                    return typing_.throw_unexpected_type(ast.token().span, expected.?, poison_.poisoned_type, &self.ctx.errors);
                }
            }

            const body_type = self.typecheck_AST(ast.body_block(), expected_type) catch return error.CompileError;
            var else_type: ?*Type_AST = null;
            if (ast.else_block() != null) {
                else_type = self.typecheck_AST(ast.else_block().?, expected_type) catch return error.CompileError;
            }
            if (body_type.is_ident_type("Void") and ast.else_block() != null) {
                return else_type.?;
            } else if (ast.else_block() != null or
                prelude_.unit_type.types_match(body_type.expand_identifier()) or
                body_type.expand_identifier().types_match(prelude_.void_type))
            {
                return body_type;
            } else {
                return Type_AST.create_optional_type(body_type, self.ctx.allocator());
            }
        },
        .match => {
            if (ast.match.let != null) {
                _ = self.typecheck_AST(ast.match.let.?, null) catch return error.CompileError;
            }
            const expr_type = self.typecheck_AST(ast.expr(), null) catch return error.CompileError;

            const expanded_expr_type = expr_type.expand_identifier();

            var retval: ?*Type_AST = null;
            for (0..ast.children().items.len) |i| {
                const child_type = self.typecheck_AST(ast.children().items[i], expected) catch return error.CompileError;
                if (!child_type.is_ident_type("Void")) {
                    retval = child_type;
                }
                try self.ctx.validate_pattern.assert_pattern_matches(ast.children().items[i].lhs(), expanded_expr_type);
            }

            try self.ctx.validate_pattern.exhaustive_check(expanded_expr_type, ast.children(), ast.token().span);
            return retval orelse prelude_.void_type;
        },
        .mapping => return self.typecheck_AST(ast.rhs(), expected),
        .@"while" => {
            if (ast.@"while".let) |let| {
                _ = self.typecheck_AST(let, null) catch return error.CompileError;
            }
            if (ast.@"while".post) |post| {
                _ = self.typecheck_AST(post, null) catch return error.CompileError;
            }
            _ = self.typecheck_AST(ast.@"while".condition, prelude_.bool_type) catch return error.CompileError;

            _ = self.typecheck_AST(ast.body_block(), null) catch return error.CompileError;
            if (ast.else_block() != null) {
                _ = self.typecheck_AST(ast.else_block().?, null) catch return error.CompileError;
            }

            return prelude_.unit_type;
        },
        .with => {
            for (ast.with.contexts.items) |child| {
                _ = try self.typecheck_AST(child, null);
            }
            return self.typecheck_AST(ast.with._body_block, expected);
        },
        .block => {
            var last_type: ?*Type_AST = null;
            for (0..ast.children().items.len) |i| {
                const expected_type: ?*Type_AST = if (ast.block.final == null and i == ast.children().items.len - 1) expected else null;
                const statement_type = self.typecheck_AST(ast.children().items[i], expected_type) catch return error.CompileError;
                const statement = ast.children().items[i];
                last_type = statement_type;
                // A middle-statement is any but the last statement if there is no final, or any statement if there is a final
                // Middle statements' type must be unit
                const is_middle_statement = if (ast.block.final == null) i < ast.children().items.len - 1 else true;
                if ((statement.* != .fn_decl or statement.fn_decl.name == null) and is_middle_statement) {
                    // Don't worry about fn_decl's, those should be allowed to be "discarded"
                    const expanded_statement_type = statement_type.expand_identifier();
                    try typing_.middle_statement_check(statement.token().span, expanded_statement_type, &self.ctx.errors);
                }
            }
            if (ast.block.final) |final| {
                _ = self.typecheck_AST(final, expected) catch return error.CompileError;
                return prelude_.void_type;
            } else if (ast.children().items.len == 0) {
                return prelude_.unit_type;
            } else {
                return last_type.?;
            }
        },
        .@"break", .@"continue", .@"unreachable" => return prelude_.void_type,
        .@"return" => {
            const inner_fn_ret_type = ast.symbol().?.type().rhs();
            if (ast.@"return"._ret_expr) |expr| {
                _ = self.typecheck_AST(expr, inner_fn_ret_type) catch return error.CompileError;
            } else if (expected != null and (expected.?.expand_identifier()).* != .unit_type) {
                if (inner_fn_ret_type.expand_identifier().* != .unit_type) {
                    return typing_.throw_unexpected_type(ast.token().span, expected.?, prelude_.void_type, &self.ctx.errors);
                }
            } else if (expected == null and inner_fn_ret_type.expand_identifier().* != .unit_type) {
                return typing_.throw_unexpected_type(ast.token().span, inner_fn_ret_type, prelude_.unit_type, &self.ctx.errors);
            }
            return prelude_.void_type;
        },
        .@"defer", .@"errdefer" => {
            _ = self.typecheck_AST(ast.statement(), null) catch return error.CompileError;
            return prelude_.unit_type;
        },
        .fn_decl => {
            try self.ctx.validate_symbol.validate_symbol(ast.symbol().?);
            return ast.decl_type();
        },
        .method_decl => {
            std.debug.assert(expected == null); // Why wouldn't it be?
            if (ast.symbol() != null) {
                // Not a trait-method
                try self.ctx.validate_symbol.validate_symbol(ast.symbol().?);
            }
            return ast.decl_type();
        },
        .decl => {
            // ast.decl.type = try self.typecheck_AST(ast.decl.type, prelude_.type_type);
            try self.ctx.validate_type.validate_type(ast.decl.type);
            if (ast.decl.init) |_init| {
                _ = self.typecheck_AST(_init, ast.decl.type) catch return error.CompileError;
            }
            try self.ctx.validate_symbol.validate_symbol(ast.decl.name.symbol().?);
            return prelude_.unit_type;
        },
        .context_value_decl => {
            try self.ctx.validate_type.validate_type(ast.context_value_decl.parent);
            if (ast.context_value_decl.init) |_init| {
                _ = self.typecheck_AST(_init, ast.context_value_decl.parent) catch return error.CompileError;
            }
            try self.ctx.validate_symbol.validate_symbol(ast.symbol().?);
            return prelude_.unit_type;
        },
        .binding => {
            try self.ctx.validate_type.validate_type(ast.binding.type);
            if (ast.binding.init) |_init| {
                _ = self.typecheck_AST(_init, ast.binding.type) catch return error.CompileError;
            }
            for (ast.binding.decls.items) |decl| {
                try self.ctx.validate_symbol.validate_symbol(decl.decl.name.symbol().?);
            }
            try self.ctx.validate_pattern.assert_pattern_matches(ast.binding.pattern, ast.binding.type);
            return prelude_.unit_type;
        },
        else => std.debug.panic("compiler error: typecheck_AST() unimplemented for {s}", .{@tagName(ast.*)}),
    }
}

/// Validates an open binary operator. An operator is `open` if it returns a type different from the
/// ones it acts on.
///
/// Returns the type of the validated operator.
pub fn binary_operator_open(
    self: *Self,
    ast: *ast_.AST,
    expected: ?*Type_AST,
) Validate_Error_Enum!*Type_AST {
    const lhs_type = self.typecheck_AST(ast.lhs(), expected) catch return error.CompileError;
    if (ast.lhs().* == .identifier and ast.lhs().symbol().?.is_type()) {
        return error.CompileError;
    }
    _ = self.typecheck_AST(ast.rhs(), lhs_type) catch return error.CompileError;
    return lhs_type;
}

/// Validates just that each argument's type matches its corresponding parameter's type. Assumes arity is valid.
pub fn validate_args_type(
    self: *Self,
    args: *std.array_list.Managed(*ast_.AST),
    expected: *const std.array_list.Managed(*Type_AST),
    variadic: bool,
) Validate_Error_Enum!std.array_list.Managed(*Type_AST) {
    const expected_length = expected.items.len;

    var terms = std.array_list.Managed(*Type_AST).init(self.ctx.allocator());
    errdefer terms.deinit();
    for (0..expected_length) |i| {
        const param_type = expected.items[i];
        const term_type = self.typecheck_AST(args.items[i], param_type) catch return error.CompileError;
        terms.append(term_type) catch unreachable;
    }
    if (variadic) {
        for (expected_length..args.items.len) |i| {
            const term_type = self.typecheck_AST(args.items[i], null) catch return error.CompileError;
            terms.append(term_type) catch unreachable;
        }
    }
    return terms;
}

fn validate_context(self: *Self, function_type: *Type_AST, ast: *ast_.AST) Validate_Error_Enum!void {
    for (function_type.function.contexts.items) |context| {
        var fn_ctx = context;
        const symbol = ast.scope().?.context_lookup(fn_ctx, self.ctx) orelse {
            if (fn_ctx.* == .addr_of) {
                fn_ctx = fn_ctx.child();
            }
            self.ctx.errors.add_error(errs_.Error{ .missing_context = .{
                .span = ast.token().span,
                .context = fn_ctx,
            } });
            return error.CompileError;
        };
        try ast.context_args().append(symbol);
    }
}

fn validate_L_Value(self: *Self, ast: *ast_.AST) Validate_Error_Enum!void {
    switch (ast.*) {
        .select, .identifier, .array_value, .context_value => {},

        .dereference => if (ast.expr().* != .addr_of) {
            try self.validate_L_Value(ast.expr());
        },

        .index => try self.validate_L_Value(ast.lhs()),

        .tuple_value => for (ast.children().items) |term| {
            try self.validate_L_Value(term);
        },

        else => {
            self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "not an l-value" } });
            return error.CompileError;
        },
    }
}

fn implicit_dereference(self: *Self, ast: *ast_.AST, old_lhs_type: *Type_AST) Validate_Error_Enum!*Type_AST {
    var lhs_type = old_lhs_type;
    if (lhs_type.* == .addr_of and !lhs_type.addr_of.multiptr) {
        ast.set_lhs(ast_.AST.create_dereference(ast.token(), ast.lhs(), self.ctx.allocator()));
        lhs_type = (self.typecheck_AST(ast.lhs(), null) catch return error.CompileError).expand_identifier();
    }
    try poison_.assert_none_poisoned(.{ ast.lhs(), lhs_type });
    return lhs_type;
}

fn assert_mutable(self: *Self, ast: *ast_.AST) Validate_Error_Enum!void {
    switch (ast.*) {
        .identifier => {
            const symbol = ast.symbol().?;
            if (!std.mem.eql(u8, symbol.name, "_") and symbol.kind != .mut) {
                self.ctx.errors.add_error(errs_.Error{ .modify_immutable = .{ .identifier = ast.token() } });
                return error.CompileError;
            }
        },

        .dereference => {
            const expr_expanded_type = self.typeof(ast.expr()).expand_identifier();
            try self.assert_mutable_address(expr_expanded_type);
        },

        .index => {
            const lhs_type = self.typeof(ast.lhs());
            if (lhs_type.* == .struct_type and lhs_type.struct_type.was_slice) {
                try self.assert_mutable_address(lhs_type.children().items[0].child());
            } else if (lhs_type.* == .addr_of and lhs_type.addr_of.multiptr) {
                try self.assert_mutable_address(lhs_type);
            } else {
                try self.assert_mutable(ast.lhs());
            }
        },

        .tuple_value => for (ast.children().items) |term| {
            try self.assert_mutable(term);
        },

        .select => try self.assert_mutable(ast.lhs()),

        else => {},
    }
}

fn assert_mutable_address(self: *const Self, ast: *Type_AST) Validate_Error_Enum!void {
    if (!ast.addr_of.mut) {
        self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot modify non-mutable address" } });
        return error.CompileError;
    }
}

fn get_expanded_expected(expected: ?*Type_AST) ?*Type_AST {
    return if (expected == null) null else expected.?.expand_identifier();
}
