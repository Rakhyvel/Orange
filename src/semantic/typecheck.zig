//! This file contains logic for validating an AST.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const args_ = @import("args.zig");
const Const_Eval = @import("../semantic/const_eval.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const core_ = @import("../hierarchy/core.zig");
const Decorate = @import("../ast/decorate.zig");
const Type_Decorate = @import("../ast/type_decorate.zig");
const errs_ = @import("../util/errors.zig");
const fmt_ = @import("../util/fmt.zig");
const poison_ = @import("../ast/poison.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Scope = @import("../symbol/scope.zig");
const typing_ = @import("typing.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const walk_ = @import("../ast/walker.zig");
const unification_ = @import("../types/unification.zig");
const union_fields_ = @import("../util/union_fields.zig");
const generic_apply_ = @import("../ast/generic_apply.zig");

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

pub fn deinit(self: *Self) void {
    self.map.deinit();
}

fn clone(self: *const Self) !Self {
    var new_self = Self.init(self.ctx);
    new_self.map = try self.map.clone();
    return new_self;
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
pub fn typecheck_AST(
    self: *Self,
    ast: *ast_.AST,
    expected: ?*Type_AST,
    subst: *unification_.Substitutions,
) Validate_Error_Enum!*Type_AST {
    // TODO: Bit long
    if (ast.common().validation_state == .validating) {
        // std.debug.print("{f}\n", .{ast});
        self.ctx.errors.add_error(errs_.Error{ .recursive_definition = .{
            .span = ast.token().span,
            .symbol_name = null,
        } });
        return error.CompileError;
    } else if (ast.common().validation_state != .unvalidated and self.map.contains(ast)) {
        const res = self.typeof(ast);
        if (res.* == .poison) std.debug.panic("{f}", .{ast});
        return res;
    }

    var expected_type = expected;

    // std.debug.print("{f}: {?f}\n", .{ ast, expected_type });
    ast.common().validation_state = .validating;

    if (expected_type != null and expected_type.?.* == .poison) {
        expected_type = null;
    }
    if (expected_type) |_| {
        try walk_.walk_type(expected_type, Decorate.new(self.ctx));
        try walk_.walk_type(expected_type, Type_Decorate.new(self.ctx));
    }

    var actual_type = self.typecheck_AST_internal(ast, expected_type, subst) catch |e| {
        ast.common().validation_state = .invalid;
        return e;
    };

    try walk_.walk_type(actual_type, Decorate.new(self.ctx));

    if (expected_type) |_| {
        actual_type = try self.insert_coercion(ast, actual_type, expected_type.?, subst);
        const expanded_expected = expected_type.?.expand_identifier();
        if (expanded_expected.* != .unit_type or (actual_type.* == .enum_type and actual_type.enum_type.from == .@"error"))
            typing_.type_check(ast.token().span, actual_type, expected_type.?, subst, &self.ctx.errors) catch |e| {
                ast.common().validation_state = .invalid;
                return e;
            };
    }

    if (actual_type.* != .poison) {
        try walk_.walk_type(actual_type, Type_Decorate.new(self.ctx));
        self.map.put(ast, actual_type) catch unreachable;
        ast.common().validation_state = .valid;
    } else {
        ast.common().validation_state = .invalid;
        return error.CompileError;
    }

    return actual_type;
}

var depth: usize = 0;
fn typecheck_AST_internal(self: *Self, ast: *ast_.AST, expected: ?*Type_AST, subst: *unification_.Substitutions) Validate_Error_Enum!*Type_AST {
    // TODO: Ugh this function is too long
    depth += 1;
    // std.debug.print("[{}] {f}: {?f}\n", .{ depth, ast, expected });
    // defer std.debug.print("^[{}] {f}: {?f}\n", .{ depth, ast, expected });
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
            // look up the symbol, it contains the type
            const symbol = try Decorate.symbol(ast, self.ctx);
            if (symbol.validation_state == .invalid) {
                return error.CompileError;
            }
            if (symbol.kind == .@"const") {
                try self.ctx.validate_symbol.validate_symbol(symbol);
            }
            const ref = Type_AST.create_type_identifier(ast.token(), self.ctx.allocator());
            ref.set_symbol(symbol);
            if (try ref.resolve_ability_reference(self.ctx)) |resolved| {
                const ability_val_symbol = try ast.scope().?.ability_lookup(resolved, self.ctx) orelse {
                    self.ctx.errors.add_error(errs_.Error{ .missing_ability = .{
                        .span = ast.token().span,
                        .ability = resolved,
                    } });
                    return error.CompileError;
                };
                ast.set_symbol(ability_val_symbol);
                if (resolved.symbol().?.init_typedef() != null) {
                    return resolved.symbol().?.init_typedef().?.expand_identifier().child();
                }
            }
            if (symbol.is_type() or symbol.kind == .ability) {
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
            const _type = symbol.type();
            try self.ctx.validate_type.validate_type(_type);
            return _type;
        },
        .access => {
            // Structural-type receivers arent values, tolerate their value-typecheck failure
            const lhs_is_structural = !union_fields_.has_struct_field(ast.lhs().*, "_symbol");
            _ = self.typecheck_AST(ast.lhs(), null, subst) catch |e| switch (e) {
                error.UnexpectedTypeType => {}, // This is expected
                error.CompileError => if (!lhs_is_structural) return e,
                else => return e,
            };

            // look up symbol, that's the type
            const symbol = try Decorate.symbol(ast, self.ctx);
            if (symbol.validation_state == .invalid) {
                return error.CompileError;
            }
            // try self.ctx.validate_symbol.validate_symbol(symbol);

            if (symbol.decl.?.num_generic_params() > 0) {
                self.ctx.errors.add_error(errs_.Error{ .unapplied_generic = .{
                    .span = ast.token().span,
                    .symbol_name = symbol.name,
                    .num_generics = symbol.decl.?.num_generic_params(),
                } });
                return error.CompileError;
            }
            const ref = Type_AST.create_type_identifier(ast.token(), self.ctx.allocator());
            ref.set_symbol(symbol);
            if (try ref.resolve_ability_reference(self.ctx)) |resolved| {
                const ability_val_symbol = try ast.scope().?.ability_lookup(resolved, self.ctx) orelse {
                    self.ctx.errors.add_error(errs_.Error{ .missing_ability = .{
                        .span = ast.token().span,
                        .ability = resolved,
                    } });
                    return error.CompileError;
                };
                ast.set_symbol(ability_val_symbol);
                return resolved.symbol().?.init_typedef().?.expand_identifier().child();
            }

            if (symbol.is_type()) {
                if (expected != null) {
                    self.ctx.errors.add_error(errs_.Error{ .unexpected_type_type = .{ .expected = expected, .span = ast.token().span } });
                    return error.CompileError;
                } else {
                    return prelude_.unit_type;
                }
            } else {
                const _type = symbol.type();
                try self.ctx.validate_type.validate_type(_type);
                return _type;
            }
        },
        .type_access => {
            const lhs_is_trait_ident = (ast.type_access._lhs_type.* == .identifier or ast.type_access._lhs_type.* == .generic_apply) and
                ast.type_access._lhs_type.symbol() != null and
                ast.type_access._lhs_type.symbol().?.kind == .trait;

            if (!lhs_is_trait_ident) {
                // Trait::method uses a trait identifier as lhs_type. Traits are not types so skip validation.
                try self.ctx.validate_type.validate_type(ast.type_access._lhs_type);
            }

            // look up symbol, that's the type
            const symbol = try Decorate.symbol(ast, self.ctx);
            if (symbol.validation_state == .invalid) {
                return error.CompileError;
            }
            if (symbol.kind == .@"const") {
                try self.ctx.validate_symbol.validate_symbol(symbol);
            }

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
                const _type = symbol.type();
                try self.ctx.validate_type.validate_type(_type);
                return _type;
            }
        },
        .true, .false => return prelude_.bool_type,
        .not => {
            _ = self.typecheck_AST(ast.expr(), prelude_.bool_type, subst) catch return error.CompileError;
            return prelude_.bool_type;
        },
        .negate => {
            const expr_type = self.typecheck_AST(ast.expr(), expected, subst) catch return error.CompileError;
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

            const expr_type = self.typecheck_AST(ast.expr(), expr_expected_type, subst) catch return error.CompileError;
            const expanded_expr_type = expr_type.expand_identifier();
            if (expanded_expr_type.* != .addr_of) {
                return typing_.throw_wrong_from("address", "dereference", expanded_expr_type, expr_span, &self.ctx.errors);
            } else {
                return expanded_expr_type.child();
            }
        },
        .@"try" => {
            const expr_span = ast.expr().token().span;
            const expr_type = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;
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
        .size_of => {
            try self.ctx.validate_type.validate_type(ast.size_of._type);
            return prelude_.int_type;
        },
        .@"comptime" => {
            const expr_type = self.typecheck_AST(ast.expr(), expected, subst) catch return error.CompileError;
            const expr_expanded_type = expr_type.expand_identifier();
            if (expr_expanded_type.types_match(prelude_.void_type)) {
                return typing_.throw_unexpected_void_type(ast.expr().token().span, &self.ctx.errors);
            }
            return expr_type;
        },
        .assign => {
            try self.validate_L_Value(ast.lhs());
            const lhs_type = self.typecheck_AST(ast.lhs(), null, subst) catch return error.CompileError;
            const rhs_expected: ?*Type_AST = if (ast.lhs().* == .identifier and std.mem.eql(u8, ast.lhs().token().data, "_")) null else lhs_type;
            _ = self.typecheck_AST(ast.rhs(), rhs_expected, subst) catch return error.CompileError;
            try self.assert_mutable(ast.lhs());
            return prelude_.unit_type;
        },
        .@"or", .@"and" => {
            _ = self.typecheck_AST(ast.lhs(), prelude_.bool_type, subst) catch return error.CompileError;
            _ = self.typecheck_AST(ast.rhs(), prelude_.bool_type, subst) catch return error.CompileError;
            return prelude_.bool_type;
        },
        .add, .sub, .mult, .div => {
            // These operators are open, since they allow for polymorphism between their operands.
            const lhs_type = try self.binary_operator_open(ast, expected, subst);
            const expanded_lhs_type = lhs_type.expand_identifier();
            try typing_.type_check_arithmetic(ast.token().span, expanded_lhs_type, &self.ctx.errors);
            return expanded_lhs_type;
        },
        .mod => {
            const lhs_type = try self.binary_operator_open(ast, null, subst);
            try typing_.type_check_integral(ast.token().span, lhs_type, &self.ctx.errors);
            return lhs_type;
        },
        .greater, .lesser, .greater_equal, .lesser_equal => {
            const lhs_type = try self.binary_operator_open(ast, null, subst);
            try typing_.type_check_ord(ast.token().span, lhs_type, &self.ctx.errors);
            return prelude_.bool_type;
        },
        .@"catch", .@"orelse" => {
            const lhs_span = ast.lhs().token().span;
            const lhs_type = self.typecheck_AST(ast.lhs(), null, subst) catch return error.CompileError;
            _ = self.typecheck_AST(ast.rhs(), expected, subst) catch return error.CompileError;
            var expanded_lhs_type = lhs_type.expand_identifier();
            try typing_.coalesce_operator(expanded_lhs_type, ast, lhs_span, &self.ctx.errors);
            return expanded_lhs_type.get_nominal_type().child();
        },
        .bit_not => {
            const expr_type = self.typecheck_AST(ast.expr(), expected orelse prelude_.word64_type, subst) catch return error.CompileError;
            try typing_.type_check_bits(ast.token().span, expr_type, &self.ctx.errors);
            return expr_type;
        },
        .left_shift, .right_shift, .bit_and, .bit_or, .bit_xor => {
            const lhs_type = self.typecheck_AST(ast.lhs(), expected orelse prelude_.word64_type, subst) catch return error.CompileError;
            _ = self.typecheck_AST(ast.rhs(), lhs_type, subst) catch return error.CompileError;
            try typing_.type_check_bits(ast.token().span, lhs_type, &self.ctx.errors);
            return lhs_type;
        },
        .call => {
            const lhs_span = ast.lhs().token().span;
            if (ast.lhs().* == .type_access) {
                // FQS call, overwrite abstract symbol with concrete impl, lhs_type stays identifier(Trait) to avoid nested as_trait on monomorphized clones
                const lhs_type_node = ast.lhs().type_access._lhs_type;
                if ((lhs_type_node.* == .identifier or lhs_type_node.* == .access or lhs_type_node.* == .generic_apply) and lhs_type_node.symbol() != null and lhs_type_node.symbol().?.kind == .trait) {
                    // Search for where the Self type should be extracted given the trait method decl
                    const trait_decl = lhs_type_node.symbol().?.decl.?;
                    const method_name = ast.lhs().rhs().token().data;
                    const res = try find_self_type(trait_decl, method_name);
                    if (res == null) {
                        self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot select method implementation, trait method's signature does not mention `Self`" } });
                        return error.CompileError;
                    }

                    if (res.? == .param and res.?.param >= ast.children().items.len) {
                        self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "trait method call requires at least one argument" } });
                        return error.CompileError;
                    }

                    // Extract the receiver type based on where the trait told us it should be
                    const receiver_type = if (res.? == .return_value) (if (expected) |exp|
                        exp
                    else {
                        self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot infer `Self` from call site without type annotation" } });
                        return error.CompileError;
                    }) else self.typecheck_AST(ast.children().items[res.?.param], null, subst) catch return error.CompileError;

                    // Strip as_trait (monomorphization wrapper) and addr_of (explicit &self receiver
                    // in FQS calls like Trait::method(&x, ...)) to reach the base type for impl lookup.
                    var concrete_type = receiver_type;
                    while (concrete_type.* == .as_trait) concrete_type = concrete_type.lhs();
                    if (concrete_type.* == .addr_of) concrete_type = concrete_type.child();
                    var candidate_methods = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(self.ctx.allocator());
                    defer candidate_methods.deinit();
                    var constraints_arr = [1]*Type_AST{lhs_type_node};
                    try Scope.as_trait_member_lookup(concrete_type, &constraints_arr, method_name, &candidate_methods, self.ctx);
                    if (candidate_methods.keys().len == 0) {
                        self.ctx.errors.add_error(errs_.Error{ .type_not_impl_method = .{ .span = ast.token().span, .method_name = method_name, ._type = concrete_type.strip_as_trait(), .candidates = null } });
                        return error.CompileError;
                    }
                    ast.lhs().set_symbol(candidate_methods.keys()[0].symbol().?);
                }
            }

            if (ast.lhs().* == .type_access and ast.lhs().type_access._lhs_type.* == .as_trait) {
                const as_trait_node = ast.lhs().type_access._lhs_type;
                // Auto-deref the receiver type to its base type
                // This lets stuff like `(@typeof(self) as Index)::index(self, i)` work when `self: &[]T`
                var base = as_trait_node.lhs();
                while (true) {
                    const exp = base.expand_identifier();
                    if (exp.* == .addr_of and !exp.addr_of.multiptr) {
                        base = exp.child();
                    } else break;
                }
                as_trait_node.as_trait._lhs = base;

                const for_type = as_trait_node.lhs();
                try self.ctx.validate_type.validate_type(for_type);
                var candidate_method_decls = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(self.ctx.allocator());
                defer candidate_method_decls.deinit();

                const method_name = ast.lhs().rhs().token().data;

                try Scope.as_trait_member_lookup(for_type, as_trait_node.as_trait.constraints.items, method_name, &candidate_method_decls, self.ctx);

                if (candidate_method_decls.keys().len == 0) {
                    self.ctx.errors.add_error(errs_.Error{
                        .type_not_impl_trait = .{
                            .span = ast.token().span,
                            .trait_name = as_trait_node.as_trait.constraints.items[0].symbol().?.name, // TODO: This only does the first one, if there are many it'd be confusing
                            ._type = for_type.strip_as_trait(),
                        },
                    });
                    return error.CompileError;
                }
            }
            var lhs_type = self.typecheck_AST(ast.lhs(), null, subst) catch return error.CompileError;
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

            var fn_type = expanded_lhs_type;
            if (ast.lhs().* == .generic_apply) {
                const callee_sym = ast.lhs().symbol().?;
                const callee_params = callee_sym.decl.?.generic_params().items;
                if (callee_params.len > 0) {
                    // monomorphize early-returned the template
                    var s = unification_.Substitutions.init(self.ctx.allocator());
                    var param_i: usize = 0;
                    for (ast.lhs().generic_apply._children.items) |arg| {
                        switch (arg) {
                            .type_arg => |ty| {
                                if (ty.* == .eq_constraint) continue;
                                s.put_type(callee_params[param_i].symbol().?.name, ty) catch unreachable;
                                param_i += 1;
                            },
                            .const_arg => |v| {
                                s.put_const(callee_params[param_i].symbol().?.name, v) catch unreachable;
                                param_i += 1;
                            },
                        }
                    }
                    fn_type = expanded_lhs_type.clone(&s, self.ctx.allocator());
                }
            }
            try self.validate_ability(fn_type, ast);

            const domain = fn_type.function.args;
            const codomain = fn_type.rhs();
            const variadic = fn_type.function.variadic;

            // Peel any extra addrof off an FQS receiver
            if (ast.lhs().* == .type_access and domain.items.len > 0 and ast.children().items.len > 0) {
                const p0 = domain.items[0];
                if (p0.* == .annotation and p0.annotation.pattern.* == .receiver and
                    p0.child().* == .addr_of and !p0.child().addr_of.multiptr)
                {
                    try self.normalize_addr_receiver(ast.children(), subst);
                }
            }

            var args_validator = args_.init(.function, ast.children(), ast.token().span, &domain, &self.ctx.errors, self.ctx.allocator());
            args_validator.set_variadic(variadic);
            ast.set_children(try args_validator.fill_default_args());
            args_validator.implicit_ref();
            try args_validator.validate_arity();
            try args_validator.validate_type(subst, self.ctx);
            // Resolve associated type returns to concrete type when receiver is concrete. Keep as_trait constraint, so indexed monomorph elements stay bound for its trait dispatch
            if (codomain.* == .access) return codomain.expand_keep_constraint();
            if (codomain.* == .addr_of and codomain.child().* == .access) {
                return Type_AST.create_addr_of_type(codomain.token(), codomain.child().expand_keep_constraint(), codomain.addr_of.mut, codomain.addr_of.multiptr, self.ctx.allocator());
            }
            return codomain;
        },
        .child_addr, .child_addr_mut => {
            const is_mut = ast.* == .child_addr_mut;
            const lhs_span = ast.lhs().token().span;
            const ptr_type = self.typecheck_AST(ast.lhs(), null, subst) catch return error.CompileError;
            _ = self.typecheck_AST(ast.rhs(), prelude_.int_type, subst) catch return error.CompileError;

            const expanded = ptr_type.expand_identifier();
            if (expanded.* != .addr_of) {
                self.ctx.errors.add_error(errs_.Error{ .not_indexable = .{ .span = lhs_span, ._type = expanded } });
                return error.CompileError;
            }
            if (is_mut and !expanded.addr_of.mut) {
                self.ctx.errors.add_error(errs_.Error{ .not_indexable = .{ .span = lhs_span, ._type = expanded } });
                return error.CompileError;
            }

            var elem_type: *Type_AST = undefined;
            if (expanded.addr_of.multiptr) {
                elem_type = expanded.child();
            } else {
                const pointee = expanded.child().expand_identifier();
                if (pointee.* == .array_of) {
                    elem_type = pointee.child();
                } else if (pointee.* == .tuple_type) {
                    try walk_.walk_ast(ast.rhs(), Const_Eval.new(self.ctx));
                    if (ast.rhs().* != .int) {
                        self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "not a constant integer" } });
                        return error.CompileError;
                    }
                    const pos = ast.rhs().int.data;
                    if (pos < 0 or pos >= pointee.children().items.len) {
                        self.ctx.errors.add_error(errs_.Error{ .bad_index = .{
                            .span = ast.token().span,
                            ._type = pointee,
                            .index = pos,
                            .length = pointee.children().items.len,
                        } });
                        return error.CompileError;
                    }
                    elem_type = pointee.children().items[@as(usize, @intCast(pos))];
                    while (elem_type.* == .annotation) {
                        elem_type = elem_type.child();
                    }
                } else {
                    self.ctx.errors.add_error(errs_.Error{ .not_indexable = .{ .span = lhs_span, ._type = expanded } });
                    return error.CompileError;
                }
            }
            return Type_AST.create_addr_of_type(ast.token(), elem_type, is_mut, false, self.ctx.allocator());
        },
        .generic_apply => {
            // look up symbol, that's the type
            const symbol = try Decorate.symbol(ast, self.ctx);
            if (symbol.validation_state == .invalid) {
                return error.CompileError;
            }
            try self.ctx.validate_symbol.validate_symbol(symbol);
            if (symbol.is_type() or symbol.kind == .ability) {
                if (expected != null) {
                    self.ctx.errors.add_error(errs_.Error{ .unexpected_type_type = .{ .expected = expected, .span = ast.token().span } });
                    return error.CompileError;
                }
                return error.UnexpectedTypeType;
            }
            const _type = symbol.type();
            try self.ctx.validate_type.validate_type(_type);
            return _type;
        },
        .positional_select => {
            const lhs_type = self.typecheck_AST(ast.lhs(), null, subst) catch return error.CompileError;
            const rhs_type = self.typecheck_AST(ast.rhs(), null, subst) catch return error.CompileError;
            const expanded_lhs_type = try self.implicit_dereference(ast, lhs_type.expand_identifier(), subst);

            try typing_.type_check_integral(ast.token().span, rhs_type, &self.ctx.errors);
            try walk_.walk_ast(ast.rhs(), Const_Eval.new(self.ctx));

            if (expanded_lhs_type.* != .tuple_type and expanded_lhs_type.* != .struct_type) {
                return typing_.throw_wrong_from("tuple or struct", "positional select", expanded_lhs_type, ast.lhs().token().span, &self.ctx.errors);
            }
            if (ast.rhs().* != .int) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "not a constant integer" } });
                return error.CompileError;
            }

            const evald_pos = ast.rhs().int.data;

            if (evald_pos < 0 or evald_pos >= expanded_lhs_type.children().items.len) {
                self.ctx.errors.add_error(errs_.Error{ .bad_index = .{
                    .span = ast.token().span,
                    ._type = lhs_type,
                    .index = evald_pos,
                    .length = expanded_lhs_type.children().items.len,
                } });
                return error.CompileError;
            }
            ast.set_pos(@intCast(evald_pos));
            var retval = expanded_lhs_type.children().items[ast.pos().?];
            while (retval.* == .annotation) {
                retval = retval.child();
            }
            return retval;
        },
        .select => {
            const lhs_type = self.typecheck_AST(ast.lhs(), null, subst) catch return error.CompileError;
            const expanded_lhs_type = try self.implicit_dereference(ast, lhs_type.expand_identifier(), subst);

            if (ast.pos() == null) {
                ast.set_pos(try typing_.find_select_pos(expanded_lhs_type, ast.rhs().token().data, ast.token().span, &self.ctx.errors));
            }

            if (ast.pos().? < 0 or ast.pos().? >= expanded_lhs_type.children().items.len) {
                self.ctx.errors.add_error(errs_.Error{ .bad_index = .{
                    .span = ast.token().span,
                    ._type = lhs_type,
                    .index = ast.pos().?,
                    .length = expanded_lhs_type.children().items.len,
                } });
                return error.CompileError;
            }
            var retval = expanded_lhs_type.children().items[ast.pos().?];
            while (retval.* == .annotation) {
                retval = retval.child();
            }
            return retval;
        },

        .invoke => {
            // true_lhs_type is lhs's type
            const true_lhs_type = self.typecheck_AST(ast.lhs(), null, subst) catch return error.CompileError;
            // method_decl is the method_decl AST of the method being invoked
            var method_decl: ?*ast_.AST = undefined;
            var candidate_method_decls = std.array_hash_map.AutoArrayHashMap(*ast_.AST, void).init(self.ctx.allocator());
            defer candidate_method_decls.deinit();
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
                if (method_decl == null) {
                    self.ctx.errors.add_error(errs_.Error{
                        .type_not_impl_method = .{
                            .span = ast.token().span,
                            .method_name = ast.rhs().token().data,
                            ._type = true_lhs_type.strip_as_trait().strip_addrs(),
                            .candidates = null,
                        },
                    });
                    return error.CompileError;
                }
                try candidate_method_decls.put(method_decl.?, void{});
            } else {
                // The receiver is a regular type. STRIP AWAY 1 (non-multiptr) ADDR only!
                const lhs_type = if (true_lhs_type.* == .addr_of and !true_lhs_type.addr_of.multiptr) true_lhs_type.child() else true_lhs_type;
                try self.ctx.validate_type.validate_type(lhs_type);
                if (lhs_type.* == .as_trait) {
                    // Dispatch through the constraint traits, so a monomorph carrying `(Concrete as C)` stays bound to C instead of any same-named method the concrete type also has
                    try Scope.as_trait_member_lookup(lhs_type.lhs(), lhs_type.as_trait.constraints.items, ast.rhs().token().data, &candidate_method_decls, self.ctx);
                    if (candidate_method_decls.keys().len == 0) {
                        // Fall back to a broad lookup for methods not covered by the constraints
                        try ast.scope().?.lookup_impl_member(lhs_type.lhs(), ast.rhs().token().data, &candidate_method_decls, false, self.ctx);
                    }
                } else {
                    try ast.scope().?.lookup_impl_member(lhs_type, ast.rhs().token().data, &candidate_method_decls, false, self.ctx);
                }
                if (!lhs_type.is_type_param() and lhs_type.* != .as_trait) {
                    var i: usize = 0;
                    while (i < candidate_method_decls.keys().len) {
                        const candidate_method_decl = candidate_method_decls.keys()[i];
                        if (candidate_method_decl.* != .method_decl or candidate_method_decl.method_decl.init == null) {
                            _ = candidate_method_decls.swapRemove(candidate_method_decl);
                        } else {
                            i += 1;
                        }
                    }
                }
            }
            // Try to find the method that fits, and get the impl substitutions needed to make it work
            const method_selection = try self.select_method(ast, expected, &candidate_method_decls, true_lhs_type, subst);
            method_decl = ast.invoke.method_decl;
            const method_decl_type = method_selection.method_type;

            const domain = method_decl_type.function.args;
            try self.validate_ability(method_decl_type, ast);

            var args_validator = args_.init(.method, ast.children(), ast.token().span, &domain, &self.ctx.errors, self.ctx.allocator());
            ast.set_children(try args_validator.fill_default_args());
            try args_validator.validate_arity();
            try args_validator.validate_type(method_selection.subst, self.ctx);

            return method_decl_type.function._rhs;
        },
        .dyn_value => {
            var expr_type = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;
            while (expr_type.* == .addr_of) {
                ast.set_expr(ast_.AST.create_dereference(ast.token(), ast.expr(), self.ctx.allocator()));
                expr_type = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;
            }
            try self.ctx.validate_type.validate_type(ast.dyn_value.dyn_type);
            const expanded_expr_type = expr_type.expand_identifier();

            if (expanded_expr_type.* == .dyn_type) {
                // Expected type is a potential upcast to a dyn type
                if (!expanded_expr_type.child().is_sub_trait(ast.dyn_value.dyn_type.child())) {
                    self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                        .span = ast.token().span,
                        .trait_name = ast.dyn_value.dyn_type.child().symbol().?.name,
                        ._type = expr_type,
                    } });
                }
                if (ast.dyn_value.mut and !expanded_expr_type.dyn_type.mut) {
                    self.ctx.errors.add_error(errs_.Error{ .modify_immutable = .{ .identifier = ast.token() } });
                    return error.CompileError;
                }
            } else if (expr_type.* == .identifier and expr_type.symbol() != null and expr_type.symbol().?.decl.?.* == .type_param_decl) {
                // Expected type is a type param
                if (ast.dyn_value.mut) {
                    try self.assert_mutable(ast.expr());
                }
                const type_param_decl = expr_type.symbol().?.decl.?;
                var found_constraint = false;
                // Check to see if the type parameter has the constraint type that we're looking for
                for (type_param_decl.type_param_decl.constraints.items) |constraint| {
                    if (Type_AST.is_sub_trait(constraint, ast.dyn_value.dyn_type.child())) {
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
                // Expected type is a concrete type
                if (ast.dyn_value.mut) {
                    try self.assert_mutable(ast.expr());
                }
                const impl = try Scope.impl_trait_lookup(expr_type, ast.dyn_value.dyn_type.child().symbol().?, self.ctx);
                if (impl.impl_ast == null) {
                    self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                        .span = ast.token().span,
                        .trait_name = ast.dyn_value.dyn_type.child().symbol().?.name,
                        ._type = expr_type,
                    } });
                    return error.CompileError;
                }
                ast.dyn_value.impl = impl.impl_ast;
            }

            return Type_AST.create_dyn_type(ast.token(), ast.dyn_value.dyn_type.child(), ast.dyn_value.mut, self.ctx.allocator());
        },
        .ability_value => {
            // Canonicalzie alias spellings
            if (try ast.ability_value.parent.resolve_ability_reference(self.ctx)) |resolved| {
                ast.ability_value.parent.* = resolved.*;
            }
            try self.ctx.validate_type.validate_type(ast.ability_value.parent);
            const expanded_expected: *Type_AST = ast.ability_value.parent.expand_identifier();
            if (ast.ability_value.parent.expand_identifier().* != .ability_type) {
                // Parent wasn't even an ability type!
                return typing_.throw_wrong_from("ability", "ability value", ast.ability_value.parent, ast.token().span, &self.ctx.errors);
            }
            _ = try self.typecheck_AST(ast.expr(), expanded_expected.child(), subst);
            if (expected != null and !ast.ability_value.parent.types_match(expected.?)) {
                return typing_.throw_unexpected_type(ast.token().span, expanded_expected.child(), ast.ability_value.parent, &self.ctx.errors);
            }
            return ast.ability_value.parent;
        },
        .struct_value => {
            try self.ctx.validate_type.validate_type(ast.struct_value.parent);
            const expanded_expected: *Type_AST = if (expected == null) ast.struct_value.parent.expand_identifier() else expected.?.expand_identifier();
            if (expanded_expected.* == .struct_type) {
                // Expecting ast to be a product value of some product type
                var args_validator = args_.init(.@"struct", ast.children(), ast.token().span, expanded_expected.children(), &self.ctx.errors, self.ctx.allocator());
                ast.set_children(try args_validator.fill_default_args());
                try args_validator.validate_arity();
                try args_validator.validate_type(subst, self.ctx);
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
                    const term_type = self.typecheck_AST(ast.children().items[i], null, subst) catch return error.CompileError;
                    terms.append(term_type) catch unreachable;
                }
            } else if (expanded_expected != null and expanded_expected.?.* == .tuple_type) {
                // Expecting ast to be a product value of some product type
                var args_validator = args_.init(.tuple, ast.children(), ast.token().span, expanded_expected.?.children(), &self.ctx.errors, self.ctx.allocator());
                try args_validator.validate_arity();
                try args_validator.validate_type(subst, self.ctx);
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
                const first_type = self.typecheck_AST(ast.children().items[0], elem_type, subst) catch return error.CompileError;
                for (ast.children().items[1..]) |child| {
                    _ = self.typecheck_AST(child, first_type, subst) catch return error.CompileError;
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
            _ = try self.typecheck_AST(ast.expr(), ast.type(), subst);
            return ast.type();
        },
        .addr_of => {
            // FIXME: High cyclo
            if (expected == null) {
                // Not expecting anything, just validate expr
                const child_type = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;
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
                    return self.typecheck_AST(ast, expected, subst);
                } else if (expanded_expected.* == .anyptr_type) {
                    ast.addr_of.anytptr = true;
                    return expanded_expected;
                } else if (expanded_expected.* == .struct_type and expanded_expected.struct_type.was_slice) {
                    const expr_type = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;

                    // ast.expr() must be array type of expected
                    if (expr_type.* != .array_of) {
                        self.ctx.errors.add_error(errs_.Error{ .basic = .{
                            .span = ast.token().span,
                            .msg = "attempt to take slice-of something that is not an array",
                        } });
                        return error.CompileError;
                    }

                    if (ast.addr_of.mut) {
                        try self.assert_mutable(ast.expr());
                    }
                    const retval = Type_AST.create_slice_type(expr_type.child(), ast.addr_of.mut, self.ctx.allocator());
                    ast.* = ast_.AST.create_slice_value(ast.expr(), ast.addr_of.mut, expr_type, self.ctx.allocator()).*;
                    _ = self.typecheck_AST(ast, null, subst) catch return error.CompileError;
                    return retval;
                } else if (expanded_expected.* != .addr_of) {
                    // Didn't expect an address type. Validate expr and report error
                    return typing_.throw_unexpected_type(ast.token().span, expected.?, poison_.poisoned_type, &self.ctx.errors);
                }
                _ = expanded_expected.child().expand_identifier();

                // Everythings Ok.
                const child_type = self.typecheck_AST(ast.expr(), expanded_expected.child(), subst) catch return error.CompileError;
                if (ast.addr_of.mut and !child_type.is_indirect_mutable()) {
                    try self.assert_mutable(ast.expr());
                }
                return Type_AST.create_addr_of_type(ast.token(), child_type, ast.addr_of.mut, ast.addr_of.multiptr, self.ctx.allocator());
            }
        },
        .variant_tag => {
            const expr_type: *Type_AST = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;
            const expanded_expr_type = expr_type.expand_identifier();
            if (expanded_expr_type.* != .enum_type) {
                return typing_.throw_wrong_from("enum", "enum value", expanded_expr_type, ast.token().span, &self.ctx.errors);
            }
            return prelude_.word64_type;
        },
        .variant_name => {
            const expr_type: *Type_AST = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;
            const expanded_expr_type = expr_type.expand_identifier();
            if (expanded_expr_type.* != .enum_type) {
                return typing_.throw_wrong_from("enum", "enum value", expanded_expr_type, ast.token().span, &self.ctx.errors);
            }
            return prelude_.string_type;
        },
        .addr_cast => {
            const child_type = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;

            if (expected == null) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot infer type to cast to, try using `as`" } });
                return error.CompileError;
            }

            const from_type = child_type.expand_identifier();
            const to_type = expected.?.expand_identifier();

            if (to_type.* != .addr_of) {
                return typing_.throw_wrong_from("address", "addr_cast", to_type, ast.expr().token().span, &self.ctx.errors);
            }
            if (from_type.* != .addr_of) {
                return typing_.throw_wrong_from("address", "addr_cast", from_type, ast.token().span, &self.ctx.errors);
            }

            if (to_type.addr_of.mut and !from_type.addr_of.mut) {
                // Casting away mutability directly is never allowed
                // Cast to Word64 first like a normal person
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot cast away immutability" } });
                return error.CompileError;
            }

            return expected.?;
        },
        .word64_from_addr => {
            const child_type = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;

            const from_type = child_type.expand_identifier();

            if (from_type.* != .addr_of) {
                return typing_.throw_wrong_from("address type", "word64_from_addr", from_type, ast.token().span, &self.ctx.errors);
            }
            return prelude_.word64_type;
        },
        .addr_from_word64 => {
            _ = try self.typecheck_AST(ast.expr(), prelude_.word64_type, subst);

            if (expected == null) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot infer type to cast to, try using `as`" } });
                return error.CompileError;
            }

            const to_type = expected.?.expand_identifier();

            if (to_type.* != .addr_of) {
                return typing_.throw_wrong_from("address type", "addr_from_word64", to_type, ast.expr().token().span, &self.ctx.errors);
            }

            return expected.?;
        },
        .primitive_cast => {
            const child_type = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;

            if (expected == null) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot infer type to cast to, try using `as`" } });
                return error.CompileError;
            }

            const from_expanded = child_type.expand_identifier();
            const to_expanded = expected.?.expand_identifier();

            const from_info = prelude_.info_from_ast(from_expanded);
            const to_info = prelude_.info_from_ast(to_expanded);

            if (from_info == null or to_info == null) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "cannot infer type to cast to, try using `as`" } });
            }

            return expected.?;
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
                ast.enum_value.init = ast.enum_value.domain.?.annotation.init orelse prelude_.unit_value;
            }
            _ = self.typecheck_AST(ast.enum_value.init.?, ast.enum_value.domain.?.child(), subst) catch return error.CompileError;
            if (ast.enum_value.base.?.* == .annotation) {
                return ast.enum_value.base.?.child();
            }
            return ast.enum_value.base.?;
        },
        .@"if" => {
            // FIXME: High cyclo
            if (ast.@"if".let) |let| {
                _ = self.typecheck_AST(let, null, subst) catch return error.CompileError;
            }

            _ = self.typecheck_AST(ast.@"if".condition, prelude_.bool_type, subst) catch return error.CompileError;

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

            const body_type = self.typecheck_AST(ast.body_block(), expected_type, subst) catch return error.CompileError;
            var else_type: ?*Type_AST = null;
            if (ast.else_block() != null) {
                else_type = self.typecheck_AST(ast.else_block().?, expected_type, subst) catch return error.CompileError;
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
                _ = self.typecheck_AST(ast.match.let.?, null, subst) catch return error.CompileError;
            }
            const expr_type = self.typecheck_AST(ast.expr(), null, subst) catch return error.CompileError;

            const expanded_expr_type = expr_type.expand_identifier();

            var retval: ?*Type_AST = null;
            for (0..ast.children().items.len) |i| {
                const child_type = self.typecheck_AST(ast.children().items[i], expected, subst) catch return error.CompileError;
                if (!child_type.is_ident_type("Void")) {
                    retval = child_type;
                }
                try self.ctx.validate_pattern.assert_pattern_matches(ast.children().items[i].lhs(), expanded_expr_type, subst);
            }

            try self.ctx.validate_pattern.exhaustive_check(expanded_expr_type, ast.children(), ast.token().span);
            return retval orelse prelude_.void_type;
        },
        .mapping => return self.typecheck_AST(ast.rhs(), expected, subst),
        .@"while" => {
            if (ast.@"while".let) |let| {
                _ = self.typecheck_AST(let, null, subst) catch return error.CompileError;
            }
            if (ast.@"while".post) |post| {
                _ = self.typecheck_AST(post, null, subst) catch return error.CompileError;
            }
            _ = self.typecheck_AST(ast.@"while".condition, prelude_.bool_type, subst) catch return error.CompileError;

            _ = self.typecheck_AST(ast.body_block(), null, subst) catch return error.CompileError;
            if (ast.else_block() != null) {
                _ = self.typecheck_AST(ast.else_block().?, null, subst) catch return error.CompileError;
            }

            return prelude_.unit_type;
        },
        .@"for" => {
            if (ast.@"for".let) |let| {
                _ = self.typecheck_AST(let, null, subst) catch return error.CompileError;
            }
            const into_iter_type = self.typecheck_AST(ast.@"for".into_iter, null, subst) catch return error.CompileError;
            const impl = try Scope.impl_trait_lookup(into_iter_type, core_.into_iterator_trait, self.ctx);
            if (impl.impl_ast == null) {
                self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                    .span = ast.token().span,
                    .trait_name = core_.into_iterator_trait.name,
                    ._type = into_iter_type,
                } });
                return error.CompileError;
            }
            // Can assume these will be defined by check above
            const instantiated_impl = try ast.scope().?.instantiate_generic_impl(impl.impl_ast.?, &impl.subst.?, self.ctx);
            ast.@"for".into_iter_into_iter_method_decl = Scope.search_impl(instantiated_impl, "into_iter").?;
            const iterator_type = ast.@"for".into_iter_into_iter_method_decl.?.method_decl.ret_type;

            const iterator_impl = try Scope.impl_trait_lookup(iterator_type, core_.iterator_trait, self.ctx);
            if (iterator_impl.impl_ast == null) {
                self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                    .span = ast.token().span,
                    .trait_name = core_.iterator_trait.name,
                    ._type = prelude_.string_type,
                } });
                return error.CompileError;
            }
            const iterator_instantiated_impl = try ast.scope().?.instantiate_generic_impl(iterator_impl.impl_ast.?, &iterator_impl.subst.?, self.ctx);
            ast.@"for".into_iter_next_method_decl = Scope.search_impl(iterator_instantiated_impl, "next").?;
            const item_type = ast.@"for".into_iter_next_method_decl.?.method_decl.ret_type.get_some_type();
            try self.ctx.validate_pattern.assert_pattern_matches(ast.@"for".elem.binding.pattern, item_type, subst);
            _ = self.typecheck_AST(ast.body_block(), null, subst) catch return error.CompileError;
            if (ast.else_block() != null) {
                _ = self.typecheck_AST(ast.else_block().?, null, subst) catch return error.CompileError;
            }

            return prelude_.unit_type;
        },
        .with => {
            for (ast.with.abilities.items) |child| {
                _ = try self.typecheck_AST(child, null, subst);
            }
            return self.typecheck_AST(ast.with._body_block, expected, subst);
        },
        .block => {
            var last_type: ?*Type_AST = null;
            for (0..ast.children().items.len) |i| {
                const expected_type: ?*Type_AST = if (ast.block.final == null and i == ast.children().items.len - 1) expected else null;
                const statement_type = self.typecheck_AST(ast.children().items[i], expected_type, subst) catch return error.CompileError;
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
                _ = self.typecheck_AST(final, expected, subst) catch return error.CompileError;
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
                _ = self.typecheck_AST(expr, inner_fn_ret_type, subst) catch return error.CompileError;
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
            _ = self.typecheck_AST(ast.statement(), null, subst) catch return error.CompileError;
            return prelude_.unit_type;
        },
        .fn_decl => {
            try self.ctx.validate_symbol.validate_symbol(ast.symbol().?);
            return ast.decl_type();
        },
        .method_decl => {
            if (ast.symbol() != null) {
                // Not a trait-method
                try self.ctx.validate_symbol.validate_symbol(ast.symbol().?);
            }
            return ast.decl_type();
        },
        .decl => {
            try self.ctx.validate_type.validate_type(ast.decl.type);
            if (ast.decl.init) |_init| {
                _ = self.typecheck_AST(_init, ast.decl.type, subst) catch return error.CompileError;
            }
            try self.ctx.validate_symbol.validate_symbol(ast.decl.name.symbol().?);
            return prelude_.unit_type;
        },
        .ability_value_decl => {
            try self.ctx.validate_type.validate_type(ast.ability_value_decl.parent);
            const stripped = ast.ability_value_decl.parent.strip_addrs();
            _ = try stripped.resolve_ability_reference(self.ctx) orelse {
                self.ctx.errors.add_error(errs_.Error{ .expected_ability = .{
                    .span = ast.token().span,
                    .got = stripped,
                } });
                return error.CompileError;
            };
            if (ast.ability_value_decl.init) |_init| {
                _ = self.typecheck_AST(_init, ast.ability_value_decl.parent, subst) catch return error.CompileError;
            }
            try self.ctx.validate_symbol.validate_symbol(ast.symbol().?);
            return prelude_.unit_type;
        },
        .binding => {
            try self.ctx.validate_type.validate_type(ast.binding.type);
            if (ast.binding.init) |_init| {
                _ = self.typecheck_AST(_init, ast.binding.type, subst) catch return error.CompileError;
            }
            for (ast.binding.decls.items) |decl| {
                try self.ctx.validate_symbol.validate_symbol(decl.decl.name.symbol().?);
            }
            try self.ctx.validate_pattern.assert_pattern_matches(ast.binding.pattern, ast.binding.type, subst);
            return prelude_.unit_type;
        },
        else => std.debug.panic("compiler error: typecheck_AST() unimplemented for {s}", .{@tagName(ast.*)}),
    }
}

fn insert_coercion(self: *Self, ast: *ast_.AST, actual: *Type_AST, expected: *Type_AST, subst: *unification_.Substitutions) Validate_Error_Enum!*Type_AST {
    const actual_expanded = actual.expand_identifier();
    const expected_expanded = expected.expand_identifier();

    // TODO: Maybe &[n]T => []T. Both T => E!T, T => ?T have ambiguity problems, but might be more ergonomic?

    if
    // Expected a dyn type of some super trait, got a dyn type of some sub trait. Wrap it in a new dyn_value
    (actual_expanded.* == .dyn_type and expected_expanded.* == .dyn_type and
        actual_expanded.child().symbol() != expected_expanded.child().symbol() and
        actual_expanded.child().is_sub_trait(expected_expanded.child()) and
        !(expected_expanded.dyn_type.mut and !actual_expanded.dyn_type.mut))
    {

        // Preserve the already-typechecked expression as the dyn value's operand
        const inner = self.ctx.allocator().create(ast_.AST) catch unreachable;
        inner.* = ast.*;
        inner.common().validation_state = .valid;
        self.map.put(inner, actual) catch unreachable;

        ast.* = ast_.AST.create_dyn_value(ast.token(), expected_expanded, inner, ast.scope().?, expected_expanded.dyn_type.mut, self.ctx.allocator()).*;
        return self.typecheck_AST(ast, expected, subst);
    }

    return actual;
}

/// Validates an open binary operator. An operator is `open` if it returns a type different from the
/// ones it acts on.
///
/// Returns the type of the validated operator.
pub fn binary_operator_open(
    self: *Self,
    ast: *ast_.AST,
    expected: ?*Type_AST,
    subst: *unification_.Substitutions,
) Validate_Error_Enum!*Type_AST {
    const lhs_type = self.typecheck_AST(ast.lhs(), expected, subst) catch return error.CompileError;
    if (ast.lhs().* == .identifier and ast.lhs().symbol().?.is_type()) {
        return error.CompileError;
    }
    _ = self.typecheck_AST(ast.rhs(), lhs_type, subst) catch return error.CompileError;
    return lhs_type;
}

const Self_Find_Result = union(enum) {
    return_value,
    param: usize,
};
fn find_self_type(trait_decl: *ast_.AST, method_name: []const u8) !?Self_Find_Result {
    const method_decl: *ast_.AST = trait_decl.trait.find_method(method_name).?; // TODO: Throw error

    // A receiver (`self`, `&self`, `&mut self`) is a Self-typed first argument
    if (method_decl.method_decl.receiver != null) {
        return .{ .param = 0 };
    }

    for (method_decl.children().items, 0..) |param_binding, i| {
        const symbol = param_binding.binding.decls.items[0].decl.name.symbol().?;
        if (symbol.type().refers_to_self()) {
            return .{ .param = i };
        }
    }

    if (method_decl.method_decl.ret_type.refers_to_self()) {
        return .return_value;
    }

    return null;
}

const Method_Selection = struct {
    subst: *unification_.Substitutions,
    method_type: *Type_AST,
};
/// Finds the appropriate method decl for an invoke, if it exists and is unambiguous. Updates `ast.invoke.method_decl` to point to the correct method decl.
fn select_method(
    self: *Self,
    ast: *ast_.AST,
    expected: ?*Type_AST,
    candidate_method_decls: *const std.array_hash_map.AutoArrayHashMap(*ast_.AST, void),
    true_lhs_type: *Type_AST,
    subst: *unification_.Substitutions,
) !Method_Selection {
    const Viable_Method = struct { method: *ast_.AST, plan: Receiver_Plan };
    var viable = std.array_list.Managed(Viable_Method).init(self.ctx.allocator());
    defer viable.deinit();

    var rejection_reasons = std.array_list.Managed(errs_.Candidate).init(self.ctx.allocator());

    for (candidate_method_decls.keys()) |method_decl| {
        switch (try self.method_fits(ast, expected, method_decl, true_lhs_type, subst)) {
            .fits => |plan| {
                try viable.append(Viable_Method{ .method = plan.method_decl, .plan = plan });
            },
            else => |e| try rejection_reasons.append(errs_.Candidate{ .span = method_decl.token().span, .reason = e.not_viable }),
        }
    }

    var chosen: ?Viable_Method = null;
    switch (viable.items.len) {
        0 => {
            // Type check for any genuine compile errors in the args that might've made it seem like there were no viable methods
            for (ast.children().items) |arg| {
                _ = self.typecheck_AST(arg, null, subst) catch return error.CompileError;
            }

            // Good args + no viable methods = BAD (genuinely no viable methods)
            const saved_rejection_reasons = try rejection_reasons.clone();
            self.ctx.errors.add_error(errs_.Error{
                .type_not_impl_method = .{
                    .span = ast.token().span,
                    .method_name = ast.rhs().token().data,
                    ._type = true_lhs_type.strip_as_trait().strip_addrs(),
                    .candidates = saved_rejection_reasons.items,
                },
            });
            return error.CompileError;
        },
        1 => {
            // One viable method = GOOD :)
            chosen = viable.items[0];
        },
        else => {
            // Prefer candidates whose return type matches the expected exactly, fallback to coercions
            if (expected) |exp| {
                var exact_count: usize = 0;
                for (viable.items) |candidate| {
                    const ret = candidate.method.decl_type().function._rhs;
                    if (ret.types_match(exp) and exp.types_match(ret)) {
                        chosen = candidate;
                        exact_count += 1;
                    }
                }
                if (exact_count != 1) chosen = null;
            }

            if (chosen == null) {
                // >1 viable methods = BAD! (ambiguous)
                var saved_viable = std.array_list.Managed(*ast_.AST).init(self.ctx.allocator());
                for (viable.items) |candidate| {
                    try saved_viable.append(candidate.method);
                }
                self.ctx.errors.add_error(errs_.Error{
                    .ambiguous_methods = .{
                        .span = ast.token().span,
                        .method_name = ast.rhs().token().data,
                        ._type = true_lhs_type.strip_as_trait().strip_addrs(),
                        .viable = saved_viable.items,
                    },
                });
                return error.CompileError;
            }
        },
    }

    rejection_reasons.deinit();
    const choice = chosen.?;
    var sel_subst: *unification_.Substitutions = subst;
    var method_type = choice.plan.method_type;
    if (choice.plan.impl_subst) |isub| {
        // Candidate came from a generic impl, instantiate a clone of it with the solved params
        const instantiated_impl = try ast.scope().?.instantiate_generic_impl(choice.method.method_decl.impl.?, isub, self.ctx);
        const new_method_decl = Scope.search_impl(instantiated_impl, ast.rhs().token().data).?;
        // Inject the caller's bindings with the impl solution
        var t_it = subst.type_subst.iterator();
        while (t_it.next()) |entry| {
            if (isub.get_type(entry.key_ptr.*) == null) isub.put_type(entry.key_ptr.*, entry.value_ptr.*) catch unreachable;
        }
        var c_it = subst.const_subst.iterator();
        while (c_it.next()) |entry| {
            if (isub.get_const(entry.key_ptr.*) == null) isub.put_const(entry.key_ptr.*, entry.value_ptr.*) catch unreachable;
        }
        sel_subst = isub;
        // The plan's method_type predates instantiation and the injected bindings, re-derive through them
        if (new_method_decl != choice.method) {
            // an instantiation happened and we gotta frickin deal with it
            method_type = new_method_decl.decl_type().clone(sel_subst, self.ctx.allocator());
            ast.invoke.method_decl = new_method_decl;
        } else {
            // no real instantiation happened, keep using the plans stuff because its most current
            method_type = choice.plan.method_type;
            ast.invoke.method_decl = choice.plan.method_decl;
        }
    } else {
        ast.invoke.method_decl = choice.method;
    }
    if (choice.plan.prepend) {
        try ast.children().insert(0, choice.plan.receiver_expr.?);
        ast.invoke.prepended = true;
    }
    return .{
        .subst = sel_subst,
        .method_type = method_type,
    };
}

const Receiver_Plan = struct {
    prepend: bool,
    receiver_expr: ?*ast_.AST,
    impl_subst: ?*unification_.Substitutions = null,
    method_decl: *ast_.AST,
    method_type: *Type_AST,
};
const Method_Result = union(enum) {
    fits: Receiver_Plan,
    not_viable: errs_.Candidate_Reason,
};
fn method_fits(
    self: *Self,
    ast: *ast_.AST,
    expected: ?*Type_AST,
    method_decl: *ast_.AST,
    true_lhs_type: *Type_AST,
    subst: *unification_.Substitutions,
) !Method_Result {
    var new_self = try self.clone();
    defer new_self.deinit();

    const enclosing_impl = method_decl.method_decl.impl;
    const is_impl_template = enclosing_impl != null and enclosing_impl.?.impl._generic_params.items.len > 0;
    var impl_subst: ?*unification_.Substitutions = null;
    var method_type = method_decl.decl_type();
    var instantiated_method_decl = method_decl;

    // Try to unify the method's impl's for type with the true lhs type, if generic
    if (is_impl_template) {
        var stripped_lhs = true_lhs_type;
        while (stripped_lhs.* == .as_trait) stripped_lhs = stripped_lhs.lhs();
        if (stripped_lhs.* == .addr_of and !stripped_lhs.addr_of.multiptr) stripped_lhs = stripped_lhs.child();

        const scratch = self.ctx.allocator().create(unification_.Substitutions) catch unreachable;
        scratch.* = unification_.Substitutions.init(self.ctx.allocator());
        scratch.put_type("Self", stripped_lhs) catch unreachable;
        unification_.unify(enclosing_impl.?.impl._type, stripped_lhs, scratch, .{}) catch
            return .{ .not_viable = .receiver_mismatch };
        impl_subst = scratch;
        method_type = method_decl.decl_type().clone(scratch, self.ctx.allocator());
    }

    // Generic method, gotta monomorphize it
    const method_is_generic = method_decl.generic_params().items.len > 0;
    if (method_is_generic) {
        try generic_apply_.instantiate_generic_invoke(method_decl.symbol().?, ast, self.ctx);
        instantiated_method_decl = ast.symbol().?.decl.?;
        method_type = instantiated_method_decl.decl_type();
        if (instantiated_method_decl == method_decl) {
            // Monomorphization deferred (abstract generic args), check against the renamed signature
            var scratch = unification_.Substitutions.init(self.ctx.allocator());
            defer scratch.deinit();
            unification_.generate_substitutions_from_args(instantiated_method_decl.generic_params().items, ast.invoke.generic_args.items, &scratch);
            method_type = instantiated_method_decl.decl_type().clone(&scratch, self.ctx.allocator());
        }
    }

    const domain: std.array_list.Managed(*Type_AST) = method_type.function.args;

    var temp_args = std.array_list.Managed(*ast_.AST).init(new_self.ctx.allocator());
    defer temp_args.deinit();

    const maybe_receiver: ?*ast_.AST = new_self.extract_receiver(ast, instantiated_method_decl, true_lhs_type) catch return .{ .not_viable = .receiver_mismatch };
    if (maybe_receiver) |receiver| {
        try temp_args.append(receiver);
    }
    try temp_args.appendSlice(ast.children().items);

    var args_validator = args_.init(
        .method,
        &temp_args,
        ast.token().span,
        &domain,
        &new_self.ctx.errors,
        new_self.ctx.allocator(),
    );

    temp_args = try args_validator.fill_default_args();

    // Disable error recording so typecheck_AST errors inside probe_type are not stored
    self.ctx.errors.record_errors = false;
    defer self.ctx.errors.record_errors = true;
    args_validator.validate_arity() catch return .{ .not_viable = .{ .arity_mismatch = .{ .expects = domain.items.len, .got = temp_args.items.len } } };
    if (args_validator.probe_type(subst, self.ctx)) |failure| {
        const expected_type = domain.items[failure.param_idx];
        return .{ .not_viable = .{ .param_arg_mismatch = .{ .param_idx = failure.param_idx, .expects = expected_type, .got = failure.arg_type } } };
    }

    // Try unifying the return type with the expected return type, if its provided
    if (expected) |exp| {
        const actual = method_type.function._rhs;
        try walk_.walk_type(actual, Decorate.new(self.ctx)); // Type might not be decorated yet!
        if (impl_subst) |is| {
            // Impl was generic, try to unify return type with the substitutions
            unification_.unify(actual, exp, is, .{}) catch {
                return .{ .not_viable = .{ .ret_type_mismatch = .{ .expected = exp, .got = actual } } };
            };
        } else {
            // Impl wasn't generic, use the surrounding context's subst
            unification_.unify(actual, exp, subst, .{ .allow_rigid = false }) catch {
                return .{ .not_viable = .{ .ret_type_mismatch = .{ .expected = exp, .got = actual } } };
            };
        }
    }

    // A template is only viable if every impl param got solved
    if (impl_subst) |is| {
        for (enclosing_impl.?.impl._generic_params.items) |param| {
            if ((param.is_typelike_param_decl()) and is.get_type(param.symbol().?.name) == null) {
                return .{ .not_viable = .{ .ret_type_mismatch = .{ .expected = expected orelse method_type.function._rhs, .got = method_type.function._rhs } } };
            }
        }

        // Every solved param must satisfy its constraints, subst'd through the solution
        for (enclosing_impl.?.impl._generic_params.items) |param| {
            if (param.* != .type_param_decl) continue;
            const solved = is.get_type(param.symbol().?.name).?;
            for (param.type_param_decl.constraints.items) |constraint| {
                const substd_constraint = constraint.clone(is, self.ctx.allocator());
                const sat_res = solved.satisfies_all_constraints(&[_]*Type_AST{substd_constraint}, null, self.ctx) catch
                    return .{ .not_viable = .{ .constraint_not_satisfied = .{ .solved = solved, .constraint = substd_constraint } } };
                if (sat_res != .satisfies) {
                    return .{ .not_viable = .{ .constraint_not_satisfied = .{ .solved = solved, .constraint = substd_constraint } } };
                }
            }
        }
    }

    return .{ .fits = .{
        .prepend = maybe_receiver != null,
        .receiver_expr = maybe_receiver,
        .impl_subst = impl_subst,
        .method_decl = instantiated_method_decl,
        .method_type = method_type,
    } };
}

fn extract_receiver(self: *Self, ast: *ast_.AST, method_decl: *ast_.AST, true_lhs_type: *Type_AST) !?*ast_.AST {
    const expanded_true_lhs_type = true_lhs_type.expand_identifier();

    if (method_decl.method_decl.receiver != null and !ast.invoke.prepended) {
        const receiver_kind: ?ast_.Receiver_Kind = method_decl.method_decl.receiver.?.receiver.kind;
        // Trait method takes a receiver...
        // Prepend invoke lhs to args if there is a receiver
        const lhs_is_addr = expanded_true_lhs_type.* == .dyn_type or expanded_true_lhs_type.* == .addr_of;
        if (lhs_is_addr) {
            // lhs type is dynamic or an address...
            if (!expanded_true_lhs_type.mut() and receiver_kind == .mut_addr_of) {
                // Receiver is immutable when it should be mutable
                self.ctx.errors.add_error(errs_.Error{ .invoke_receiver_mismatch = .{
                    .lhs_type = true_lhs_type,
                    .method_name = method_decl.method_decl.name.token().data,
                    .method_receiver = receiver_kind.?,
                    .receiver_span = ast.lhs().token().span,
                } });
                return error.CompileError;
            }
        }

        switch (method_decl.method_decl.receiver.?.receiver.kind) {
            .value => {
                // receiver is value, create any dereferences necessary
                var stripped_expanded_true_lhs_type = expanded_true_lhs_type;
                var retval = ast.lhs();
                while (stripped_expanded_true_lhs_type.* == .addr_of and !stripped_expanded_true_lhs_type.addr_of.multiptr) {
                    retval = ast_.AST.create_dereference(ast.lhs().token(), retval, self.ctx.allocator());
                    stripped_expanded_true_lhs_type = stripped_expanded_true_lhs_type.child();
                }
                return retval;
            },
            else => if (lhs_is_addr) {
                return ast.lhs();
            } else {
                const addr_of = ast_.AST.create_addr_of(ast.lhs().token(), ast.lhs(), receiver_kind.? == .mut_addr_of, false, self.ctx.allocator());
                return addr_of;
            },
        }
    }
    return null;
}

fn validate_ability(self: *Self, function_type: *Type_AST, ast: *ast_.AST) Validate_Error_Enum!void {
    for (function_type.function.abilities.items) |ability| {
        var fn_ab = ability;
        const symbol = try ast.scope().?.ability_lookup(fn_ab, self.ctx) orelse {
            if (fn_ab.* == .addr_of) {
                fn_ab = fn_ab.child();
            }
            self.ctx.errors.add_error(errs_.Error{ .missing_ability = .{
                .span = ast.token().span,
                .ability = fn_ab,
            } });
            return error.CompileError;
        };
        try ast.ability_args().append(symbol);
    }
}

/// Validates that `ast` is a valid lvalue
fn validate_L_Value(self: *Self, ast: *ast_.AST) Validate_Error_Enum!void {
    switch (ast.*) {
        // These are all good
        .select, .positional_select, .identifier, .access, .array_value, .ability_value, .string => {},

        // A dereference is an lvalue IF its a deref of an lval, thats not an address
        .dereference => if (ast.expr().* != .addr_of and ast.expr().* != .call) {
            try self.validate_L_Value(ast.expr());
        },

        // A tuple is a valid lvalue if its made up of lvalues
        .tuple_value => for (ast.children().items) |term| {
            try self.validate_L_Value(term);
        },

        else => {
            self.ctx.errors.add_error(errs_.Error{ .basic = .{ .span = ast.token().span, .msg = "not an l-value" } });
            return error.CompileError;
        },
    }
}

/// Rewrites `args[0]` so its type is the address `&base`/`&mut base` an address
/// receiver expects: dereferences through any non-multiptr address layers to reach
/// the base value, then takes its address it with the requested mutability.
fn normalize_addr_receiver(
    self: *Self,
    args: *std.array_list.Managed(*ast_.AST),
    subst: *unification_.Substitutions,
) Validate_Error_Enum!void {
    var arg = args.items[0];
    // Peel an addrof chain until we get `&self`
    while (true) {
        const at = (self.typecheck_AST(arg, null, subst) catch return error.CompileError).expand_identifier();
        if (at.* != .addr_of or at.addr_of.multiptr) break;
        const inner = at.child().expand_identifier();
        if (inner.* != .addr_of or inner.addr_of.multiptr) break;
        // Unwrap a literal `&x` to `x` so codegen doesn't emit `&` of a non-lvalue cast
        arg = if (arg.* == .addr_of) arg.expr() else ast_.AST.create_dereference(arg.token(), arg, self.ctx.allocator());
    }
    args.items[0] = arg;
}

fn implicit_dereference(
    self: *Self,
    ast: *ast_.AST,
    old_lhs_type: *Type_AST,
    subst: *unification_.Substitutions,
) Validate_Error_Enum!*Type_AST {
    var lhs_type = old_lhs_type;
    if (lhs_type.* == .addr_of and !lhs_type.addr_of.multiptr) {
        ast.set_lhs(ast_.AST.create_dereference(ast.token(), ast.lhs(), self.ctx.allocator()));
        lhs_type = (self.typecheck_AST(ast.lhs(), null, subst) catch return error.CompileError).expand_identifier();
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
