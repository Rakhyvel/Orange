//! This file contains the semantic validation logic for symbols.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const Const_Eval = @import("const_eval.zig");
const typecheck_AST = @import("typecheck.zig").typecheck_AST;
const errs_ = @import("../util/errors.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Symbol = @import("../symbol/symbol.zig");
const Token = @import("../lexer/token.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const walk_ = @import("../ast/walker.zig");

const Validate_Error_Enum = error{ OutOfMemory, CompileError };

const Self: type = @This();

ctx: *Compiler_Context,

pub fn init(ctx: *Compiler_Context) Self {
    return Self{ .ctx = ctx };
}

pub fn validate_symbol(self: *Self, symbol: *Symbol) Validate_Error_Enum!void {
    // TODO: Bit long
    if (symbol.validation_state == .valid or symbol.validation_state == .validating) {
        return;
    }
    symbol.validation_state = .validating;
    symbol.init_validation_state = .validating;

    _ = symbol.assert_symbol_valid();
    const expected: ?*Type_AST = switch (symbol.kind) {
        .@"fn", .@"test" => symbol.type().rhs(),
        .type, .context => null,
        .import_inner => if (symbol.decl.?.* == .type_alias) null else symbol.type(),
        else => symbol.type(),
    };

    // std.debug.print("validating type for: {s} ({t}): {?f}\n", .{ symbol.name, symbol.kind, expected });

    if (expected) |expected_type| {
        try self.ctx.validate_type.validate_type(expected_type);
        if (self.ctx.validate_type.detect_cycle(expected_type, null)) {
            self.ctx.errors.add_error(errs_.Error{ .symbol_error = .{
                .problem = "cyclic type detected",
                .span = symbol.span(),
                .name = symbol.name,
            } });
            return error.CompileError;
        }
    }

    if (symbol.init_value()) |_init| {
        // might be null for parameters
        // Tree_Writer.print_tree(_init);
        _ = self.ctx.typecheck.typecheck_AST(_init, expected) catch |e| switch (e) {
            error.CompileError => return error.CompileError,
            error.OutOfMemory => return error.OutOfMemory,
            error.UnexpectedTypeType => {
                self.ctx.errors.add_error(errs_.Error{ .unexpected_type_type = .{ .expected = expected, .span = _init.token().span } });
                return error.CompileError;
            },
        };
        if (_init.* != .module) {
            try walk_.walk_ast(_init, Const_Eval.new(self.ctx));
        }
    } else if (symbol.kind == .type and symbol.init_typedef() != null) {
        try self.ctx.validate_type.validate_type(symbol.init_typedef().?);
        if (self.ctx.validate_type.detect_cycle(symbol.init_typedef().?, symbol)) {
            self.ctx.errors.add_error(errs_.Error{ .basic = .{
                .msg = "cyclic type detected",
                .span = symbol.span(),
            } });
            return error.CompileError;
        }
    }

    if (symbol.kind == .trait) {
        try self.validate_trait(symbol);
    } else if (symbol.init_value() != null and symbol.init_value().?.* == .poison) {
        symbol.validation_state = .invalid;
        symbol.init_validation_state = .invalid;
        return error.CompileError;
        // unreachable;
    }

    // Check that tests are requesting good contexts
    if (symbol.kind == .@"test") {
        const args_ = @import("args.zig");
        try args_.validate_requested_contexts(symbol.type().function.contexts.items, &self.ctx.errors);
    }

    if (symbol.decl.?.* == .type_param_decl) {
        if (symbol.decl.?.type_param_decl.constraint) |constraint| {
            if (!constraint.refers_to_trait()) {
                self.ctx.errors.add_error(errs_.Error{ .not_constraint = .{ .got = constraint, .span = constraint.token().span } });
                return error.CompileError;
            }
        }
    }

    // Symbol's name must be capitalized iff its type is Type
    if (symbol.is_type() or symbol.kind == .trait or symbol.kind == .context) {
        if (!is_capitalized(symbol.name)) {
            self.ctx.errors.add_error(errs_.Error{ .symbol_error = .{
                .problem = "must start with an uppercase letter",
                .span = symbol.span(),
                .name = symbol.name,
            } });
            return error.CompileError;
        }
    } else {
        if (is_capitalized(symbol.name)) {
            self.ctx.errors.add_error(errs_.Error{ .symbol_error = .{
                .problem = "must start with an lowercase letter",
                .span = symbol.span(),
                .name = symbol.name,
            } });
            return error.CompileError;
        }
    }

    if (symbol.storage == .@"extern") {
        if (symbol.storage.@"extern".c_name != null) {
            _ = self.ctx.typecheck.typecheck_AST(symbol.storage.@"extern".c_name.?, prelude_.string_type) catch return error.CompileError;
        } else {
            symbol.storage.@"extern".c_name = ast_.AST.create_string(Token.init_simple(symbol.name), symbol.name, self.ctx.allocator());
        }
    }

    _ = symbol.assert_init_valid();
}

fn type_is_type_type(ast: *ast_.AST) bool {
    if (ast.* == .function) {
        return type_is_type_type_atom(ast.rhs());
    } else {
        return type_is_type_type_atom(ast);
    }
}

fn type_is_type_type_atom(ast: *ast_.AST) bool {
    return ast.* == .identifier and std.mem.eql(u8, ast.token().data, "Type");
}

fn validate_trait(self: *Self, trait: *Symbol) Validate_Error_Enum!void {
    var names = std.StringArrayHashMap(*ast_.AST).init(self.ctx.allocator());
    defer names.deinit();

    for (trait.decl.?.trait.method_decls.items) |decl| {
        if (names.get(decl.method_decl.name.token().data)) |other| {
            self.ctx.errors.add_error(errs_.Error{ .duplicate = .{
                .span = decl.token().span,
                .identifier = decl.method_decl.name.token().data,
                .first = other.token().span,
            } });
            return error.CompileError;
        } else {
            names.put(decl.method_decl.name.token().data, decl) catch unreachable;
        }

        for (decl.method_decl._params.items) |param| {
            try self.ctx.validate_type.validate_type(param.binding.type);
        }
        try self.ctx.validate_type.validate_type(decl.method_decl.ret_type);
        try self.ctx.validate_type.validate_type(decl.method_decl.c_type.?);

        if (decl.method_decl.is_virtual) {
            if (decl.method_decl.c_type.?.refers_to_self()) {
                self.ctx.errors.add_error(errs_.Error{ .trait_virtual_refers_to_self = .{
                    .span = decl.token().span,
                    .method_name = decl.method_decl.name.token().data,
                    .trait_name = trait.name,
                } });
                return error.CompileError;
            }
            trait.decl.?.trait.num_virtual_methods += 1;
        }
    }
}

fn is_capitalized(name: []const u8) bool {
    var should_be_upper = true;
    for (name) |c| {
        if (should_be_upper and std.ascii.isLower(c)) {
            return false;
        }
        should_be_upper = c == '_';
    }
    return true;
}
