//! This file represents a struct for an AST walk, and is used to decorate identifier ASTs with symbols they refer to.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const Interpreter_Context = @import("../interpretation/interpreter.zig");
const defaults_ = @import("defaults.zig");
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Symbol_Tree = @import("../ast/symbol-tree.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const walk_ = @import("../ast/walker.zig");
const unification_ = @import("../types/unification.zig");

const Self: type = @This();

ctx: *Compiler_Context,
map: std.AutoArrayHashMap(*ast_.AST, void),
expected_type: ?*Type_AST = null,

pub fn new(ctx: *Compiler_Context) Self {
    return Self{
        .ctx = ctx,
        .map = std.AutoArrayHashMap(*ast_.AST, void).init(ctx.allocator()),
    };
}

pub fn prefix(self: *Self, ast: *ast_.AST) walk_.Error!?Self {
    return self.const_eval_prefix(ast);
}

fn const_eval_prefix(self: *Self, ast: *ast_.AST) walk_.Error!?Self {
    if (self.map.get(ast)) |_| {
        return null;
    }

    const new_self = try self.eval_internal(ast);
    self.map.put(ast, void{}) catch unreachable;

    return new_self;
}

fn eval_internal(self: *Self, ast: *ast_.AST) walk_.Error!Self {
    switch (ast.*) {
        else => {},

        .decl => {
            var new_self = self.*;
            new_self.expected_type = ast.decl.type;
            return new_self;
        },

        .@"comptime" => {
            _ = try self.eval_internal(ast.expr());
            var subst = unification_.Sym_Substitutions.init(self.ctx.allocator());
            defer subst.deinit();
            const expected_type = self.ctx.typecheck.typecheck_AST(ast, self.expected_type, &subst) catch return error.CompileError;
            ast.* = (try self.interpret_comptime_expr(ast.expr(), expected_type, ast.scope().?)).*;
            _ = self.ctx.typecheck.typecheck_AST(ast, expected_type, &subst) catch return error.CompileError;
        },

        .default => {
            const _type = ast.default._type;
            ast.* = (try defaults_.generate_default(ast.default._type, ast.token().span, &self.ctx.errors, self.ctx.allocator())).*;
            var subst = unification_.Sym_Substitutions.init(self.ctx.allocator());
            defer subst.deinit();
            _ = self.ctx.typecheck.typecheck_AST(ast, _type, &subst) catch return error.CompileError;
        },

        .size_of => {
            const _type = ast.size_of._type;
            if (self.ctx.validate_type.detect_cycle(ast.size_of._type, null)) {
                self.ctx.errors.add_error(errs_.Error{ .basic = .{
                    .msg = "cyclic type detected",
                    .span = ast.token().span,
                } });
                return error.CompileError;
            }
            const size = _type.sizeof();
            if (size == null) {
                self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                    .span = ast.token().span,
                    ._type = _type,
                    .trait_name = "Sized",
                } });
                return error.CompileError;
            }
            ast.* = ast_.AST.create_int(ast.token(), size.?, self.ctx.allocator()).*;
            var subst = unification_.Sym_Substitutions.init(self.ctx.allocator());
            defer subst.deinit();
            _ = self.ctx.typecheck.typecheck_AST(ast, null, &subst) catch return error.CompileError;
        },
    }
    return self.*;
}

fn interpret_comptime_expr(
    self: *Self,
    ast: *ast_.AST,
    ret_type: *Type_AST,
    scope: *Scope,
) !*ast_.AST {
    const symbol: *Symbol = (try Symbol_Tree.create_temp_comptime_symbol(
        ast,
        ret_type,
        scope,
        self.ctx.allocator(),
    )).assert_symbol_valid().assert_init_valid();

    // Get the cfg from the symbol, and embed into the module
    const module = symbol.scope.module.?;
    const intered_strings = self.ctx.lookup_interned_string_set(module.uid).?;
    const cfg = try self.ctx.cfg_store.get_cfg(symbol, intered_strings);
    defer cfg.deinit(); // Remove the cfg so that it isn't output

    const idx = cfg.emplace_cfg(module.uid, &module.cfgs, &module.instructions);
    defer module.pop_cfg(idx); // Remove the cfg so that it isn't output

    // Create a context and interpret
    var context = Interpreter_Context.init(self.ctx);
    context.set_entry_point(cfg, ret_type);
    defer context.deinit();
    context.load_module(module);
    try context.run();

    // Extract the retval
    return try context.extract_ast(0, ret_type, ast.token().span);
}
