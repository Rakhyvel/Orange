//! This file contains the semantic validation logic for modules.

const std = @import("std");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const module_ = @import("../hierarchy/module.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Span = @import("../util/span.zig");
const unification_ = @import("../types/unification.zig");

// TODO: Are lexer, parse errors even possible?
const Validate_Error_Enum = error{ LexerError, ParseError, CompileError, OutOfMemory };

const Self: type = @This();

ctx: *Compiler_Context,

pub fn init(ctx: *Compiler_Context) Self {
    return Self{ .ctx = ctx };
}

pub fn validate_module(self: *Self, module: *module_.Module) Validate_Error_Enum!void {
    try self.ctx.validate_scope.validate_scope(self.ctx.module_scope(module.absolute_path).?);
    for (0..module.cincludes.items.len) |i| {
        var subst = unification_.Sym_Substitutions.init(self.ctx.allocator());
        defer subst.deinit();
        _ = self.ctx.typecheck.typecheck_AST(module.cincludes.items[i], prelude_.string_type, &subst) catch return error.CompileError;
    }
    if (self.ctx.errors.errors_list.items.len > 0) {
        return error.CompileError;
    }

    if (!module_name_is_good(module.name())) {
        self.ctx.errors.add_error(errs_.Error{ .symbol_error = .{
            .problem = "has an improper module name",
            .span = Span{ .filename = module.absolute_path, .col = 0, .line_number = 0, .line_text = "" },
            .name = module.name(),
        } });
        return error.CompileError;
    }
}

fn module_name_is_good(module_name: []const u8) bool {
    if (module_name.len == 0) {
        return false;
    }
    if (!std.ascii.isLower(module_name[0])) {
        return false;
    }
    for (module_name) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') {
            return false;
        }
    }
    return true;
}
