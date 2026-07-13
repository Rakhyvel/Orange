// The name of this file is

const std = @import("std");
const Symbol = @import("../symbol/symbol.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const CFG = @import("../ir/cfg.zig");
const errs_ = @import("../util/errors.zig");
const Interned_String_Set = @import("../ir/interned_string_set.zig");
const Lower_Context = @import("../ir/lower.zig");
const cfg_validate_ = @import("../semantic/cfg_validate.zig");
const optimizations_ = @import("../ir/optimizations.zig");

const Self: type = @This();

ctx: *Compiler_Context,
map: std.AutoArrayHashMap(*Symbol, *CFG),

pub fn init(ctx: *Compiler_Context) Self {
    return Self{
        .ctx = ctx,
        .map = std.AutoArrayHashMap(*Symbol, *CFG).init(ctx.allocator()),
    };
}

pub fn get_cfg(
    self: *Self,
    symbol: *Symbol,
    interned_strings: *Interned_String_Set,
) Lower_Context.Lower_Errors!*CFG {
    std.debug.assert(symbol.kind == .@"fn" or symbol.kind == .@"test" or symbol.kind == .@"comptime");
    if (symbol.init_validation_state == .unvalidated) {
        try self.ctx.validate_symbol.validate_symbol(symbol);
    } else if (symbol.init_validation_state == .validating) {
        self.ctx.errors.add_error(errs_.Error{ .recursive_definition = .{
            .span = symbol.span(),
            .symbol_name = symbol.name,
        } });
        return error.CompileError;
    }
    if (symbol.cfg == null) {
        symbol.cfg = CFG.init(symbol, self.ctx.allocator());
        var lower_ability = Lower_Context.init(self.ctx, symbol.cfg.?, interned_strings);
        try lower_ability.lower_AST_into_cfg();
        try cfg_validate_.validate_cfg(symbol.cfg.?, &self.ctx.errors);
        try optimizations_.optimize(symbol.cfg.?, &self.ctx.errors, self.ctx.allocator());
        symbol.cfg.?.collect_generated_symbvers();
    }
    return symbol.cfg.?;
}
