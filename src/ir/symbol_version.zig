const std = @import("std");
const Symbol = @import("../symbol/symbol.zig");
const Instruction = @import("../ir/instruction.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const fmt_ = @import("../util/fmt.zig");

const Self = @This();

symbol: *Symbol,
def: ?*Instruction,

uses: usize = 0,

allocator: std.mem.Allocator,

pub fn create_unversioned(symbol: *Symbol, allocator: std.mem.Allocator) *Self {
    var retval = allocator.create(Self) catch unreachable;
    retval.symbol = symbol;
    retval.uses = 0;
    retval.def = null;
    retval.allocator = allocator;
    return retval;
}

pub fn deinit(self: *Self) void {
    _ = self;
    // self.allocator.destroy(self); // TODO: Bwuh?!
}

pub fn format(self: ?*Self, out: *std.io.Writer) !void {
    if (self) |symbver| {
        try out.print("{s:<10}", .{symbver.symbol.name});
    } else {
        try out.print("<null>    ", .{});
    }
}

pub fn set_def(self: *Self, def: ?*Instruction) void {
    self.def = def;
    if (def) |_| {
        self.symbol.defs += 1;
    }
}

/// Finds a Symbol Version in a Symbol Version set, or null if not found.
///
/// Two Symbol Versions are considered equivalent if they refer to the same Symbol.
pub fn find_symbol_version_set(
    self: *Self,
    set: *std.array_list.Managed(*Self),
) ?*Self {
    // Go through the set's symbvers
    for (set.items) |symbver| {
        if (symbver.symbol == self.symbol) {
            // Set element symbver has the same symbol as the symbver we're looking for
            // Return it
            return symbver;
        }
    }
    return null;
}

pub fn put_symbol_version_set(self: *Self, set: *std.array_list.Managed(*Self)) bool {
    for (set.items) |v| {
        if (v.symbol == self.symbol) {
            return false;
        }
    }
    set.append(self) catch unreachable;
    return true;
}
