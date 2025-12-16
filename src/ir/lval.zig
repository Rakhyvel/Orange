// TODO: Split up symbol version and lval, put them in the SSA namespace along with basic blocks

const std = @import("std");
const Instruction = @import("../ir/instruction.zig");
const Symbol = @import("../symbol/symbol.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Symbol_Version = @import("symbol_version.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const fmt_ = @import("../util/fmt.zig");

/// Represents different forms of l-values in an l-value tree.
///
/// The root of every L-Value tree is a symbol-version. If an L-Value tree represents a destination address, only the root symbol is modified.
/// Most nodes are unary, however, indices have two children.
pub const L_Value = union(enum) {
    /// Represents an SSA version of a symbol
    ///
    /// lval is the address of the symbol
    symbver: *Symbol_Version,

    /// Represents an address dereference
    ///
    /// lval is `expr`
    dereference: struct {
        /// Address to dereference
        expr: *L_Value,
        /// Dereference's resulting expanded type
        expanded_type: *Type_AST,
        /// Allocator that allocated this lvalue
        allocator: std.mem.Allocator,
    },

    /// Represents a runtime array or slice index
    ///
    /// lval is `lhs + rhs * sizeof()`
    index: struct {
        /// The base of the array/slice.
        lhs: *L_Value,
        /// The element's index, which is multiplied by the element size to get the lval
        rhs: *L_Value,
        /// Debug UB info. Is the length of the array/slice. In debug mode, a panic is raised if rhs is greater-to-or-equal to this length.
        length: ?*L_Value,
        /// Index's resulting expanded type
        expanded_type: *Type_AST,
        /// Allocator that allocated this lvalue
        allocator: std.mem.Allocator,
    },

    /// Represents a product selection based on a comptile-time known field and offset.
    ///
    /// lval is `lhs + offset`
    select: struct {
        /// The base lval that is being projected
        lhs: *L_Value,
        /// The id of the field that is being selected. Used by codegen.
        field: i128,
        /// The offset, in bytes, of the selection from the `lhs` address. Used by interpreter.
        offset: i64,
        /// Selection's resulting expanded type
        expanded_type: *Type_AST,
        /// Debug UB info. Populated by a `get_tag` before selection.
        /// In debug mode, a panic is raised if this doesn't match `field` at runtime.
        tag: ?*L_Value,
        /// Allocator that allocated this lvalue
        allocator: std.mem.Allocator,
    },

    /// Represents a raw address. Only used internally by the interpreter.
    raw_address: struct {
        /// The raw address that this lvalue refers to
        adrs: i64,
        /// Allocator that allocated this lvalue
        allocator: std.mem.Allocator,
    },

    fn create_symbver(symbver: *Symbol_Version, allocator: std.mem.Allocator) *L_Value {
        const retval = allocator.create(L_Value) catch unreachable;
        retval.* = L_Value{ .symbver = symbver };
        return retval;
    }

    pub fn create_unversioned_symbver(symbol: *Symbol, allocator: std.mem.Allocator) *L_Value {
        const retval = L_Value.create_symbver(Symbol_Version.create_unversioned(symbol, allocator), allocator);
        return retval;
    }

    pub fn create_dereference_lval(lhs: *L_Value, expanded_type: *Type_AST, allocator: std.mem.Allocator) *L_Value {
        const retval = allocator.create(L_Value) catch unreachable;
        retval.* = L_Value{
            .dereference = .{
                .expr = lhs,
                .expanded_type = expanded_type,
                .allocator = allocator,
            },
        };
        return retval;
    }

    pub fn create_index_lval(
        lhs: *L_Value,
        rhs: *L_Value,
        length: ?*L_Value,
        expanded_type: *Type_AST,
        allocator: std.mem.Allocator,
    ) *L_Value {
        const retval = allocator.create(L_Value) catch unreachable;
        retval.* = L_Value{ .index = .{
            .lhs = lhs,
            .rhs = rhs,
            .length = length,
            .expanded_type = expanded_type,
            .allocator = allocator,
        } };
        return retval;
    }

    pub fn create_select_lval(
        lhs: *L_Value,
        field: i128,
        offset: i64,
        expanded_type: *Type_AST,
        tag: ?*L_Value,
        allocator: std.mem.Allocator,
    ) *L_Value {
        const retval = allocator.create(L_Value) catch unreachable;
        retval.* = L_Value{
            .select = .{
                .lhs = lhs,
                .field = field,
                .offset = offset,
                .expanded_type = expanded_type,
                .tag = tag,
                .allocator = allocator,
            },
        };
        return retval;
    }

    pub fn create_raw_address_lval(adrs: i64, allocator: std.mem.Allocator) *L_Value {
        const retval = allocator.create(L_Value) catch unreachable;
        retval.* = L_Value{
            .raw_address = .{ .adrs = adrs, .allocator = allocator },
        };
        return retval;
    }

    pub fn deinit(self: *L_Value) void {
        switch (self.*) {
            .symbver => self.symbver.deinit(),
            .dereference => self.dereference.allocator.destroy(self),
            .index => self.index.allocator.destroy(self),
            .select => self.select.allocator.destroy(self),
            .raw_address => self.raw_address.allocator.destroy(self),
        }
    }

    pub fn pprint(self: L_Value, allocator: std.mem.Allocator) ![]const u8 {
        var out = std.array_list.Managed(u8).init(allocator);
        defer out.deinit();

        switch (self) {
            .symbver => {
                try out.print("{f}", .{self.symbver});
            },
            .dereference => {
                try out.print("{f}^", .{self.dereference.expr});
            },
            .index => {
                try out.print("{f}[{f}]", .{ self.index.lhs, self.index.rhs });
            },
            .select => {
                try out.print("{f}._{}", .{ self.select.lhs, self.select.field });
            },
            .raw_address => {
                try out.print("0x{X}", .{self.raw_address.adrs});
            },
        }

        return try out.toOwnedSlice();
    }

    pub fn format(self: L_Value, writer: *std.io.Writer) !void {
        try fmt_.indirect_format(self, writer);
    }

    pub fn extract_symbver(self: *L_Value) *Symbol_Version {
        return switch (self.*) {
            .symbver => self.symbver,
            .dereference => self.dereference.expr.extract_symbver(),
            .index => self.index.lhs.extract_symbver(),
            .select => self.select.lhs.extract_symbver(),
            .raw_address => std.debug.panic("compiler error: raw addresses do not have symbvers", .{}),
        };
    }

    pub fn expanded_type_sizeof(self: *L_Value) ?i64 {
        return self.get_expanded_type().sizeof();
    }

    pub fn expanded_type_alignof(self: *L_Value) ?i64 {
        return self.get_expanded_type().alignof();
    }

    pub fn get_expanded_type(self: *L_Value) *Type_AST {
        switch (self.*) {
            .symbver => return self.symbver.symbol.expanded_type(),
            .dereference => return self.dereference.expanded_type,
            .index => return self.index.expanded_type,
            .select => return self.select.expanded_type,
            .raw_address => std.debug.panic("compiler error: raw addresses do not have types", .{}),
        }
    }

    pub fn lval_precedence(self: *L_Value) Instruction.Precedence {
        return switch (self.*) {
            .symbver => Instruction.Kind.precedence(Instruction.Kind.load_symbol),
            .dereference => Instruction.Precedence.prefix,
            .index => Instruction.Precedence.prefix,
            .select => Instruction.Precedence.postfix,
            .raw_address => std.debug.panic("compiler error: raw addresses do not have precedence", .{}),
        };
    }

    pub fn reset_usage(lval: *L_Value) void {
        switch (lval.*) {
            .symbver => {
                lval.symbver.uses = 0;
                lval.symbver.symbol.uses = 0;
            },
            .dereference => lval.dereference.expr.reset_usage(),
            .index => {
                lval.index.lhs.reset_usage();
                lval.index.rhs.reset_usage();
                if (lval.index.length != null) {
                    lval.index.length.?.reset_usage();
                }
            },
            .select => {
                lval.select.lhs.reset_usage();
                if (lval.select.tag != null) {
                    lval.select.tag.?.reset_usage();
                }
            },
            .raw_address => std.debug.panic("compiler error: raw addresses do not have usage", .{}),
        }
    }

    pub fn increment_usage(lval: *L_Value) void {
        switch (lval.*) {
            .symbver => {
                lval.symbver.uses += 1;
                lval.symbver.symbol.uses += 1;
            },
            .dereference => lval.dereference.expr.increment_usage(),
            .index => {
                lval.index.lhs.increment_usage();
                lval.index.rhs.increment_usage();
                if (lval.index.length != null) {
                    lval.index.length.?.increment_usage();
                }
            },
            .select => {
                lval.select.lhs.increment_usage();
                if (lval.select.tag != null) {
                    lval.select.tag.?.increment_usage();
                }
            },
            .raw_address => std.debug.panic("compiler error: raw addresses do not have usage", .{}),
        }
    }
};
