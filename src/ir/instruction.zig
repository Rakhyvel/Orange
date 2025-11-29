const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Basic_Block = @import("../ir/basic-block.zig");
const errs_ = @import("../util/errors.zig");
const lval_ = @import("../ir/lval.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Span = @import("../util/span.zig");
const String_Idx = @import("../ir/interned_string_set.zig").String_Idx;
const Symbol = @import("../symbol/symbol.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const fmt_ = @import("../util/fmt.zig");

const Self = @This();

pub const Index: type = u32;

var instruction_uid: u64 = 0;
/// Unique identifier for this Self. Mainly used for debugging.
uid: u64,
kind: Kind,
dest: ?*lval_.L_Value,
src1: ?*lval_.L_Value,
src2: ?*lval_.L_Value,

data: Data,

in_block: ?*Basic_Block,
span: Span,

allocator: std.mem.Allocator,

pub fn init(
    kind: Kind,
    dest: ?*lval_.L_Value,
    src1: ?*lval_.L_Value,
    src2: ?*lval_.L_Value,
    span: Span,
    allocator: std.mem.Allocator,
) *Self {
    var retval = allocator.create(Self) catch unreachable;
    retval.kind = kind;
    retval.dest = dest;
    retval.src1 = src1;
    retval.src2 = src2;
    retval.uid = instruction_uid;
    retval.in_block = null;
    retval.data = Data.none;
    retval.span = span;
    retval.allocator = allocator;
    instruction_uid += 1;
    return retval;
}

pub fn init_int(dest: *lval_.L_Value, int: i128, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.load_int, dest, null, null, span, allocator);
    retval.data = Data{ .int = int };
    return retval;
}

pub fn init_float(dest: *lval_.L_Value, float: f64, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.load_float, dest, null, null, span, allocator);
    retval.data = Data{ .float = float };
    return retval;
}

pub fn init_string(dest: *lval_.L_Value, id: String_Idx, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.load_string, dest, null, null, span, allocator);
    retval.data = Data{ .string_id = id };
    return retval;
}

pub fn init_symbol(dest: *lval_.L_Value, symbol: *Symbol, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.load_symbol, dest, null, null, span, allocator);
    retval.data = Data{ .symbol = symbol };
    return retval;
}

pub fn init_simple_copy(dest: *lval_.L_Value, src: *lval_.L_Value, span: Span, allocator: std.mem.Allocator) *Self {
    const instr = Self.init(.copy, dest, src, null, span, allocator);
    return instr;
}

pub fn init_label(name: []const u8, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.label, null, null, null, span, allocator);
    retval.data = Data{ .label = .{ .name = name } };
    return retval;
}

pub fn init_branch(condition: *lval_.L_Value, label: ?*Self, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.branch_if_false, null, condition, null, span, allocator);
    retval.data = Data{ .branch = label };
    return retval;
}

pub fn init_jump(label: ?*Self, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.jump, null, null, null, span, allocator);
    retval.data = Data{ .branch = label };
    return retval;
}

pub fn init_jump_addr(next: ?*Basic_Block, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.jump, null, null, null, span, allocator);
    retval.data = Data{ .jump_bb = .{ .next = next } };
    return retval;
}

pub fn init_branch_addr(
    condition: *lval_.L_Value,
    next: ?*Basic_Block,
    branch: ?*Basic_Block,
    span: Span,
    allocator: std.mem.Allocator,
) *Self {
    var retval = Self.init(.branch_if_false, null, condition, null, span, allocator);
    retval.data = Data{ .branch_bb = .{ .next = next, .branch = branch } };
    return retval;
}

pub fn init_call(dest: *lval_.L_Value, src1: *lval_.L_Value, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.call, dest, src1, null, span, allocator);
    retval.data = Data{ .call = .{
        .arg_lval_list = std.array_list.Managed(*lval_.L_Value).init(allocator),
        .context_arg_lval_list = std.array_list.Managed(*lval_.L_Value).init(allocator),
    } };
    return retval;
}

pub fn init_invoke(dest: *lval_.L_Value, method_decl: *ast_.AST, lval_list: std.array_list.Managed(*lval_.L_Value), dyn_value: ?*lval_.L_Value, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.invoke, dest, null, null, span, allocator);
    retval.data = Data{
        .invoke = .{
            .method_decl = method_decl,
            .arg_lval_list = lval_list,
            .context_arg_lval_list = std.array_list.Managed(*lval_.L_Value).init(allocator),
            .dyn_value = dyn_value,
            .method_decl_lval = null, // Filled in later
        },
    };
    return retval;
}

pub fn init_dyn(dest: *lval_.L_Value, src1: *lval_.L_Value, mut: bool, impl: *ast_.AST, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(if (mut) .mut_dyn_value else .dyn_value, dest, src1, null, span, allocator);
    retval.data = Data{ .dyn = .{ .impl = impl } };
    return retval;
}

pub fn init_load_struct(dest: *lval_.L_Value, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.load_struct, dest, null, null, span, allocator);
    retval.data = Data{ .lval_list = std.array_list.Managed(*lval_.L_Value).init(allocator) };
    return retval;
}

pub fn init_get_tag(dest: *lval_.L_Value, src1: *lval_.L_Value, span: Span, allocator: std.mem.Allocator) *Self {
    const retval = Self.init(.get_tag, dest, src1, null, span, allocator);
    return retval;
}

pub fn init_union(dest: *lval_.L_Value, _init: ?*lval_.L_Value, tag: i128, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.load_union, dest, _init, null, span, allocator);
    retval.data = Data{ .int = tag };
    return retval;
}

pub fn init_stack_push(span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.push_stack_trace, null, null, null, span, allocator);
    retval.data = Data.none;
    return retval;
}

pub fn init_stack_pop(span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.pop_stack_trace, null, null, null, span, allocator);
    retval.data = Data.none;
    return retval;
}

pub fn init_stack_err_pop(span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.pop_err_trace, null, null, null, span, allocator);
    retval.data = Data.none;
    return retval;
}

pub fn init_panic(message: []const u8, span: Span, allocator: std.mem.Allocator) *Self {
    var retval = Self.init(.panic, null, null, null, span, allocator);
    retval.data = Data{ .string = message };
    return retval;
}

pub fn init_int_float_kind(
    int_kind: Kind,
    float_kind: Kind,
    dest: ?*lval_.L_Value,
    src1: ?*lval_.L_Value,
    src2: ?*lval_.L_Value,
    span: Span,
    allocator: std.mem.Allocator,
) *Self {
    if (src1.?.get_expanded_type().can_represent_float()) {
        return init(float_kind, dest, src1, src2, span, allocator);
    } else {
        return init(int_kind, dest, src1, src2, span, allocator);
    }
}

pub fn deinit(self: *Self) void {
    if (self.dest != null) {
        self.dest.?.deinit();
    }
    self.allocator.destroy(self);
}

pub fn pprint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var out = std.array_list.Managed(u8).init(allocator);
    defer out.deinit();

    switch (self.kind) {
        .label => try out.print("{s}{}:\n", .{ self.data.label.name, self.uid }),
        .load_int => {
            try out.print("    {f} := {}\n", .{ self.dest.?, self.data.int });
        },
        .load_float => {
            try out.print("    {f} := {}\n", .{ self.dest.?, self.data.float });
        },
        .load_symbol => {
            try out.print("    {f} := {s}\n", .{ self.dest.?, self.data.symbol.name });
        },
        .load_struct => {
            try out.print("    {f} := {{", .{self.dest.?});
            try print_lval_list(self.data.lval_list, &out);
            try out.print("}}\n", .{});
        },
        .load_union => {
            // init may be null, if it is unit
            try out.print("    {f} := {{init={?f}, tag={}}}\n", .{ self.dest.?, self.src1, self.data.int });
        },
        .load_string => {
            try out.print("    {f} := <interned string:{}>\n", .{ self.dest.?, self.data.string_id });
        },
        .load_unit => {
            try out.print("    {f} := {{}}\n", .{self.dest.?});
        },
        .copy => {
            try out.print("    {f} := {?f} // {f}\n", .{ self.dest.?, self.src1, self.dest.?.get_expanded_type() });
        },
        .not => {
            try out.print("    {f} := !{f}\n", .{ self.dest.?, self.src1.? });
        },
        .negate_int => {
            try out.print("    {f} := -{f}\n", .{ self.dest.?, self.src1.? });
        },
        .negate_float => {
            try out.print("    {f} := -.{f}\n", .{ self.dest.?, self.src1.? });
        },
        .size_of => {
            try out.print("    {f} := sizeof({f})\n", .{ self.dest.?, self.src1.? });
        },
        .addr_of => {
            try out.print("    {f} := &{f}\n", .{ self.dest.?, self.src1.? });
        },
        .mut_addr_of => {
            try out.print("    {f} := &mut {f}\n", .{ self.dest.?, self.src1.? });
        },
        .equal => {
            try out.print("    {f} := {f} == {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .not_equal => {
            try out.print("    {f} := {f} != {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .greater_int => {
            try out.print("    {f} := {f} > {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .greater_float => {
            try out.print("    {f} := {f} >. {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .lesser_int => {
            try out.print("    {f} := {f} < {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .lesser_float => {
            try out.print("    {f} := {f} <. {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .greater_equal_int => {
            try out.print("    {f} := {f} >= {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .greater_equal_float => {
            try out.print("    {f} := {f} >=. {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .lesser_equal_int => {
            try out.print("    {f} := {f} <= {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .lesser_equal_float => {
            try out.print("    {f} := {f} <=. {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .add_int => {
            try out.print("    {f} := {f} + {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .add_float => {
            try out.print("    {f} := {f} +. {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .sub_int => {
            try out.print("    {f} := {f} - {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .sub_float => {
            try out.print("    {f} := {f} -. {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .mult_int => {
            try out.print("    {f} := {f} * {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .mult_float => {
            try out.print("    {f} := {f} *. {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .div_int => {
            try out.print("    {f} := {f} / {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .div_float => {
            try out.print("    {f} := {f} /. {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .mod => {
            try out.print("    {f} := {f} % {f}\n", .{ self.dest.?, self.src1.?, self.src2.? });
        },
        .get_tag => {
            try out.print("    {f} := {f}.tag\n", .{ self.dest.?, self.src1.? });
        },
        .jump => {
            if (self.data == .jump_bb and self.data.jump_bb.next != null) {
                try out.print("    jump BB{}\n", .{self.data.jump_bb.next.?.uid});
            } else if (self.data == .branch and self.data.branch != null) {
                try out.print("    jump {s}{}\n", .{ self.data.branch.?.data.label.name, self.data.branch.?.uid });
            } else {
                try out.print("    ret\n", .{});
            }
        },
        .branch_if_false => {
            if (self.data == .branch_bb and self.data.branch_bb.next != null) {
                try out.print("    if ({f}) jump BB{}", .{ self.src1.?, self.data.branch_bb.next.?.uid });
            } else if (self.data == .branch and self.data.branch != null) {
                try out.print("    if (!{f}) jump {s}{}\n", .{ self.src1.?, self.data.branch.?.data.label.name, self.data.branch.?.uid });
            } else {
                try out.print("    if ({f}) ret", .{self.src1.?});
            }
            if (self.data == .branch_bb and self.data.branch_bb.branch != null) {
                try out.print(" else jump BB{}\n", .{self.data.branch_bb.branch.?.uid});
            } else if (self.data != .branch) {
                try out.print(" else ret\n", .{});
            }
        },
        .call => {
            try out.print("    {f} := {f}(", .{ self.dest.?, self.src1.? });
            try print_lval_list(self.data.call.arg_lval_list, &out); // TODO: print context args, too
            try out.print(")\n", .{});
        },
        .invoke => {
            try out.print("    {f} := ::{s}(", .{ self.dest.?, self.data.invoke.method_decl.method_decl.name.token().data });
            try print_lval_list(self.data.invoke.arg_lval_list, &out); // TODO: print context args, too
            try out.print(")\n", .{});
        },
        .push_stack_trace => {
            try out.print("    push-stack-trace\n", .{});
        },
        .pop_stack_trace => {
            try out.print("    pop-stack-trace\n", .{});
        },
        .panic => {
            try out.print("    panic\n", .{});
        },
        else => {
            try out.print("<TODO: {s}>\n", .{@tagName(self.kind)});
        },
    }

    return try out.toOwnedSlice();
}

pub fn format(self: Self, writer: *std.io.Writer) !void {
    try fmt_.indirect_format(self, writer);
}

fn print_lval_list(lval_list: std.array_list.Managed(*lval_.L_Value), writer: *std.array_list.Managed(u8)) !void {
    for (lval_list.items, 0..) |lval, i| {
        try writer.print("{f}", .{lval});
        if (i < lval_list.items.len - 1) {
            try writer.print(", ", .{});
        }
    }
}

pub fn copy_prop(self: *Self, src1_def: ?*Self, kind: Kind, errors: *errs_.Errors) error{CompileError}!bool {
    if (src1_def != null and src1_def.?.kind == kind) {
        if (kind == .load_int) {
            try assert_fits(src1_def.?.data.int, self.dest.?.get_expanded_type(), self.span, errors);
        }
        self.kind = kind;
        self.data = src1_def.?.data;
        self.span = src1_def.?.span;
        self.src2 = src1_def.?.src2;
        self.src1 = src1_def.?.src1;
        return true;
    } else {
        return false;
    }
}

/// Converts a non-load Instruction to a load Instruction, with data. Performs bounds checking for load_int destination Instruction Kinds.
pub fn convert_to_load(self: *Self, kind: Kind, data: Data, errors: *errs_.Errors) error{CompileError}!void {
    if (kind == .load_int) {
        try assert_fits(data.int, self.dest.?.get_expanded_type(), self.span, errors);
    }
    self.kind = kind;
    self.data = data;
    self.src1 = null;
    self.src2 = null;
}

/// Converts a non-unop Instruction to an unop Instruction.
pub fn convert_to_unop(self: *Self, src1: *lval_.L_Value, kind: Kind) void {
    self.kind = kind;
    self.data = .none;
    self.src1 = src1;
    self.src2 = null;
}

/// Checks if a value fits within the bounds of a given type and reports an overflow error if it does not, modifying the
/// error list.
fn assert_fits(val: i128, _type: *Type_AST, span: Span, errors: *errs_.Errors) error{CompileError}!void {
    const bounds = prelude_.bounds_from_ast(_type);
    if (bounds != null and (val < bounds.?.lower or val > bounds.?.upper)) {
        errors.add_error(errs_.Error{ .integer_out_of_bounds = .{
            .span = span,
            .expected = _type,
            .value = val,
        } });
        return error.CompileError;
    }
}

/// Determines if a source value can be safely replaced by its definition in the Instruction and updates the source if possible,
/// modifying src when propagation is performed.
pub fn copy_of_prop(self: *Self, src: *?*lval_.L_Value, src_def: ?*Self) bool {
    if (self.kind != .addr_of and self.kind != .mut_addr_of and // instr is not an address, this could mess with aliasing
        src_def != null and // src has a definition
        src_def.?.kind == .copy and // src's definition is a copy of something
        src_def.?.src1.?.* == .symbver and // src's copy definition is of a plain symbver
        (src_def.?.dest.?.* != .symbver or src_def.?.src1.?.extract_symbver() != src_def.?.dest.?.extract_symbver()) // prevent self-copy
    ) {
        // Find recent version of what src's definition copies
        const src_def_redefinition = src_def.?.in_block.?.any_def_after(src_def.?, src_def.?.src1.?.symbver.symbol, self);
        if (src_def_redefinition == null) {
            // there is no re-definition in between src's definition and this Instruction, safe to copy-propagate
            // std.debug.print("copy-of propagation: {?} => {?} {}", .{ src, src_def.?.src1, self });
            src.* = src_def.?.src1;
            return true;
        } else {
            // there was another definition in between src's definition, cannot copy-propagate.
            // the redefinition is likely junk, too. ex: `x[3] = 4` <= here we cannot just replace `x` with `4`.
            return false;
        }
    } else {
        // src's definition isn't in a good shape to look up re-definitions. Cannot do copy-propagation.
        return false;
    }
}

pub fn self_eq(self: *Self) bool {
    return self.src1.?.* == .symbver and self.src2.?.* == .symbver and self.src1.?.symbver.symbol == self.src2.?.symbver.symbol;
}

pub const Precedence = enum(usize) {
    literal = 0,
    postfix = 1,
    prefix = 2,
    multiplicative = 3,
    additive = 4,
    bitwise = 5,
    comparative = 6,
    equality = 7,
    copy = 14,
    highest = 100,
};

pub const Kind = enum {
    // nullary instructions
    load_symbol,
    load_extern,
    load_int,
    load_float,
    load_struct,
    load_union, // src1 is init, data.int is tag id
    load_string,
    load_unit, // no-op, but required. DO NOT optimize away

    // Monadic instructions
    copy,
    not,
    negate_int,
    negate_float,
    bit_not,
    size_of, //< For extern types that Orange can't do automatically
    addr_of,
    mut_addr_of, // Separate kind to allow for aliasing analysis
    dyn_value,
    mut_dyn_value, // Separate kind to allow for aliasing analysis

    // Diadic instructions
    equal,
    not_equal,
    greater_int,
    greater_float,
    lesser_int,
    lesser_float,
    greater_equal_int,
    greater_equal_float,
    lesser_equal_int,
    lesser_equal_float,
    bit_and,
    bit_or,
    bit_xor,
    left_shift,
    right_shift,
    add_int,
    add_float,
    sub_int,
    sub_float,
    mult_int,
    mult_float,
    div_int,
    div_float,
    mod,
    get_tag, // dest = src1.tag
    cast,

    // Control-flow
    label,
    jump, // jump to BB{uid} if codegen, ip := {addr} if interpreting
    branch_if_false,
    call, // dest = src1(symbver_list...)
    invoke, // dest = {impl-vtable}.{method_name}(symbver_list...)

    // Errors
    push_stack_trace, // Pushes a static span/code to the lines array if debug mode is on
    pop_stack_trace, // Pops a message off the stack after a function is successfully called
    pop_err_trace,
    panic, // if debug mode is on, panics with a message, unrolls lines stack, exits

    pub fn is_checked(self: Kind) bool {
        return switch (self) {
            .negate_int,
            .add_int,
            .sub_int,
            .mult_int,
            .div_int,
            .mod,
            => true,
            else => false,
        };
    }

    pub fn checked_name(self: Kind) []const u8 {
        return switch (self) {
            .negate_int => "negate",
            .add_int => "add",
            .sub_int => "sub",
            .mult_int => "mult",
            .div_int => "div",
            .mod => "mod",
            else => unreachable,
        };
    }

    pub fn c_token(self: Kind) []const u8 {
        return switch (self) {
            .not => "!",
            .bit_not => "~",
            .negate_int, .negate_float => "-",
            .equal => "==",
            .not_equal => "!=",
            .greater_int, .greater_float => ">",
            .lesser_int, .lesser_float => "<",
            .greater_equal_int, .greater_equal_float => ">=",
            .lesser_equal_int, .lesser_equal_float => "<=",
            .add_int, .add_float => "+",
            .sub_int, .sub_float => "-",
            .mult_int, .mult_float => "*",
            .div_int, .div_float => "/",
            .mod => "%",
            .bit_and => "&",
            .bit_or => "|",
            .bit_xor => "^",
            .left_shift => "<<",
            .right_shift => ">>",
            else => unreachable,
        };
    }

    pub fn arity(self: Kind) enum { unop, binop } {
        return switch (self) {
            .not, .negate_int, .negate_float, .bit_not => .unop,
            .equal,
            .not_equal,
            .greater_int,
            .greater_float,
            .lesser_int,
            .lesser_float,
            .greater_equal_int,
            .greater_equal_float,
            .lesser_equal_int,
            .lesser_equal_float,
            .add_int,
            .add_float,
            .sub_int,
            .sub_float,
            .mult_int,
            .mult_float,
            .div_int,
            .div_float,
            .mod,
            .bit_and,
            .bit_or,
            .bit_xor,
            .left_shift,
            .right_shift,
            => .binop,
            else => unreachable,
        };
    }

    pub fn precedence(self: Kind) Precedence {
        return switch (self) {
            .load_symbol,
            .load_extern,
            .load_int,
            .load_float,
            .load_struct,
            .load_union,
            .load_string,
            => Precedence.literal,

            .call,
            .get_tag,
            => Precedence.postfix,

            .negate_int,
            .negate_float,
            .not,
            .bit_not,
            .size_of,
            .cast,
            .mut_addr_of,
            .addr_of,
            .mut_dyn_value,
            .dyn_value,
            => Precedence.prefix,

            .mult_int,
            .mult_float,
            .div_int,
            .div_float,
            .mod,
            => Precedence.multiplicative,

            .add_int,
            .add_float,
            .sub_int,
            .sub_float,
            => Precedence.additive,

            .bit_and, .bit_or, .bit_xor, .left_shift, .right_shift => Precedence.bitwise,

            .greater_int,
            .greater_float,
            .lesser_int,
            .lesser_float,
            .greater_equal_int,
            .greater_equal_float,
            .lesser_equal_int,
            .lesser_equal_float,
            => Precedence.comparative,

            .equal,
            .not_equal,
            => Precedence.equality,

            .copy => Precedence.copy,

            else => std.debug.panic("compiler error: unimplemented precedence for kind {s}", .{@tagName(self)}),
        };
    }
};

pub const Data = union(enum) {
    branch: ?*Self,
    jump_bb: struct { next: ?*Basic_Block },
    branch_bb: struct { next: ?*Basic_Block, branch: ?*Basic_Block },
    int: i128,
    float: f64,
    string_id: String_Idx,
    label: struct {
        name: []const u8,
    },
    string: []const u8,
    symbol: *Symbol,
    lval_list: std.array_list.Managed(*lval_.L_Value),
    call: struct {
        arg_lval_list: std.array_list.Managed(*lval_.L_Value),
        context_arg_lval_list: std.array_list.Managed(*lval_.L_Value),
    },
    invoke: struct {
        method_decl: *ast_.AST, // AST of method decl. Either trait's definition if dyn, or impl's declaration if static
        method_decl_lval: ?*lval_.L_Value, // L-value of the method; non-null when statically known (ie not dyn)
        arg_lval_list: std.array_list.Managed(*lval_.L_Value), // List of args. Receiver will always be prepended
        context_arg_lval_list: std.array_list.Managed(*lval_.L_Value), // List of args. Receiver will always be prepended
        dyn_value: ?*lval_.L_Value, // L-value of the vtable-data-ptr pair; non-null when the call is through a vtable, regardless of if the method uses the receiver
    },
    dyn: struct { impl: *ast_.AST },
    select: struct { offset: i128, field: i128 },
    ast: *ast_.AST,
    none,

    pub fn pprint(self: Data, allocator: std.mem.Allocator) ![]const u8 {
        var out = std.array_list.Managed(u8).init(allocator, "") catch unreachable;
        defer out.deinit();

        switch (self) {
            .int => {
                try out.print("{}", .{self.int});
            },
            .float => {
                try out.print("{d:.6}", .{self.float});
            },
            .none => {
                try out.print("none", .{});
            },
            .symbol => {
                try out.print("{s}", .{self.symbol.name});
            },
            .string_id => {
                try out.print("<interned string:{}>", .{self.string_id});
            },
            else => {
                try out.print("??? ({s})", .{@tagName(self)});
            },
        }

        return (try out.toOwned()).?;
    }

    pub fn format(self: Data, writer: *std.io.Writer) !void {
        try fmt_.indirect_format(self, writer);
    }

    pub fn equals(self: Data, other: Data) bool {
        if (self == .branch and other == .branch) {
            return self.branch == other.branch;
        } else if (self == .int and other == .int) {
            return self.int == other.int;
        } else if (self == .float and other == .float) {
            return self.float == other.float;
        } else if (self == .int and other == .float) {
            return self.int == @as(i128, @intFromFloat(other.float)); // These will crash if float is not representable by i128
        } else if (self == .float and other == .int) { //                This should not be a problem with user-generated code, since it's type-checked
            return @as(i128, @intFromFloat(self.float)) == other.int; // But keep it in mind for compiler-generated code generated after type-checking, such as defaults
        } else if (self == .string_id and other == .string_id) {
            return self.string_id == other.string_id;
        } else if (self == .string and other == .string) {
            return std.mem.eql(u8, self.string, other.string);
        } else if (self == .symbol and other == .symbol) {
            return self.symbol == other.symbol;
        } else {
            return false; // tags are mismatched
        }
    }

    // TODO: Fix these when removing i128

    pub fn add_int_overflow(self: Data, other: Data) error{CompileError}!Data {
        return Data{
            .int = std.math.add(i128, self.int, other.int) catch unreachable,
        };
    }

    pub fn sub_int_overflow(self: Data, other: Data) error{CompileError}!Data {
        return Data{
            .int = std.math.sub(i128, self.int, other.int) catch unreachable,
        };
    }

    pub fn mult_int_overflow(self: Data, other: Data) error{CompileError}!Data {
        return Data{
            .int = std.math.mul(i128, self.int, other.int) catch unreachable,
        };
    }
};
