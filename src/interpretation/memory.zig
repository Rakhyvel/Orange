const std = @import("std");

const Self: type = @This();

const memory_debugger: bool = false;

memory: []u8,

pub fn init(memory: []u8) Self {
    return Self{ .memory = memory };
}

/// Moves a block of memory from the source address to the destination address in the interpreter's memory.
pub fn move(self: *Self, dest: i64, src: i64, len: i64) void {
    if (memory_debugger) {
        std.debug.print("dest:0x{X} src:0x{X} len:0x{X}\n", .{ dest, src, len });
    }
    std.debug.assert(dest >= 0);
    std.debug.assert(src >= 0);
    if (len == 0) {
        // moving no bytes is a no-op
        return;
    }
    std.debug.assert(dest != src); // dest is not src
    if (dest > src) {
        std.debug.assert(dest >= src + len); // dest is not within src
    } else {
        std.debug.assert(src >= dest + len); // src is not within dest
    }
    @memcpy(
        self.memory[@as(usize, @intCast(dest))..@as(usize, @intCast(dest + len))],
        self.memory[@as(usize, @intCast(src))..@as(usize, @intCast(src + len))],
    );
}

/// Stores an integer value at the specified address in the interpreter's memory.
pub fn store_int(self: *Self, address: i64, size: i64, val: i128) void {
    std.debug.assert(address >= 0);
    switch (size) {
        1 => self.store(i8, address, @as(i8, @intCast(val))),
        2 => self.store(i16, address, @as(i16, @intCast(val))),
        4 => self.store(i32, address, @as(i32, @intCast(val))),
        8 => self.store(i64, address, @as(i64, @intCast(val))),
        else => std.debug.panic("interpreter error: cannot store an int of size {}\n", .{size}),
    }
}

/// Stores a floating-point value at the specified address in the interpreter's memory.
pub fn store_float(self: *Self, address: i64, size: i64, val: f64) void {
    std.debug.assert(address >= 0);
    switch (size) {
        4 => self.store(f32, address, @as(f32, @floatCast(val))),
        8 => self.store(f64, address, val),
        else => unreachable,
    }
}
/// Loads an integer value from the specified address in the interpreter's memory.
pub fn load_int(self: *Self, address: i64, size: i64) i128 {
    std.debug.assert(address >= 0);
    return switch (size) {
        1 => self.load(i8, address),
        2 => self.load(i16, address),
        4 => self.load(i32, address),
        8 => self.load(i64, address),
        else => unreachable,
    };
}

/// Loads an integer value from the specified address in the interpreter's memory.
pub fn load_unsigned_int(self: *Self, address: i64, size: i64) u64 {
    std.debug.assert(address >= 0);
    return switch (size) {
        1 => self.load(u8, address),
        2 => self.load(u16, address),
        4 => self.load(u32, address),
        8 => self.load(u64, address),
        else => unreachable,
    };
}

/// Loads a floating-point value from the specified address in the interpreter's memory.
pub fn load_float(self: *Self, address: i64, size: i64) f64 {
    std.debug.assert(address >= 0);
    return switch (size) {
        4 => self.load(f32, address),
        8 => self.load(f64, address),
        else => unreachable,
    };
}

/// Stores a value of type T at the specified address in the interpreter's memory.
pub fn store(self: *Self, comptime T: type, address: i64, val: T) void {
    std.debug.assert(address >= 0);
    // std.debug.print("[0x{X}:{}] = {}\n", .{ address, @alignOf(T), val });
    @as(*T, @ptrCast(@alignCast(&self.memory[@as(usize, @intCast(address))]))).* = val;
}

/// Loads a value of type T from the specified address in the interpreter's memory.
pub fn load(self: *Self, comptime T: type, address: i64) T {
    std.debug.assert(address >= 0);
    const val = @as(*T, @ptrCast(@alignCast(&self.memory[@as(usize, @intCast(address))]))).*;
    // std.debug.print("[0x{X}:{}] \t// {}\n", .{ address, @alignOf(T), val });
    return val;
}

pub fn print_memory(self: *Self, start: usize, end: usize) void {
    for (start..end) |i| {
        if (@rem(i, 8) == 0) {
            std.debug.print("0x{0X:0>2.}: ", .{i});
        }
        std.debug.print("{X:0>2.}", .{self.memory[i]});
        if (@rem(i, 8) == 7) {
            std.debug.print(" ", .{});
        }
        if (@rem(i, 32) == 31) {
            std.debug.print("\n", .{});
        }
    }
}
