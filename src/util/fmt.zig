const std = @import("std");

pub fn List_Printer(Elem: type) type {
    return struct {
        list: *const std.array_list.Managed(*Elem),
        pub fn format(self: *const @This(), writer: *std.io.Writer) !void {
            try writer.print("[", .{});
            for (self.list.items, 0..) |item, i| {
                try writer.print("{f}", .{item});
                if (i < self.list.items.len -| 1) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print("]", .{});
        }
    };
}

/// Constructs a new buffer to be printed to, and passes it to the object's `pprint` method.
///
/// This is required for objects who call `print` inside format, since this is not allowed.
pub fn indirect_format(obj: anytype, writer: *std.io.Writer) !void {
    const T = @TypeOf(obj);
    std.debug.assert(@hasDecl(T, "pprint"));

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const out = obj.pprint(arena.allocator()) catch unreachable;

    try writer.print("{s}", .{out});
}
