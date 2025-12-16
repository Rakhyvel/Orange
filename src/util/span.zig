const std = @import("std");
const String = @import("../zig-string/zig-string.zig").String;
const fmt_ = @import("../util/fmt.zig");

const Self = @This();

pub const Span_Print_Error = error{OutOfMemory};

const Span_Format = struct {
    fmt_str: []const u8,
    sanitize: bool,
};

pub const c_format: Span_Format = .{
    .fmt_str = "\"{s}:{}:{}:\\n{s}\\n{s}^\"",
    .sanitize = true,
};

pub const interpreter_format: Span_Format = .{
    .fmt_str = "{s}:{}:{}:\n{s}\n{s}^\n",
    .sanitize = false,
};

pub const phony = Self{ .filename = "", .line_text = "", .line_number = 0, .col = 0 };

filename: []const u8,
line_text: []const u8, // The entire gosh-darn line from the source file text that this span originates
line_number: usize,
col: usize,

/// Prints out a line string, with quotes and arrow.
pub fn print_debug_line(self: *const Self, writer: *std.array_list.Managed(u8), comptime span_format: Span_Format) Span_Print_Error!void {
    var spaces = String.init_with_contents(std.heap.page_allocator, "") catch unreachable;
    defer spaces.deinit();

    if (self.col > 0) {
        for (1..self.col -| 1) |_| {
            spaces.insert(" ", spaces.size) catch unreachable;
        }
    }

    const sanitized_filename = if (span_format.sanitize)
        sanitize_string(self.filename, std.heap.page_allocator)
    else
        self.filename;

    const text_to_write = if (span_format.sanitize)
        sanitize_string(self.line_text, std.heap.page_allocator)
    else
        self.line_text;

    try writer.print(span_format.fmt_str, .{
        sanitized_filename,
        self.line_number,
        self.col,
        text_to_write,
        spaces.str(),
    });

    if (span_format.sanitize) {
        std.heap.page_allocator.free(sanitized_filename);
        std.heap.page_allocator.free(text_to_write);
    }
}

pub fn format(self: Self, writer: *std.io.Writer) !void {
    try writer.print("{s}:{}:{}", .{ self.filename, self.line_number, self.col });
}

/// Sanitizes a string, escaping proper characters.
///
/// This is needed so that string escapes in Orange do not escape in the generate C source.
///
/// For example:
///     let str = some_function("You better \n sanitize me!")
///
/// Would produce an error diagnostic in debug mode, which needs to escape the `\` correctly so
/// that the error shown to the user is identical to the text in the file.
fn sanitize_string(str: []const u8, allocator: std.mem.Allocator) []const u8 {
    var builder = String.init_with_contents(allocator, "") catch unreachable;
    for (str) |byte| {
        if (byte == '\\' or byte == '"') {
            builder.insert("\\", builder.len()) catch unreachable;
        }
        if (byte == '\r') {
            builder.insert("\\r", builder.len()) catch unreachable;
        } else if (byte == '\n') {
            builder.insert("\\n", builder.len()) catch unreachable;
        } else {
            const insert_me: [1]u8 = .{byte};
            builder.insert(&insert_me, builder.len()) catch unreachable;
        }
    }
    return (builder.toOwned() catch unreachable).?;
}
