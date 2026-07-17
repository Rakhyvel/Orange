// kcov-ignore-start
const std = @import("std");

pub const Color = enum {
    none,
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bright_black,
    bright_red,
    bright_green,
    bright_yellow,
    bright_blue,
    bright_magenta,
    bright_cyan,
    bright_white,
};

pub const Attr = struct {
    fg: Color = .white,
    bg: Color = .none,

    // Pretty much all style escape codes I could find, minus a few that are
    // not widely supported.
    bold: bool = false,
    dimmed: bool = false,
    italic: bool = false,
    underline: bool = false,
    blinking: bool = false,
    reverse: bool = false,
    hidden: bool = false,
    overline: bool = false,
    strikethrough: bool = false,

    fn eql(self: Attr, other: Attr) bool {
        inline for (@typeInfo(Attr).Struct.fields) |field| {
            if (@field(self, field.name) != @field(other, field.name)) return false;
        }
        return true;
    }

    pub fn dump(self: Attr, writer: anytype) !void {
        try writer.writeAll("\x1B[0");
        if (self.bold) try writer.writeAll(";1");
        if (self.dimmed) try writer.writeAll(";2");
        if (self.italic) try writer.writeAll(";3");
        if (self.underline) try writer.writeAll(";4");
        if (self.blinking) try writer.writeAll(";5");
        if (self.reverse) try writer.writeAll(";7");
        if (self.hidden) try writer.writeAll(";8");
        if (self.overline) try writer.writeAll(";53");
        if (self.strikethrough) try writer.writeAll(";9");
        switch (self.fg) {
            .none => {},
            .black => try writer.writeAll(";30"),
            .red => try writer.writeAll(";31"),
            .green => try writer.writeAll(";32"),
            .yellow => try writer.writeAll(";33"),
            .blue => try writer.writeAll(";34"),
            .magenta => try writer.writeAll(";35"),
            .cyan => try writer.writeAll(";36"),
            .white => try writer.writeAll(";37"),
            .bright_black => try writer.writeAll(";90"),
            .bright_red => try writer.writeAll(";91"),
            .bright_green => try writer.writeAll(";92"),
            .bright_yellow => try writer.writeAll(";93"),
            .bright_blue => try writer.writeAll(";94"),
            .bright_magenta => try writer.writeAll(";95"),
            .bright_cyan => try writer.writeAll(";96"),
            .bright_white => try writer.writeAll(";97"),
        }
        switch (self.bg) {
            .none => {},
            .black => try writer.writeAll(";40"),
            .red => try writer.writeAll(";41"),
            .green => try writer.writeAll(";42"),
            .yellow => try writer.writeAll(";43"),
            .blue => try writer.writeAll(";44"),
            .magenta => try writer.writeAll(";45"),
            .cyan => try writer.writeAll(";46"),
            .white => try writer.writeAll(";74"),
            .bright_black => try writer.writeAll(";100"),
            .bright_red => try writer.writeAll(";101"),
            .bright_green => try writer.writeAll(";102"),
            .bright_yellow => try writer.writeAll(";103"),
            .bright_blue => try writer.writeAll(";104"),
            .bright_magenta => try writer.writeAll(";105"),
            .bright_cyan => try writer.writeAll(";106"),
            .bright_white => try writer.writeAll(";107"),
        }
        try writer.writeAll("m");
    }
};

pub fn outputColor(attr: Attr, contents: []const u8, writer: *std.io.Writer) !void {
    try attr.dump(writer);
    try writer.print("{s}", .{contents});
    try (Attr{}).dump(writer);
}

// kcov-ignore-end
