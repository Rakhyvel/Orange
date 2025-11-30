const std = @import("std");
const Span = @import("../util/span.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Token = @import("../lexer/token.zig");

const Self: type = @This();

pub fn init() Self {
    return Self{};
}

pub fn run(self: Self, tokens: *std.array_list.Managed(Token)) !*std.array_list.Managed(Token) {
    _ = self;
    var i: usize = 0;
    while (i < tokens.items.len - 1) : (i += 1) {
        const token = tokens.items[i];
        if (token.kind != .at_symbol) continue;
        const next_token = tokens.items[i + 1];
        if (next_token.kind != .string) continue;

        var new_token = next_token;
        new_token.data = next_token.data[1 .. next_token.data.len - 1];
        new_token.kind = .identifier;
        _ = tokens.orderedRemove(i);
        _ = tokens.orderedRemove(i);
        try tokens.insert(i, new_token);
        i -= 1;
    }
    return tokens;
}
