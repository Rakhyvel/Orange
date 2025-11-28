const std = @import("std");
const AST = @import("../ast/ast.zig").AST;
const errs_ = @import("../util/errors.zig");
const Span = @import("../util/span.zig");

const pipeline_ = @import("../util/pipeline.zig");

const Split_Lines = @import("../lexer/split_lines.zig");
const Tokenize = @import("../lexer/tokenize.zig");
const Apply_Layout = @import("../lexer/apply_layout.zig");
const Parse = @import("../parser/parse.zig");

const Self = @This();

fmt_str: *AST,
errors: *errs_.Errors,
allocator: std.mem.Allocator,

i: usize = 0,

pub fn init(fmt_str: *AST, errors: *errs_.Errors, allocator: std.mem.Allocator) Self {
    std.debug.assert(fmt_str.* == .string);
    return Self{
        .fmt_str = fmt_str,
        .errors = errors,
        .allocator = allocator,
    };
}

/// Takes a format string and returns a list of things to print out
pub fn parse_fmt_string(self: *Self) !std.array_list.Managed(*AST) {
    var retval = std.array_list.Managed(*AST).init(self.allocator);

    const data = self.fmt_str.string.data;
    var prev: usize = 0;
    while (self.i < data.len) : (self.i += 1) {
        if (data[self.i] == '{') {
            try self.ensure_not_end();

            if (data[self.i + 1] == '{') {
                try retval.append(AST.create_string(self.fmt_str.token(), data[prev..self.i], self.allocator));
                self.i += 1;
                prev = self.i;
            } else {
                const fmt_bracket_open = self.i;
                while (self.i < data.len and data[self.i] != '}') : (self.i += 1) {}
                try self.ensure_not_end();
                try self.ensure_not_empty(fmt_bracket_open);
                if (fmt_bracket_open > prev) {
                    try retval.append(AST.create_string(self.fmt_str.token(), data[prev..fmt_bracket_open], self.allocator));
                    prev = fmt_bracket_open;
                }

                const asts = pipeline_.run(data[fmt_bracket_open + 1 .. self.i], .{
                    Split_Lines.init(self.errors, self.allocator),
                    Tokenize.init(self.fmt_str.token().span.filename, self.fmt_str.token().span.line_number, self.fmt_str.token().span.col, self.errors, false, self.allocator),
                    Parse.init(.expr, self.errors, self.allocator),
                }) catch return error.ParseError;

                try retval.append(asts.items[0]);
                prev = self.i + 1;
            }
        } else if (data[self.i] == '}') {
            self.i += 1;
            try self.ensure_not_end();
            try self.expect_closing_brace();
            try retval.append(AST.create_string(self.fmt_str.token(), data[prev .. self.i - 2], self.allocator));
            prev = self.i;
        }
    }

    if (self.i > prev) {
        try retval.append(AST.create_string(self.fmt_str.token(), data[prev..self.i], self.allocator));
    }

    return retval;
}

fn expect_closing_brace(self: *Self) !void {
    if (self.fmt_str.string.data[self.i] != '}') {
        var err_span = self.fmt_str.token().span;
        err_span.col -= self.fmt_str.string.data.len;
        err_span.col += self.i;
        self.errors.add_error(errs_.Error{ .basic = .{
            .span = err_span,
            .msg = "expected `}`",
        } });
        return error.ParseError;
    }
}

fn ensure_not_end(self: *Self) !void {
    if (self.i >= self.fmt_str.string.data.len) {
        var err_span = self.fmt_str.token().span;
        err_span.col -= self.fmt_str.string.data.len;
        err_span.col += self.i;
        self.errors.add_error(errs_.Error{ .basic = .{
            .span = err_span,
            .msg = "unexpected end of format string",
        } });
        return error.ParseError;
    }
}

fn ensure_not_empty(self: *Self, bracket_open_pos: usize) !void {
    if (self.i - bracket_open_pos <= 1) {
        var err_span = self.fmt_str.token().span;
        err_span.col -= self.fmt_str.string.data.len;
        err_span.col += bracket_open_pos;
        self.errors.add_error(errs_.Error{ .basic = .{
            .span = err_span,
            .msg = "empty format argument",
        } });
        return error.ParseError;
    }
}
