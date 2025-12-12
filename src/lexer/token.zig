const std = @import("std");
const Span = @import("../util/span.zig");

const Self = @This();

pub const Kind = enum(u32) {
    // Literals
    identifier,
    dec_int,
    hex_int,
    oct_int,
    bin_int,
    float,
    char,
    string,
    true,
    false,
    multi_line_string,

    // Keywords
    as,
    @"and",
    @"break",
    @"catch",
    cinclude,
    @"comptime",
    @"const",
    context,
    @"continue",
    @"defer",
    dyn,
    @"else",
    @"enum",
    @"errdefer",
    @"extern",
    @"fn",
    @"for",
    @"if",
    impl,
    import,
    in,
    let,
    match,
    mut,
    not,
    @"or",
    @"orelse",
    @"return",
    @"test",
    trait,
    @"try",
    type,
    @"unreachable",
    where,
    with,
    @"while",
    self,
    @"struct",
    undefined,
    variadic,
    virtual,

    // Equals
    double_equals,
    single_equals,
    minus_equals,
    e_mark_equals,
    percent_equals,
    plus_equals,
    slash_equals,
    star_equals,
    double_lesser_equals,
    double_greater_equals,
    ampersand_equals,
    bar_equals,
    tilde_equals,

    // Math
    bar,
    caret,
    double_bar,
    fat_arrow,
    greater,
    greater_equal,
    lesser,
    lesser_equal,
    minus,
    percent,
    plus,
    skinny_arrow,
    slash,
    star,

    // Bitwise
    double_lesser,
    double_greater,
    tilde,

    // Punctuation
    ampersand,
    single_colon,
    double_colon,
    comma,
    double_period,
    triple_period,
    exclamation_mark,
    period,
    question_mark,
    semicolon,
    at_symbol,

    // Open/Close
    left_brace,
    left_parenthesis,
    left_square,
    right_brace,
    right_parenthesis,
    right_square,

    // Trait stuff
    invoke,

    // Whitespace
    comment,
    newline,
    EOF,

    // HACK: Used to count how many constructors are in the enum
    // (yes, this is needed in Zig)
    len,

    pub fn is_end_token(self: Kind) bool {
        for (end_tokens) |kind| {
            if (self == kind) {
                return true;
            }
        }
        return false;
    }

    pub fn repr(kind: Kind) ?[]const u8 {
        return switch (kind) {
            .bin_int,
            .char,
            .dec_int,
            .hex_int,
            .identifier,
            .oct_int,
            .float,
            .string,
            .len,
            => null,

            .comment => "<a comment>",
            .newline => "<a newline>",

            .as => "as",
            .@"and" => "and",
            .@"break" => "break",
            .@"catch" => "catch",
            .cinclude => "cinclude",
            .@"comptime" => "comptime",
            .@"const" => "const",
            .context => "context",
            .@"continue" => "continue",
            .@"defer" => "defer",
            .dyn => "dyn",
            .@"else" => "else",
            .@"enum" => "enum",
            .@"errdefer" => "errdefer",
            .@"extern" => "extern",
            .false => "false",
            .@"fn" => "fn",
            .@"for" => "for",
            .@"if" => "if",
            .impl => "impl",
            .import => "import",
            .in => "in",
            .let => "let",
            .match => "match",
            .mut => "mut",
            .not => "not",
            .@"or" => "or",
            .@"orelse" => "orelse",
            .@"return" => "return",
            .@"test" => "test",
            .true => "true",
            .@"try" => "try",
            .@"unreachable" => "unreachable",
            .where => "where",
            .with => "with",
            .@"while" => "while",
            .trait => "trait",
            .type => "type",
            .self => "self",
            .@"struct" => "struct",
            .undefined => "undefined",
            .variadic => "variadic",
            .virtual => "virtual",

            // Equals
            .double_equals => "==",
            .single_equals => "=",
            .minus_equals => "-=",
            .e_mark_equals => "!=",
            .percent_equals => "%=",
            .plus_equals => "+=",
            .slash_equals => "/=",
            .star_equals => "*=",
            .double_lesser_equals => "<<=",
            .double_greater_equals => ">>=",
            .ampersand_equals => "&=",
            .bar_equals => "|=",
            .tilde_equals => "~=",

            // Math
            .bar => "|",
            .caret => "^",
            .double_bar => "||",
            .fat_arrow => "=>",
            .greater => ">",
            .greater_equal => ">=",
            .lesser => "<",
            .lesser_equal => "<=",
            .minus => "-",
            .percent => "%",
            .plus => "+",
            .skinny_arrow => "->",
            .slash => "/",
            .star => "*",

            // Bitwise
            .double_lesser => "<<",
            .double_greater => ">>",
            .tilde => "~",

            // Punctuation
            .ampersand => "&",
            .single_colon => ":",
            .double_colon => "::",
            .comma => ",",
            .double_period => "..",
            .triple_period => "...",
            .exclamation_mark => "!",
            .period => ".",
            .question_mark => "?",
            .semicolon => ";",
            .multi_line_string => "<multi-line>",
            .at_symbol => "@",

            // Open/Close
            .left_brace => "{",
            .left_parenthesis => "(",
            .left_square => "[",
            .right_brace => "}",
            .right_parenthesis => ")",
            .right_square => "]",

            // Functional
            .invoke => ".>",

            // EOF
            .EOF => "(EOF)",
        };
    }

    pub fn from_string(data: []const u8) Kind {
        var ix: usize = 0;
        const num_ctors = @intFromEnum(Kind.len);

        while (ix < num_ctors) : (ix += 1) {
            const kind: Kind = @enumFromInt(ix);
            const repr_kind: ?[]const u8 = kind.repr();
            if (repr_kind) |_repr| {
                if (std.mem.eql(u8, data, _repr)) {
                    // Found the kind!
                    return kind;
                }
            }
        }
        return Kind.identifier;
    }

    const end_tokens = [_]Kind{
        .identifier,
        .self,
        .dec_int,
        .hex_int,
        .oct_int,
        .bin_int,
        .float,
        .char,
        .string,
        .@"unreachable",
        .@"break",
        .@"continue",
        .@"return",
        .@"else",
        .right_parenthesis,
        .right_square,
        .right_brace,
        .true,
        .false,
        .caret,
        .undefined,
    };
};

// What kind of token this is
kind: Kind,
// Non-owning slice into the contents of the source file the text data for this token comes from
data: []const u8,
span: Span,

pub fn init(data: []const u8, kind: ?Kind, filename: []const u8, line_text: []const u8, line_number: usize, col: usize) Self {
    return .{ .data = data, .kind = kind orelse Kind.from_string(data), .span = Span{
        .filename = filename,
        .line_text = line_text,
        .line_number = line_number,
        .col = col,
    } };
}

// Used to create a simple, anonymous identifier token
pub fn init_simple(data: []const u8) Self {
    return Self.init(data, .identifier, "", "", 0, 0);
}

pub fn pprint(self: *Self) void {
    std.debug.print("Token {{line: {:03}, kind: {s}, data: {s}}}\n", .{ self.span.line_number, self.repr(), self.data });
}
