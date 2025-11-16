// TODO: Separate `Errors` and `Error` to their own file

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Span = @import("../util/span.zig");
const Symbol = @import("../symbol/symbol.zig");
const term_ = @import("term.zig");
const Token = @import("../lexer/token.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

pub const Error_Config = struct {
    print_full_path: bool = true,
    print_color: bool = true,
};

pub const Error = union(enum) {
    // General errors
    basic: struct { // TODO: Add a fmt'd constructor for one of these in the Error struct
        span: Span,
        msg: []const u8,
    },

    // File errors
    cannot_open: struct {
        span: Span,
        filename: []const u8,
    },

    // Lexer errors
    invalid_digit: struct {
        span: Span,
        digit: u8,
        base: []const u8,
    },
    invalid_escape: struct {
        span: Span,
        digit: u8,
    },

    // Parse errors
    expected_basic_token: struct {
        expected: []const u8,
        got: Token,
    },
    expected2token: struct {
        expected: Token.Kind,
        got: Token,
    },
    missing_close: struct {
        expected: Token.Kind,
        got: Token,
        open: Token,
    },
    comptime_known: struct {
        span: Span,
        what: []const u8, // what must be compile-time known?
    },

    // Symbol
    redefinition: struct {
        first_defined_span: Span,
        redefined_span: Span,
        name: []const u8,
    },
    symbol_error: struct {
        span: Span,
        context_span: ?Span = null,
        name: []const u8,
        problem: []const u8,
        context_message: []const u8 = "",
    },
    discard_marked: struct {
        span: Span,
        kind: Symbol.Kind,
    },
    not_inside_loop: struct {
        span: Span,
        name: []const u8,
    },
    not_inside_function: struct {
        span: Span,
        name: []const u8,
    },
    import_file_not_found: struct {
        span: Span,
        filename: []const u8,
    },
    unapplied_generic: struct {
        span: Span,
        symbol_name: []const u8,
        num_generics: usize,
    },

    // Traits
    reimpl: struct {
        first_defined_span: Span,
        redefined_span: Span,
        name: ?[]const u8,
        _type: *Type_AST,
    },
    type_not_impl_method: struct { // TODO: This should take in an enum to determine if the name is of a method or of a member
        span: Span,
        method_name: []const u8,
        _type: *Type_AST,
    },
    type_not_impl_trait: struct {
        span: Span,
        trait_name: []const u8,
        _type: *Type_AST,
    },
    method_not_in_trait: struct {
        method_span: Span,
        method_name: []const u8,
        trait_name: []const u8,
    },
    method_not_in_impl: struct {
        impl_span: Span,
        method_span: Span,
        method_name: []const u8,
        trait_name: []const u8,
    },
    impl_receiver_mismatch: struct {
        receiver_span: Span,
        impl_receiver: ?ast_.Receiver_Kind,
        trait_receiver: ?ast_.Receiver_Kind,
        method_name: []const u8,
        trait_name: []const u8,
    },
    invoke_receiver_mismatch: struct {
        receiver_span: Span,
        method_receiver: ast_.Receiver_Kind,
        method_name: []const u8,
        lhs_type: *Type_AST,
    },
    mismatch_method_param_arity: struct {
        span: Span,
        trait_arity: usize,
        impl_arity: usize,
        method_name: []const u8,
        trait_name: []const u8,
    },
    mismatch_method_type: struct {
        span: Span,
        trait_type: *Type_AST,
        impl_type: *Type_AST,
        method_name: []const u8,
        trait_name: []const u8,
    },
    mismatch_method_virtuality: struct {
        span: Span,
        trait_method_is_virtual: bool,
        impl_method_is_virtual: bool,
        method_name: []const u8,
        trait_name: []const u8,
    },
    trait_virtual_refers_to_self: struct {
        span: Span,
        method_name: []const u8,
        trait_name: []const u8,
    },

    // Contexts
    missing_context: struct {
        span: Span,
        context: *Type_AST,
    },

    // Typecheck
    unexpected_type: struct {
        span: Span,
        expected: *Type_AST,
        got: *Type_AST,
    },
    non_convertible: struct {
        span: Span,
        from: *Type_AST,
        to: *Type_AST,
    },
    unexpected_type_type: struct {
        span: Span,
        expected: ?*Type_AST,
    },
    expected_builtin_typeclass: struct {
        span: Span,
        expected: []const u8, // name of the type class
        got: *Type_AST,
    },
    duplicate: struct {
        span: Span,
        identifier: []const u8,
        first: ?Span, // printed writer as extra information
    },
    member_not_in_type: struct {
        span: Span,
        identifier: []const u8,
        name: []const u8,
        type: *Type_AST,
    },
    member_not_in_module: struct {
        span: Span,
        identifier: []const u8,
        name: []const u8,
        module_name: []const u8,
    },
    undeclared_identifier: struct {
        identifier: Token,
        expected: ?*ast_.AST,
    },
    comptime_access_runtime: struct {
        identifier: Token,
    },
    inner_fn_access_runtime: struct {
        identifier: Token,
    },
    use_before_def: struct {
        identifier: Token,
    },
    modify_immutable: struct {
        identifier: Token,
    },
    not_selectable: struct {
        span: Span,
        _type: *Type_AST,
    },
    not_indexable: struct {
        span: Span,
        _type: *Type_AST,
    },
    not_subsliceable: struct {
        span: Span,
        _type: *Type_AST,
    },
    bad_index: struct {
        span: Span,
        _type: *Type_AST,
        index: i128,
        length: usize,
    },
    non_exhaustive_sum: struct {
        span: Span,
        forgotten: std.array_list.Managed(*Type_AST),
    },
    mismatch_arity: struct {
        span: Span,
        takes: usize,
        given: usize,
        thing_name: []const u8,
        takes_name: []const u8,
        given_name: []const u8,
    },
    no_default: struct {
        span: Span,
        _type: *Type_AST,
    },
    wrong_from: struct {
        span: Span,
        operator: []const u8, // either "orelse" or "catch"
        from: []const u8, // either "optional" or "error"
        got: *Type_AST, // What the type actually was
    },
    integer_out_of_bounds: struct {
        span: Span,
        expected: *Type_AST,
        value: i128,
    },
    invalid_type: struct {
        span: Span,
        got: *Type_AST,
    },
    recursive_definition: struct {
        span: Span,
        symbol_name: ?[]const u8,
    },

    // TODO: Add span() and get field like for AST
    fn get_span(self: *const Error) ?Span {
        switch (self.*) {
            .basic => return self.basic.span,

            .cannot_open => return self.cannot_open.span,

            .invalid_digit => return self.invalid_digit.span,
            .invalid_escape => return self.invalid_escape.span,

            .expected_basic_token => return self.expected_basic_token.got.span,
            .expected2token => return self.expected2token.got.span,
            .missing_close => return self.missing_close.got.span,
            .comptime_known => return self.comptime_known.span,

            .redefinition => return self.redefinition.redefined_span,
            .symbol_error => return self.symbol_error.span,
            .discard_marked => return self.discard_marked.span,
            .not_inside_loop => return self.not_inside_loop.span,
            .not_inside_function => return self.not_inside_function.span,
            .import_file_not_found => return self.import_file_not_found.span,
            .unapplied_generic => return self.unapplied_generic.span,

            .reimpl => return self.reimpl.redefined_span,
            .type_not_impl_method => return self.type_not_impl_method.span,
            .type_not_impl_trait => return self.type_not_impl_trait.span,
            .method_not_in_trait => return self.method_not_in_trait.method_span,
            .method_not_in_impl => return self.method_not_in_impl.impl_span,
            .impl_receiver_mismatch => return self.impl_receiver_mismatch.receiver_span,
            .invoke_receiver_mismatch => return self.invoke_receiver_mismatch.receiver_span,
            .mismatch_method_param_arity => return self.mismatch_method_param_arity.span,
            .mismatch_method_type => return self.mismatch_method_type.span,
            .mismatch_method_virtuality => return self.mismatch_method_virtuality.span,
            .trait_virtual_refers_to_self => return self.trait_virtual_refers_to_self.span,

            .missing_context => return self.missing_context.span,

            .unexpected_type => return self.unexpected_type.span,
            .non_convertible => return self.non_convertible.span,
            .unexpected_type_type => return self.unexpected_type_type.span,
            .expected_builtin_typeclass => return self.expected_builtin_typeclass.span,
            .duplicate => return self.duplicate.span,
            .member_not_in_type => return self.member_not_in_type.span,
            .member_not_in_module => return self.member_not_in_module.span,
            .undeclared_identifier => return self.undeclared_identifier.identifier.span,
            .comptime_access_runtime => return self.comptime_access_runtime.identifier.span,
            .inner_fn_access_runtime => return self.inner_fn_access_runtime.identifier.span,
            .use_before_def => return self.use_before_def.identifier.span,
            .modify_immutable => return self.modify_immutable.identifier.span,
            .not_indexable => return self.not_indexable.span,
            .not_subsliceable => return self.not_subsliceable.span,
            .bad_index => return self.bad_index.span,
            .not_selectable => return self.not_selectable.span,
            .non_exhaustive_sum => return self.non_exhaustive_sum.span,
            .mismatch_arity => return self.mismatch_arity.span,
            .no_default => return self.no_default.span,
            .wrong_from => return self.wrong_from.span,
            .integer_out_of_bounds => return self.integer_out_of_bounds.span,
            .invalid_type => return self.invalid_type.span,
            .recursive_definition => return self.recursive_definition.span,
        }
    }

    pub fn fatal_error(self: Error, writer: *std.io.Writer, conf: Error_Config) noreturn {
        self.print_error(writer, conf);
        std.process.exit(1);
    }

    pub fn print_error(self: Error, writer: *std.io.Writer, conf: Error_Config) void {
        print_color(bold, writer, conf);
        print_label(self.get_span(), "error: ", .red, writer, conf);
        self.print_msg(writer);
        print_color(not_bold, writer, conf);
        print_epilude(self.get_span(), writer, conf);
        self.print_extra_info(writer, conf);
    }

    fn print_msg(err: Error, writer: *std.io.Writer) void {
        switch (err) {
            // General errors
            .basic => writer.print("{s}\n", .{err.basic.msg}) catch unreachable,

            // File errors
            .cannot_open => writer.print("cannot open file {s}\n", .{err.cannot_open.filename}) catch unreachable,

            // Lexer errors
            .invalid_digit => writer.print("'{c}' is not a valid {s} digit\n", .{ err.invalid_digit.digit, err.invalid_digit.base }) catch unreachable,
            .invalid_escape => writer.print("invalid escape sequence '\\{c}'\n", .{err.invalid_escape.digit}) catch unreachable,

            // Parser errors
            .expected_basic_token => writer.print("expected {s}, got `{s}`\n", .{
                err.expected_basic_token.expected,
                err.expected_basic_token.got.kind.repr() orelse err.expected_basic_token.got.data,
            }) catch unreachable,
            .expected2token => writer.print("expected `{s}`, got `{s}`\n", .{
                err.expected2token.expected.repr() orelse "identifier",
                err.expected2token.got.kind.repr() orelse err.expected2token.got.data,
            }) catch unreachable,
            .missing_close => {
                writer.print("expected closing `{s}` to match opening `{s}` here, got `{s}`\n", .{
                    err.missing_close.expected.repr() orelse "",
                    err.missing_close.open.kind.repr() orelse err.missing_close.open.data,
                    err.missing_close.got.kind.repr() orelse err.missing_close.got.data,
                }) catch unreachable;
            },
            .comptime_known => writer.print("{s} must be compile-time known\n", .{err.comptime_known.what}) catch unreachable,

            // Symbol
            .redefinition => writer.print("redefinition of symbol `{s}`\n", .{err.redefinition.name}) catch unreachable,
            .symbol_error => writer.print("symbol `{s}` {s}\n", .{ err.symbol_error.name, err.symbol_error.problem }) catch unreachable,
            .discard_marked => writer.print("discarded symbol marked as `{s}`\n", .{@tagName(err.discard_marked.kind)}) catch unreachable,
            .not_inside_loop => writer.print("`{s}` is not inside a loop\n", .{err.not_inside_loop.name}) catch unreachable,
            .not_inside_function => writer.print("`{s}` is not inside a function\n", .{err.not_inside_function.name}) catch unreachable,
            .import_file_not_found => writer.print("file `{s}.orng` not found\n", .{err.import_file_not_found.filename}) catch unreachable,
            .unapplied_generic => writer.print("symbol `{s}` requires {} generic arguments\n", .{ err.unapplied_generic.symbol_name, err.unapplied_generic.num_generics }) catch unreachable,

            // Traits
            .reimpl => if (err.reimpl.name != null) {
                writer.print("duplicate implementation of trait `{s}` for the type `", .{err.reimpl.name.?}) catch unreachable;
                err.reimpl._type.print_type(writer) catch unreachable;
                writer.print("`\n", .{}) catch unreachable;
            } else {
                writer.print("duplicate implementation for the type `", .{}) catch unreachable;
                err.reimpl._type.print_type(writer) catch unreachable;
                writer.print("`\n", .{}) catch unreachable;
            },
            .type_not_impl_method => {
                writer.print("the type `", .{}) catch unreachable;
                err.type_not_impl_method._type.print_type(writer) catch unreachable;
                writer.print("` does not implement the method `{s}`\n", .{err.type_not_impl_method.method_name}) catch unreachable;
            },
            .type_not_impl_trait => {
                writer.print("the type `", .{}) catch unreachable;
                err.type_not_impl_trait._type.print_type(writer) catch unreachable;
                writer.print("` does not implement the trait `{s}`\n", .{err.type_not_impl_trait.trait_name}) catch unreachable;
            },
            .method_not_in_trait => writer.print("method `{s}` is not in trait `{s}`\n", .{
                err.method_not_in_trait.method_name,
                err.method_not_in_trait.trait_name,
            }) catch unreachable,
            .method_not_in_impl => {
                writer.print("missing implementation of method `{s}` from trait `{s}`\n", .{
                    err.method_not_in_impl.method_name,
                    err.method_not_in_impl.trait_name,
                }) catch unreachable;
            },
            .impl_receiver_mismatch => if (err.impl_receiver_mismatch.trait_receiver != null and err.impl_receiver_mismatch.impl_receiver != null) {
                writer.print("trait `{s}` specifies receiver `{s}` for method `{s}`, got receiver `{s}`\n", .{
                    err.impl_receiver_mismatch.trait_name,
                    err.impl_receiver_mismatch.trait_receiver.?.to_string(),
                    err.impl_receiver_mismatch.method_name,
                    err.impl_receiver_mismatch.impl_receiver.?.to_string(),
                }) catch unreachable;
            } else if (err.impl_receiver_mismatch.trait_receiver == null and err.impl_receiver_mismatch.impl_receiver != null) {
                writer.print("trait `{s}` does not specify a receiver for method `{s}`, got receiver `{s}`\n", .{
                    err.impl_receiver_mismatch.trait_name,
                    err.impl_receiver_mismatch.method_name,
                    err.impl_receiver_mismatch.impl_receiver.?.to_string(),
                }) catch unreachable;
            } else if (err.impl_receiver_mismatch.trait_receiver != null and err.impl_receiver_mismatch.impl_receiver == null) {
                writer.print("trait `{s}` specifies receiver `{s}` for method `{s}`\n", .{
                    err.impl_receiver_mismatch.trait_name,
                    err.impl_receiver_mismatch.trait_receiver.?.to_string(),
                    err.impl_receiver_mismatch.method_name,
                }) catch unreachable;
            },
            .invoke_receiver_mismatch => {
                writer.print("receiver `{s}` of method `{s}` incompatible with type `", .{
                    err.invoke_receiver_mismatch.method_receiver.to_string(),
                    err.invoke_receiver_mismatch.method_name,
                }) catch unreachable;
                err.invoke_receiver_mismatch.lhs_type.print_type(writer) catch unreachable;
                writer.print("`\n", .{}) catch unreachable;
            },
            .mismatch_method_param_arity => writer.print("trait `{s}` specifies {} parameter{s} for method `{s}`, got {}\n", .{
                err.mismatch_method_param_arity.trait_name,
                err.mismatch_method_param_arity.trait_arity,
                if (err.mismatch_method_param_arity.trait_arity != 1) "s" else "",
                err.mismatch_method_param_arity.method_name,
                err.mismatch_method_param_arity.impl_arity,
            }) catch unreachable,
            .mismatch_method_type => {
                writer.print("trait `{s}` specifies type `", .{err.mismatch_method_type.trait_name}) catch unreachable;
                err.mismatch_method_type.trait_type.print_type(writer) catch unreachable;
                writer.print("` for method `{s}`, got `", .{err.mismatch_method_type.method_name}) catch unreachable;
                err.mismatch_method_type.impl_type.print_type(writer) catch unreachable;
                writer.print("`\n", .{}) catch unreachable;
            },
            .mismatch_method_virtuality => {
                writer.print("trait `{s}` specifies the method `{s}` as {s}, got {s}\n", .{
                    err.mismatch_method_virtuality.trait_name,
                    err.mismatch_method_virtuality.method_name,
                    if (err.mismatch_method_virtuality.trait_method_is_virtual) "virtual" else "non-virtual",
                    if (err.mismatch_method_virtuality.impl_method_is_virtual) "virtual" else "non-virtual",
                }) catch unreachable;
            },
            .trait_virtual_refers_to_self => {
                writer.print("virtual method `{s}` of trait `{s}` refers to `Self` type\n", .{
                    err.trait_virtual_refers_to_self.method_name,
                    err.trait_virtual_refers_to_self.trait_name,
                }) catch unreachable;
            },

            // Contexts
            .missing_context => {
                writer.print("missing context `{f}`\n", .{
                    err.missing_context.context,
                }) catch unreachable;
            },

            // Typecheck
            .unexpected_type => if (err.unexpected_type.expected.* == .identifier and std.mem.eql(u8, err.unexpected_type.expected.token().data, "Void")) {
                std.debug.assert(err.unexpected_type.expected.* != .poison);
                writer.print("cannot assign a value of type `", .{}) catch unreachable;
                err.unexpected_type.got.print_type(writer) catch unreachable;
                writer.print("` to type `Void`\n", .{}) catch unreachable;
            } else {
                std.debug.assert(err.unexpected_type.expected.* != .poison);
                writer.print("expected a value of type `", .{}) catch unreachable;
                err.unexpected_type.expected.print_type(writer) catch unreachable;
                if (err.unexpected_type.got.* != .poison) {
                    writer.print("`, got a value of type `", .{}) catch unreachable;
                    err.unexpected_type.got.print_type(writer) catch unreachable;
                }
                writer.print("`\n", .{}) catch unreachable;
            },
            .non_convertible => {
                writer.print("cannot cast type `{f}` to type `{f}`\n", .{ err.non_convertible.from, err.non_convertible.to }) catch unreachable;
            },
            .unexpected_type_type => {
                if (err.unexpected_type_type.expected) |expected| {
                    writer.print("expected a value of type `{f}`, got a type\n", .{expected}) catch unreachable;
                } else {
                    writer.print("unexpected type\n", .{}) catch unreachable;
                }
            },
            .expected_builtin_typeclass => {
                writer.print("expected a value of a(n) {s} type, got `", .{err.expected_builtin_typeclass.expected}) catch unreachable;
                err.expected_builtin_typeclass.got.print_type(writer) catch unreachable;
                writer.print("`\n", .{}) catch unreachable;
            },
            .duplicate => writer.print("duplicate item `{s}`\n", .{err.duplicate.identifier}) catch unreachable,
            .member_not_in_type => {
                writer.print("member `{s}` not in {s} `{f}`\n", .{ err.member_not_in_type.identifier, err.member_not_in_type.name, err.member_not_in_type.type }) catch unreachable;
            },
            .member_not_in_module => {
                writer.print("member `{s}` not in {s} `{s}`\n", .{ err.member_not_in_module.identifier, err.member_not_in_module.name, err.member_not_in_module.module_name }) catch unreachable;
            },
            .undeclared_identifier => writer.print("use of undeclared identifier `{s}`\n", .{err.undeclared_identifier.identifier.data}) catch unreachable,
            .comptime_access_runtime => writer.print("cannot access non-const variable `{s}` in a comptime context\n", .{
                err.comptime_access_runtime.identifier.data,
            }) catch unreachable,
            .inner_fn_access_runtime => writer.print("cannot access non-const variable `{s}` from an inner-function\n", .{
                err.inner_fn_access_runtime.identifier.data,
            }) catch unreachable,
            .use_before_def => writer.print("use of identifier `{s}` before its definition\n", .{err.use_before_def.identifier.data}) catch unreachable,
            .modify_immutable => writer.print("cannot modify non-mutable symbol `{s}`\n", .{err.modify_immutable.identifier.data}) catch unreachable,
            .not_indexable => {
                writer.print("the type `", .{}) catch unreachable;
                err.not_indexable._type.print_type(writer) catch unreachable;
                writer.print("` is not indexable\n", .{}) catch unreachable;
            },
            .not_subsliceable => {
                writer.print("cannot take a sub-slice of the type `", .{}) catch unreachable;
                err.not_subsliceable._type.print_type(writer) catch unreachable;
                writer.print("`\n", .{}) catch unreachable;
            },
            .bad_index => {
                writer.print("cannot index the type `{f}`, which has length {}, with index {}\n", .{ err.bad_index._type, err.bad_index.length, err.bad_index.index }) catch unreachable;
            },
            .not_selectable => {
                writer.print("the type `", .{}) catch unreachable;
                err.not_selectable._type.print_type(writer) catch unreachable;
                writer.print("` is not selectable\n", .{}) catch unreachable;
            },
            .non_exhaustive_sum => writer.print("match over enum type is not exhaustive\n", .{}) catch unreachable,
            .mismatch_arity => {
                writer.print("{s} takes {} {s}{s}, {} {s}{s} given\n", .{
                    err.mismatch_arity.thing_name,
                    err.mismatch_arity.takes,
                    err.mismatch_arity.takes_name,
                    if (err.mismatch_arity.takes == 1) "" else "s",
                    err.mismatch_arity.given,
                    err.mismatch_arity.given_name,
                    if (err.mismatch_arity.given == 1) "" else "s",
                }) catch unreachable;
            },
            .no_default => {
                writer.print("no default value for type `", .{}) catch unreachable;
                err.no_default._type.print_type(writer) catch unreachable;
                writer.print("`\n", .{}) catch unreachable;
            },
            .wrong_from => {
                writer.print("expected {s} type for {s}, got `", .{
                    err.wrong_from.from,
                    err.wrong_from.operator,
                }) catch unreachable;
                err.wrong_from.got.print_type(writer) catch unreachable;
                writer.print("`\n", .{}) catch unreachable;
            },
            .integer_out_of_bounds => {
                writer.print("error: integer is out of bounds for type `", .{}) catch unreachable;
                err.integer_out_of_bounds.expected.print_type(writer) catch unreachable;
                writer.print("`; value={}\n", .{err.integer_out_of_bounds.value}) catch unreachable;
            },
            .invalid_type => {
                writer.print("error: expected a compile-time constant type, got {s}\n", .{@tagName(err.invalid_type.got.*)}) catch unreachable;
            },
            .recursive_definition => if (err.recursive_definition.symbol_name) |symbol_name| {
                writer.print("error: recursive definition of symbol `{s}` detected\n", .{symbol_name}) catch unreachable;
            } else {
                writer.print("error: recursive definition detected\n", .{}) catch unreachable;
            },
        }
    }

    fn print_extra_info(self: Error, writer: *std.io.Writer, conf: Error_Config) void {
        // FIXME: High Cyclo
        switch (self) {
            .missing_close => {
                print_color(bold, writer, conf);
                print_note_label(self.missing_close.open.span, writer, conf);
                print_color(bold, writer, conf);
                writer.print("opening `{s}` here:\n", .{self.missing_close.open.kind.repr() orelse self.missing_close.open.data}) catch unreachable;
                print_color(not_bold, writer, conf);
                print_epilude(self.missing_close.open.span, writer, conf);
            },
            .comptime_known => {
                print_color(bold, writer, conf);
                print_note_label(self.comptime_known.span, writer, conf);
                print_color(bold, writer, conf);
                writer.print("consider wrapping with `comptime`\n", .{}) catch unreachable;
                print_color(not_bold, writer, conf);
            },
            .redefinition => {
                if (self.redefinition.first_defined_span.line_number != 0) { // Don't print redefinitions for places that don't exist
                    print_color(bold, writer, conf);
                    print_note_label(self.redefinition.first_defined_span, writer, conf);
                    print_color(bold, writer, conf);
                    writer.print("other definition of `{s}` here:\n", .{self.redefinition.name}) catch unreachable;
                    print_color(not_bold, writer, conf);
                    print_epilude(self.redefinition.first_defined_span, writer, conf);
                }
            },
            .symbol_error => {
                if (self.symbol_error.context_span != null) {
                    print_color(bold, writer, conf);
                    print_note_label(self.symbol_error.context_span.?, writer, conf);
                    print_color(bold, writer, conf);
                    writer.print("{s}\n", .{self.symbol_error.context_message}) catch unreachable;
                    print_color(not_bold, writer, conf);
                    print_epilude(self.symbol_error.context_span.?, writer, conf);
                }
            },
            .duplicate => if (self.duplicate.first != null) {
                print_color(bold, writer, conf);
                print_note_label(self.duplicate.first, writer, conf);
                print_color(bold, writer, conf);
                writer.print("other definition of `{s}` here:\n", .{self.duplicate.identifier}) catch unreachable;
                print_color(not_bold, writer, conf);
                print_epilude(self.duplicate.first, writer, conf);
            },
            .non_exhaustive_sum => {
                for (self.non_exhaustive_sum.forgotten.items) |_type| {
                    print_color(bold, writer, conf);
                    print_note_label(_type.token().span, writer, conf);
                    print_color(bold, writer, conf);
                    writer.print("term not handled: `{s}`\n", .{_type.annotation.pattern.token().data}) catch unreachable;
                    print_color(not_bold, writer, conf);
                    print_epilude(_type.token().span, writer, conf);
                }
            },
            .reimpl => {
                if (self.reimpl.first_defined_span.line_number != 0) { // Don't print reimpls for places that don't exist!
                    print_color(bold, writer, conf);
                    print_note_label(self.reimpl.first_defined_span, writer, conf);
                    print_color(bold, writer, conf);
                    if (self.reimpl.name != null) {
                        writer.print("other implementation of `{s}` here:\n", .{self.reimpl.name.?}) catch unreachable;
                    } else {
                        writer.print("other implementation here:\n", .{}) catch unreachable;
                    }
                    print_color(not_bold, writer, conf);
                    print_epilude(self.reimpl.first_defined_span, writer, conf);
                }
            },
            .method_not_in_impl => {
                print_color(bold, writer, conf);
                print_note_label(self.method_not_in_impl.method_span, writer, conf);
                print_color(bold, writer, conf);
                writer.print("method specification here:\n", .{}) catch unreachable;
                print_color(not_bold, writer, conf);
                print_epilude(self.method_not_in_impl.method_span, writer, conf);
            },
            else => {},
        }
    }

    fn peek_error(err: Error) void {
        var writer = std.fs.File.stderr().writer(&.{});
        const writer_intfc = &writer.interface;
        err.print_error(writer_intfc, .{});
        unreachable;
    }
};

// This is for compatability with Windows, since stdout for Windows isn't known at compile-time
pub fn get_std_err() std.fs.File {
    return std.fs.File.stderr();
}

const bold = term_.Attr{ .bold = true };
const not_bold = term_.Attr{ .bold = false };

pub const Errors = struct {
    errors_list: std.array_list.Managed(Error),

    pub fn init(allocator: std.mem.Allocator) Errors {
        return .{
            .errors_list = std.array_list.Managed(Error).init(allocator),
        };
    }

    pub fn deinit(self: *Errors) void {
        self.errors_list.deinit();
    }

    pub fn add_error(self: *Errors, err: Error) void {
        self.errors_list.append(err) catch unreachable;
        // err.peek_error(); // uncomment if you want to see where errors come from TODO: Make this a cmd line flag
    }

    /// Prints out all errors in the Errors list
    pub fn print_errors(self: *Errors, writer: *std.io.Writer, conf: Error_Config) void {
        for (self.errors_list.items) |err| {
            err.print_error(writer, conf);
        }
    }
};

fn print_note_label(maybe_span: ?Span, writer: *std.io.Writer, conf: Error_Config) void {
    print_label(maybe_span, "note: ", .cyan, writer, conf);
}

fn print_epilude(maybe_span: ?Span, writer: *std.io.Writer, conf: Error_Config) void {
    if (maybe_span) |old_span| {
        const span = old_span;
        if (span.line_number == 0) {
            return;
        } else if (span.line_text.len > 0) {
            print_color(not_bold, writer, conf);
            writer.print("{s}\n", .{span.line_text}) catch unreachable;
        }
        if (span.col > 2) {
            for (2..span.col) |_| {
                writer.print(" ", .{}) catch unreachable;
            }
        }
        if (conf.print_color) {
            term_.outputColor(term_.Attr{ .fg = .green, .bold = true }, "^\n", writer) catch unreachable;
        } else {
            writer.print("^\n", .{}) catch unreachable;
        }
    }
}

fn print_label(maybe_span: ?Span, label: []const u8, color: term_.Color, writer: *std.io.Writer, conf: Error_Config) void {
    if (maybe_span) |span| {
        const filename = if (conf.print_full_path) span.filename else std.fs.path.basename(span.filename);
        if (span.line_number > 0 and span.col > 0) {
            writer.print("{s}:{}:{}: ", .{ filename, span.line_number, span.col }) catch unreachable;
        } else if (filename.len > 0) {
            writer.print("{s}: ", .{filename}) catch unreachable;
        }
    }
    if (conf.print_color) {
        term_.outputColor(term_.Attr{ .fg = color, .bold = true }, label, writer) catch unreachable;
    } else {
        writer.print("{s}", .{label}) catch unreachable;
    }
}

fn print_color(attr: term_.Attr, writer: *std.io.Writer, conf: Error_Config) void {
    if (conf.print_color) {
        attr.dump(writer) catch unreachable;
    }
}
