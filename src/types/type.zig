const std = @import("std");
const alignment_ = @import("../util/alignment.zig");
const AST = @import("../ast/ast.zig").AST;
const prelude_ = @import("../hierarchy/prelude.zig");
const Scope = @import("../symbol/scope.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Symbol = @import("../symbol/symbol.zig");
const Token = @import("../lexer/token.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const unification_ = @import("unification.zig");
const union_fields_ = @import("../util/union_fields.zig");

pub const Type_AST_Common = struct {
    /// Token that defined the type
    _token: Token,

    /// The result of expanding the `_type` field.
    /// Memoized, use `expanded_type()`.
    _expanded_type: ?*Type_AST = null,

    /// What this type was expanded from
    _unexpanded_type: ?*Type_AST = null,

    /// The number of bytes a value in an AST type takes up.
    /// Memoized, use `sizeof()`.
    _size: ?i64 = null,

    /// The alignment for values of this type.
    /// In general it is *NOT* true that size == alignof, especially for products and sums.
    _alignof: ?i64 = null,
};

pub const Type_AST = union(enum) {
    pub const Sum_From = enum {
        optional, // Sum-type comes from an optional constructor (ex: `?T`)
        @"error", // Sum-type comes from an error constructor (ex: `E!T`)
        none, // Regular, plain sum-type
    };

    /// Not generated for correct programs, used to represent incorrect ASTs
    poison: struct { common: Type_AST_Common },
    // Any pointer type (void*), used for receiver's type in trait-method definitions
    anyptr_type: struct { common: Type_AST_Common },
    unit_type: struct { common: Type_AST_Common },
    identifier: struct {
        common: Type_AST_Common,
        _symbol: ?*Symbol = null,
    },
    // TOOD: generic_type
    access: struct {
        common: Type_AST_Common,
        inner_access: *AST,
        _symbol: ?*Symbol = null,
    },
    generic_apply: struct {
        common: Type_AST_Common,
        _lhs: *Type_AST,
        args: std.array_list.Managed(*Type_AST),
        _symbol: ?*Symbol = null,
        state: enum { unmorphed, morphing, morphed } = .unmorphed,
    },
    eq_constraint: struct {
        common: Type_AST_Common,
        _lhs: *Type_AST,
        _rhs: *Type_AST,
    },
    function: struct {
        common: Type_AST_Common,
        args: std.array_list.Managed(*Type_AST),
        _rhs: *Type_AST,
        contexts: std.array_list.Managed(*Type_AST),
        variadic: bool = false,

        pub fn contains_context(self: *@This(), context_type: *Type_AST) bool {
            for (self.contexts.items) |ctx| {
                if (context_type.types_match(ctx.child())) {
                    return true;
                }
            }
            return false;
        }
    },
    annotation: struct {
        common: Type_AST_Common,
        pattern: *AST,
        _child: *Type_AST,
        init: ?*AST,
    },
    enum_type: struct {
        common: Type_AST_Common,
        _terms: std.array_list.Managed(*Type_AST),
        from: Sum_From = .none,
        all_unit: ?bool = null,
        decl: ?*AST = null,

        /// Checks if all terms in the sum are unit-typed
        pub fn is_all_unit(self: *@This()) bool {
            if (self.all_unit) |all_unit| {
                return all_unit;
            }
            var res = true;
            for (self._terms.items) |term| {
                res = res and term.c_types_match(prelude_.unit_type);
            }
            self.all_unit = res;
            return res;
        }

        pub fn get_ctor(self: *@This(), name: []const u8) ?*Type_AST {
            for (self._terms.items) |term| {
                if (std.mem.eql(u8, term.annotation.pattern.token().data, name)) {
                    return term;
                }
            }
            return null;
        }
    },
    untagged_sum_type: struct {
        common: Type_AST_Common,
        _child: *Type_AST,
    },
    context_type: struct {
        common: Type_AST_Common,
        _terms: std.array_list.Managed(*Type_AST),
    },
    struct_type: struct {
        common: Type_AST_Common,
        _terms: std.array_list.Managed(*Type_AST),
        was_slice: bool = false,
        decl: ?*AST = null,

        pub fn get_offset_field(self: *@This(), field_name: []const u8) i64 {
            for (0.., self._terms.items) |i, term| {
                if (term.* == .annotation and std.mem.eql(u8, field_name, term.annotation.pattern.token().data)) {
                    return self.get_offset(i);
                }
            }
            std.debug.panic("compiler error: couldn't get offset; struct didn't have the field `{s}`", .{field_name});
        }

        /// Retrieves the offset in bytes given a field's index
        pub fn get_offset(self: *@This(), field: usize) i64 {
            var offset: i64 = 0;
            for (0..field) |i| {
                var item = self._terms.items[i].expand_identifier();
                if (self._terms.items[i + 1].alignof() == 0) {
                    continue;
                }
                offset += item.sizeof();
                offset = alignment_.next_alignment(offset, self._terms.items[i + 1].alignof());
            }
            return offset;
        }
    },
    tuple_type: struct {
        common: Type_AST_Common,
        _terms: std.array_list.Managed(*Type_AST),

        /// Retrieves the offset in bytes given a field's index
        pub fn get_offset(self: *@This(), field: usize) i64 {
            var offset: i64 = 0;
            for (0..field) |i| {
                var item = self._terms.items[i].expand_identifier();
                if (self._terms.items[i + 1].alignof() == 0) {
                    continue;
                }
                offset += item.sizeof();
                offset = alignment_.next_alignment(offset, self._terms.items[i + 1].alignof());
            }
            return offset;
        }
    },
    addr_of: struct {
        common: Type_AST_Common,
        _child: *Type_AST,
        mut: bool,
        multiptr: bool = false,
        anytptr: bool = false, // When this is true, the addr_of should output as a void*, and should be cast whenever dereferenced
        _scope: ?*Scope = null, // Surrounding scope. Filled in at symbol-tree creation.
    },
    /// A struct-like type of pointers to the vtable, and to the receiver
    dyn_type: struct {
        common: Type_AST_Common,
        _child: *Type_AST,
        mut: bool,
    },
    array_of: struct {
        common: Type_AST_Common,
        _child: *Type_AST,
        len: *AST,

        pub fn get_offset(self: *@This(), field: usize) i64 {
            return self._child.sizeof() * @as(i64, @intCast(field));
        }
    },
    type_of: struct {
        common: Type_AST_Common,
        _expr: *AST,
    },
    /// This node type is needed for pattern matching.
    /// Symbols for captured sum-values in a `match` statement are created before their type information is
    /// known. For example:
    ///
    /// ```rust
    /// match 3 {
    ///   .var1(x, y, z) => // ...
    /// }
    /// ```
    ///
    /// Here, we *need* a type to create the symbols `x`, `y`, and `z`, but we don't have the type
    /// information yet to know which domain-type the variant `var1` takes.
    ///
    /// The domain-of node allows us to specify: "the domain type of this sum value in this sum type"
    /// without knowing it fully yet.
    domain_of: struct {
        common: Type_AST_Common,
        _child: *Type_AST,
        ctor_name: []const u8,
    },
    index: struct {
        common: Type_AST_Common,
        _child: *Type_AST,
        idx: *AST, // TODO: consider just making this an int, since its not available to the user
    },

    pub fn create_poisoned_type(_token: Token, allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .poison = .{ .common = Type_AST_Common{ ._token = _token } } }, allocator);
    }

    pub fn create_anyptr_type(_token: Token, allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .anyptr_type = .{ .common = Type_AST_Common{
            ._token = _token,
        } } }, allocator);
    }

    pub fn create_unit_type(_token: Token, allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .unit_type = .{ .common = Type_AST_Common{ ._token = _token } } }, allocator);
    }

    pub fn create_type_identifier(_token: Token, allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .identifier = .{ .common = Type_AST_Common{ ._token = _token } } }, allocator);
    }

    pub fn create_type_access(_token: Token, inner_access: *AST, allocator: std.mem.Allocator) *Type_AST {
        const _common: Type_AST_Common = .{ ._token = _token };
        return Type_AST.box(Type_AST{ .access = .{
            .common = _common,
            .inner_access = inner_access,
        } }, allocator);
    }

    pub fn create_generic_apply_type(_token: Token, _lhs: *Type_AST, args: std.array_list.Managed(*Type_AST), allocator: std.mem.Allocator) *Type_AST {
        const _common: Type_AST_Common = .{ ._token = _token };
        return Type_AST.box(Type_AST{ .generic_apply = .{
            .common = _common,
            ._lhs = _lhs,
            .args = args,
        } }, allocator);
    }

    pub fn create_eq_constraint(_token: Token, _lhs: *Type_AST, _rhs: *Type_AST, allocator: std.mem.Allocator) *Type_AST {
        const _common: Type_AST_Common = .{ ._token = _token };
        return Type_AST.box(Type_AST{ .eq_constraint = .{
            .common = _common,
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_enum_type(_token: Token, terms: std.array_list.Managed(*Type_AST), allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .enum_type = .{
            .common = Type_AST_Common{ ._token = _token },
            ._terms = terms,
        } }, allocator);
    }

    pub fn create_untagged_sum_type(_token: Token, _child: *Type_AST, allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .untagged_sum_type = .{
            .common = Type_AST_Common{ ._token = _token },
            ._child = _child,
        } }, allocator);
    }

    pub fn create_function(_token: Token, args: std.array_list.Managed(*Type_AST), _rhs: *Type_AST, contexts: std.array_list.Managed(*Type_AST), allocator: std.mem.Allocator) *Type_AST {
        const _common: Type_AST_Common = .{ ._token = _token };
        return Type_AST.box(Type_AST{ .function = .{
            .common = _common,
            .args = args,
            ._rhs = _rhs,
            .contexts = contexts,
        } }, allocator);
    }

    pub fn create_annotation(
        _token: Token,
        pattern: *AST,
        _type: *Type_AST,
        init: ?*AST,
        allocator: std.mem.Allocator,
    ) *Type_AST {
        const _common: Type_AST_Common = .{ ._token = _token };
        return Type_AST.box(
            Type_AST{ .annotation = .{
                .common = _common,
                .pattern = pattern,
                ._child = _type,
                .init = init,
            } },
            allocator,
        );
    }

    pub fn create_context_type(_token: Token, terms: std.array_list.Managed(*Type_AST), allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .context_type = .{
            .common = Type_AST_Common{ ._token = _token },
            ._terms = terms,
        } }, allocator);
    }

    pub fn create_struct_type(_token: Token, terms: std.array_list.Managed(*Type_AST), allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .struct_type = .{
            .common = Type_AST_Common{ ._token = _token },
            ._terms = terms,
        } }, allocator);
    }

    pub fn create_tuple_type(_token: Token, terms: std.array_list.Managed(*Type_AST), allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .tuple_type = .{
            .common = Type_AST_Common{ ._token = _token },
            ._terms = terms,
        } }, allocator);
    }

    pub fn create_addr_of_type(_token: Token, _child: *Type_AST, _mut: bool, multiptr: bool, allocator: std.mem.Allocator) *Type_AST {
        const _common: Type_AST_Common = .{ ._token = _token };
        return Type_AST.box(Type_AST{ .addr_of = .{
            .common = _common,
            ._child = _child,
            .mut = _mut,
            .multiptr = multiptr,
        } }, allocator);
    }

    pub fn create_dyn_type(_token: Token, _child: *Type_AST, _mut: bool, allocator: std.mem.Allocator) *Type_AST {
        const _common: Type_AST_Common = .{ ._token = _token };
        return Type_AST.box(Type_AST{ .dyn_type = .{
            .common = _common,
            ._child = _child,
            .mut = _mut,
        } }, allocator);
    }

    pub fn create_array_of(_token: Token, _child: *Type_AST, len: *AST, allocator: std.mem.Allocator) *Type_AST {
        const _common: Type_AST_Common = .{ ._token = _token };
        return Type_AST.box(Type_AST{ .array_of = .{
            .common = _common,
            ._child = _child,
            .len = len,
        } }, allocator);
    }

    pub fn create_type_of(_token: Token, _expr: *AST, allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .type_of = .{
            .common = Type_AST_Common{ ._token = _token },
            ._expr = _expr,
        } }, allocator);
    }

    pub fn create_domain_of(_token: Token, sum_type: *Type_AST, ctor_name: []const u8, allocator: std.mem.Allocator) *Type_AST {
        const _common: Type_AST_Common = .{ ._token = _token };
        return Type_AST.box(Type_AST{ .domain_of = .{
            .common = _common,
            ._child = sum_type,
            .ctor_name = ctor_name,
        } }, allocator);
    }

    pub fn create_index_type(_token: Token, _lhs: *Type_AST, _rhs: *AST, allocator: std.mem.Allocator) *Type_AST {
        return Type_AST.box(Type_AST{ .index = .{
            .common = Type_AST_Common{ ._token = _token },
            ._child = _lhs,
            .idx = _rhs,
        } }, allocator);
    }

    fn box(ast: Type_AST, alloc: std.mem.Allocator) *Type_AST {
        const retval = alloc.create(Type_AST) catch unreachable;
        retval.* = ast;
        return retval;
    }

    pub fn create_slice_type(of: *Type_AST, _mut: bool, allocator: std.mem.Allocator) *Type_AST {
        var term_types = std.array_list.Managed(*Type_AST).init(allocator);
        const data_type = create_addr_of_type(
            of.token(),
            of,
            _mut,
            true,
            allocator,
        );
        const annot_type = create_annotation(
            of.token(),
            AST.create_identifier(Token.init("data", null, "", "", 0, 0), allocator),
            data_type,
            null,
            allocator,
        );
        term_types.append(annot_type) catch unreachable;
        term_types.append(create_annotation(
            of.token(),
            AST.create_identifier(Token.init("length", null, "", "", 0, 0), allocator),
            prelude_.int_type,
            null,
            allocator,
        )) catch unreachable;
        var retval = create_struct_type(of.token(), term_types, allocator);
        retval.struct_type.was_slice = true;
        return retval;
    }

    pub fn create_optional_type(of_type: *Type_AST, allocator: std.mem.Allocator) *Type_AST {
        var term_types = std.array_list.Managed(*Type_AST).init(allocator);

        const some_type = create_annotation(
            of_type.token(),
            AST.create_identifier(Token.init("some", null, "", "", 0, 0), allocator),
            of_type,
            null,
            allocator,
        );
        term_types.append(some_type) catch unreachable;

        const none_type = create_annotation(
            of_type.token(),
            AST.create_identifier(Token.init_simple("none"), allocator),
            prelude_.unit_type,
            AST.create_unit_value(Token.init_simple("none init"), allocator),
            allocator,
        );
        term_types.append(none_type) catch unreachable;

        var retval = create_enum_type(of_type.token(), term_types, allocator);
        retval.enum_type.from = .optional;
        return retval;
    }

    // Err!Ok => (ok:Ok | err:Err)
    // This is done so that `ok` has an invariant tag of `0`, and errors have a non-zero tag.
    pub fn create_error_type(err_type: *Type_AST, ok_type: *Type_AST, allocator: std.mem.Allocator) *Type_AST {
        var retval_sum_terms = std.array_list.Managed(*Type_AST).init(allocator);

        const ok_annot = create_annotation(
            ok_type.token(),
            AST.create_identifier(Token.init_simple("ok"), allocator),
            ok_type,
            null,
            allocator,
        );
        retval_sum_terms.append(ok_annot) catch unreachable;

        const err_annot = create_annotation(
            err_type.token(),
            AST.create_identifier(Token.init_simple("err"), allocator),
            err_type,
            null,
            allocator,
        );
        retval_sum_terms.append(err_annot) catch unreachable;

        var retval = create_enum_type(ok_type.token(), retval_sum_terms, allocator);
        retval.enum_type.from = .@"error";

        return retval;
    }

    pub fn from_ast(ast: *AST, allocator: std.mem.Allocator) *Type_AST {
        return switch (ast.*) {
            .identifier => blk: {
                const id = Type_AST.create_type_identifier(ast.token(), allocator);
                id.set_symbol(ast.symbol().?);
                break :blk id;
            },
            .access => blk: {
                const id = Type_AST.create_type_access(ast.token(), ast, allocator);
                id.set_symbol(ast.symbol().?);
                break :blk id;
            },
            .index => blk: {
                const base = from_ast(ast.lhs(), allocator);
                var args = std.array_list.Managed(*Type_AST).init(allocator);
                for (ast.children().items) |arg| {
                    args.append(from_ast(arg, allocator)) catch unreachable;
                }
                break :blk Type_AST.create_generic_apply_type(ast.token(), base, args, allocator);
            },
            .generic_apply => blk: {
                const _lhs = from_ast(ast.lhs(), allocator);
                const gen_apply = Type_AST.create_generic_apply_type(ast.token(), _lhs, ast.generic_apply._children, allocator);
                gen_apply.set_symbol(ast.symbol());
                break :blk gen_apply;
            },
            .unit_value => Type_AST.create_unit_type(ast.token(), allocator),
            else => std.debug.panic("unable to construct type from {t}", .{ast.*}),
        };
    }

    pub fn common(self: *const Type_AST) *const Type_AST_Common {
        return union_fields_.get_field_const_ref(self, "common");
    }

    pub fn set_alignof(self: *Type_AST, _alignof: i64) void {
        union_fields_.get_field_ref(self, "common")._alignof = _alignof;
    }

    pub fn set_sizeof(self: *Type_AST, _sizeof: i64) void {
        union_fields_.get_field_ref(self, "common")._size = _sizeof;
    }

    pub fn set_expanded_type(self: *Type_AST, _expanded_type: *Type_AST) void {
        union_fields_.get_field_ref(self, "common")._expanded_type = _expanded_type;
    }

    pub fn set_unexpanded_type(self: *Type_AST, _unexpanded_type: *Type_AST) void {
        union_fields_.get_field_ref(self, "common")._unexpanded_type = _unexpanded_type;
    }

    pub fn token(self: *const Type_AST) Token {
        return self.common()._token;
    }

    pub fn expr(self: Type_AST) *AST {
        return union_fields_.get_struct_field(self, "_expr");
    }

    pub fn child(self: Type_AST) *Type_AST {
        return union_fields_.get_struct_field(self, "_child");
    }

    pub fn mut(self: Type_AST) bool {
        return union_fields_.get_struct_field(self, "mut");
    }

    pub fn set_mut(self: *Type_AST, val: bool) void {
        union_fields_.set_field(self, "mut", val);
    }

    pub fn symbol(self: Type_AST) ?*Symbol {
        if (self == .access) {
            return self.access.inner_access.symbol();
        }
        return union_fields_.get_struct_field(self, "_symbol");
    }

    pub fn set_symbol(self: *Type_AST, val: ?*Symbol) void {
        if (self.* == .access) {
            self.access.inner_access.set_symbol(val);
            return;
        }
        union_fields_.set_field(self, "_symbol", val);
    }

    pub fn lhs(self: Type_AST) *Type_AST {
        return union_fields_.get_struct_field(self, "_lhs");
    }

    pub fn set_lhs(self: *Type_AST, val: *Type_AST) void {
        union_fields_.set_field(self, "_lhs", val);
    }

    pub fn rhs(self: Type_AST) *Type_AST {
        return union_fields_.get_struct_field(self, "_rhs");
    }

    pub fn set_rhs(self: *Type_AST, val: *Type_AST) void {
        union_fields_.set_field(self, "_rhs", val);
    }

    pub fn children(self: *const Type_AST) *const std.array_list.Managed(*Type_AST) {
        return switch (self.*) {
            .function => &self.function.args,
            .generic_apply => &self.generic_apply.args,
            .enum_type => &self.enum_type._terms,
            .untagged_sum_type => self.child().expand_identifier().children(),
            .context_type => &self.context_type._terms,
            .struct_type => &self.struct_type._terms,
            .tuple_type => &self.tuple_type._terms,
            else => std.debug.panic("compiler error: cannot call `.children()` on the Type_AST `{f}`", .{self}),
        };
    }

    pub fn set_children(self: *Type_AST, val: std.array_list.Managed(*Type_AST)) void {
        switch (self.*) {
            .enum_type => self.enum_type._terms = val,
            .untagged_sum_type => self.untagged_sum_type._child.enum_type._terms = val,
            .struct_type => self.struct_type._terms = val,
            .tuple_type => self.tuple_type._terms = val,
            else => std.debug.panic("compiler error: cannot call `.set_children()` on the Type_AST `{s}`", .{@tagName(self.*)}),
        }
    }

    /// Retrieves either the `ok` or `some` type from either an optional type or an error type
    pub fn get_nominal_type(opt_or_error_sum: *Type_AST) *Type_AST {
        std.debug.assert(opt_or_error_sum.enum_type.from == .optional or opt_or_error_sum.enum_type.from == .@"error");
        return opt_or_error_sum.children().items[0];
    }

    pub fn get_some_type(opt_sum: *const Type_AST) *Type_AST {
        std.debug.assert(opt_sum.enum_type.from == .optional);
        return opt_sum.children().items[0];
    }

    pub fn get_none_type(opt_sum: *Type_AST) *Type_AST {
        std.debug.assert(opt_sum.enum_type.from == .optional);
        return opt_sum.children().items[1];
    }

    pub fn get_ok_type(err_union: *const Type_AST) *Type_AST {
        std.debug.assert(err_union.enum_type.from == .@"error");
        return err_union.children().items[0];
    }

    pub fn get_err_type(err_union: *const Type_AST) *Type_AST {
        std.debug.assert(err_union.enum_type.from == .@"error");
        return err_union.children().items[1];
    }

    pub fn get_pos(self: *Type_AST, field_name: []const u8) ?usize {
        for (self.children().items, 0..) |term, i| {
            if (std.mem.eql(u8, term.annotation.pattern.token().data, field_name)) {
                return i;
            }
        }
        return null;
    }

    /// Expands an ast one level if it is an identifier
    pub fn expand_identifier(self: *Type_AST) *Type_AST {
        var res = self;
        while (true) {
            if ((res.* == .identifier or res.* == .access or res.* == .generic_apply) and res.symbol() != null and res.symbol().?.init_typedef() != null) {
                const new = res.symbol().?.init_typedef().?;
                new.set_unexpanded_type(res);
                res = new;
            } else if (res.* == .annotation) {
                res = res.child();
            } else {
                return res;
            }
        }
    }

    pub fn unexpand_type(self: *Type_AST) *Type_AST {
        if (self.common()._unexpanded_type) |unexpanded| {
            return unexpanded.unexpand_type();
        } else {
            return self;
        }
    }

    pub fn print_type(self: *const Type_AST, out: *std.Io.Writer) !void {
        if (self.common()._unexpanded_type) |unexpanded| {
            if (unexpanded != self) {
                return try unexpanded.print_type(out);
            }
        }

        switch (self.*) {
            .poison => try out.print("???", .{}),
            .anyptr_type => try out.print("anyptr", .{}),
            .unit_type => try out.print("()", .{}),
            .identifier => try out.print("{s}", .{self.token().data}),
            .addr_of => {
                if (self.addr_of.multiptr) {
                    try out.print("[*", .{});
                    if (self.addr_of.mut) {
                        try out.print("mut", .{});
                    }
                    try out.print("]", .{});
                } else {
                    try out.print("&", .{});
                    if (self.addr_of.mut) {
                        try out.print("mut ", .{});
                    }
                }
                try self.child().print_type(out);
            },
            .dyn_type => {
                try out.print("&", .{});
                if (self.dyn_type.mut) {
                    try out.print("mut ", .{});
                }
                try out.print("dyn ", .{});
                try self.child().print_type(out);
            },
            .array_of => {
                try out.print("[{f}]", .{self.array_of.len});
                try self.child().print_type(out);
            },
            .function => {
                if (self.function.args.items.len != 1) try out.print("(", .{});
                for (self.children().items, 0..) |term, i| {
                    try term.print_type(out);
                    if (i + 1 < self.children().items.len) {
                        try out.print(", ", .{});
                    }
                }
                if (self.function.args.items.len != 1) try out.print(")", .{});
                try out.print("->", .{});
                try self.rhs().print_type(out);
                if (self.function.contexts.items.len > 0) {
                    try out.print(" with ", .{});
                    for (self.function.contexts.items) |ctx| {
                        try ctx.print_type(out);
                    }
                }
            },
            .struct_type, .tuple_type, .context_type => if (self.* == .struct_type and self.struct_type.was_slice) {
                const data_ptr = self.children().items[0].child();
                if (data_ptr.addr_of.mut) {
                    try out.print("[mut]", .{});
                } else {
                    try out.print("[]", .{});
                }
                try data_ptr.child().print_type(out);
            } else {
                try out.print("(", .{});
                for (self.children().items, 0..) |term, i| {
                    try term.print_type(out);
                    if (i + 1 < self.children().items.len) {
                        try out.print(", ", .{});
                    }
                }
                try out.print(")", .{});
            },

            .enum_type => if (self.enum_type.from == .optional) {
                try out.print("?", .{});
                try self.get_some_type().child().print_type(out);
            } else if (self.enum_type.from == .@"error") {
                try self.get_err_type().child().print_type(out);
                try out.print("!", .{});
                try self.get_ok_type().child().print_type(out);
            } else {
                try out.print("(", .{});
                for (self.enum_type._terms.items, 0..) |term, i| {
                    try term.print_type(out);
                    if (self.enum_type._terms.items.len == 1 or i + 1 != self.enum_type._terms.items.len) {
                        try out.print(" | ", .{});
                    }
                }
                try out.print(")", .{});
            },
            .access => {
                try out.print("{f}::{s}", .{ self.access.inner_access.lhs(), self.access.inner_access.rhs().token().data });
            },
            .generic_apply => {
                try out.print("{f}[{f}", .{ self.generic_apply._lhs, self.generic_apply.args.items[0] });
                for (self.generic_apply.args.items[1..]) |arg| {
                    try out.print(", {f}", .{arg});
                }
                try out.print("]", .{});
            },
            .annotation => {
                try out.print("{s}: ", .{self.annotation.pattern.token().data});
                try self.child().print_type(out);
            },
            .type_of => {
                try out.print("@typeof({f})", .{self.type_of._expr});
            },
            .domain_of => {
                try out.print("@domainof({f}.{s})", .{ self.domain_of._child, self.domain_of.ctor_name });
            },
            .index => try out.print("{f}[{f}]", .{ self.child(), self.index.idx }),
            else => try out.print("{s}", .{@tagName(self.*)}),
        }
    }

    pub fn format(self: *const Type_AST, writer: *std.io.Writer) !void {
        self.print_type(writer) catch unreachable;
    }

    /// Retrieves the size in bytes of an AST node.
    pub fn sizeof(self: *Type_AST) i64 {
        if (self.common()._size == null) {
            self.set_sizeof(self.expand_identifier().sizeof_internal()); // memoize call
        }

        return self.common()._size.?;
    }

    /// Non-memoized slow-path for calculating the size of an AST type in bytes.
    fn sizeof_internal(self: *Type_AST) i64 {
        switch (self.*) {
            .identifier => if (self.symbol() != null and self.symbol().?.init_typedef() != null) {
                return self.symbol().?.init_typedef().?.sizeof();
            } else {
                const primitive_info = prelude_.info_from_name(self.token().data) orelse return 4; // gotta return something... TODO: Sized trait
                return primitive_info.size;
            },

            .struct_type, .tuple_type, .context_type => {
                var total_size: i64 = 0;
                for (self.children().items) |_child| {
                    total_size = alignment_.next_alignment(total_size, _child.alignof());
                    total_size += _child.sizeof();
                }
                return total_size;
            },

            .array_of => {
                const child_size = self.child().sizeof();
                return child_size * @as(i64, @intCast(self.array_of.len.int.data));
            },

            .enum_type => {
                var max_size: i64 = 0;
                for (self.children().items) |_child| {
                    max_size = @max(max_size, _child.sizeof());
                }
                return alignment_.next_alignment(max_size, 8) + 8;
            },
            .untagged_sum_type => {
                std.debug.assert(self.child().expand_identifier().* == .enum_type); // TOOD: validate...
                var max_size: i64 = 0;
                for (self.children().items) |_child| {
                    max_size = @max(max_size, _child.sizeof());
                }
                return max_size; // Same as the sum type, but without the tag and without the padding for the tag
            },

            .function, .addr_of, .dyn_type, .anyptr_type => return 8,

            .unit_type => return 0,

            .annotation => return self.child().sizeof(),

            else => std.debug.panic("compiler error: unimplemented sizeof() for {f}", .{self}),
        }
    }

    /// Calculates the alignment of an AST type in bytes. Call is memoized.
    /// Alignments are gauranteed to be positive.
    pub fn alignof(self: *Type_AST) i64 {
        if (self.common()._alignof == null) {
            self.set_alignof(self.expand_identifier().alignof_internal()); // memoize call
        }

        return self.common()._alignof.?;
    }

    /// Non-memoized slow-path of alignment calculation.
    fn alignof_internal(self: *Type_AST) i64 {
        switch (self.*) {
            .identifier => if (self.symbol() != null and self.symbol().?.init_typedef() != null) {
                return self.symbol().?.init_typedef().?.alignof();
            } else {
                const primitive_info = prelude_.info_from_name(self.token().data) orelse return 4; // gotta return something... TODO: Sized trait
                return primitive_info._align;
            },

            .struct_type, .tuple_type, .context_type => {
                var max_align: i64 = 0;
                for (self.children().items) |_child| {
                    max_align = @max(max_align, _child.alignof());
                }
                return @max(1, max_align);
            },

            .enum_type, // this pains me :-( but has to be this way for the tag // TODO: This is fixable...
            .function,
            .addr_of,
            .dyn_type,
            .anyptr_type,
            => return 8,

            .untagged_sum_type => {
                std.debug.assert(self.child().expand_identifier().* == .enum_type);
                var max_align: i64 = 0;
                for (self.children().items) |_child| {
                    max_align = @max(max_align, _child.alignof());
                }
                return @max(1, max_align);
            },

            .unit_type => return 1, // fogedda bout it

            .annotation, .array_of => return self.child().alignof(),

            else => std.debug.panic("compiler error: unimplemented alignof for {s}", .{@tagName(self.*)}),
        }
    }

    /// For two types `A` and `B`, we say `A <: B` iff for every value `a` belonging to the type `A`, `a` belongs to the
    /// type `B`.
    ///
    /// Another way to view this is if `(a: A) <: (b: B)`, then the assignment `b = a` is permissible, since `a: B`.
    ///
    /// Since the type `Void` has no values, the following are always true for any type T:
    /// - `Void <: T`
    /// - `let x: Void = y` is never type sound (there is no possible value `y` can be)
    /// - `let x: T = void_typed_expression` is always type-sound.
    /// - `types_match(prelude_.void_type, T)` is always true
    ///
    /// Thus, we have the following type map:
    /// ```txt
    ///     Bool  Byte  Word16  Float  Int  String  ...
    ///       \     \     \      |     /      /     /
    ///                        Void
    /// ```
    ///
    /// Also, `&mut T <: &T`, because for every `t: &mut T`, `t: &T`. Thus, `let x: &T = mut_expression` is
    /// always type-sound.
    ///
    /// Also, (x: T,) == T == (x: T,)
    ///
    /// Assumes ASTs structurally can refer to compile-time constant types.
    pub fn types_match(A: *Type_AST, B: *Type_AST) bool {
        // FIXME: High Cyclo
        // std.debug.print("{t} == {t}\n", .{ A.*, B.* });
        // std.debug.print("{f} == {f}\n\n", .{ A, B });
        if (A.* == .annotation) {
            return types_match(A.child(), B);
        } else if (B.* == .annotation) {
            return types_match(A, B.child());
        }
        if (A.* == .access) {
            if (A.symbol().?.decl.?.* == .type_param_decl) {
                return B.satisfies_all_constraints(A.symbol().?.decl.?.type_param_decl.constraints.items) == null;
            }
            return types_match(A.symbol().?.init_typedef().?, B);
        } else if (B.* == .access) {
            if (B.symbol().?.decl.?.* == .type_param_decl) {
                return A.satisfies_all_constraints(B.symbol().?.decl.?.type_param_decl.constraints.items) == null;
            }
            return types_match(A, B.symbol().?.init_typedef().?);
        }
        if (A.* == .generic_apply and A.generic_apply._symbol != null and B.* != .generic_apply) {
            return types_match(A.generic_apply._symbol.?.init_typedef().?, B);
        } else if (B.* == .generic_apply and B.generic_apply._symbol != null and A.* != .generic_apply) {
            return types_match(A, B.generic_apply._symbol.?.init_typedef().?);
        }
        if (B.* == .anyptr_type and (A.* == .addr_of or A.* == .dyn_type)) {
            return true;
        }
        if (B.* == .identifier and B.symbol().?.decl.?.* == .type_param_decl) {
            return B.symbol().?.decl.?.type_param_decl.constraints.items.len > 0 or (A.* == .identifier and A.symbol().?.decl.?.* == .type_param_decl);
        }
        if (A.* == .identifier and A.symbol().?.is_alias() and A != A.expand_identifier()) {
            // If A is a type alias, expand
            // std.debug.print("{f} => {f}\n", .{ A, A.expand_identifier() });
            return types_match(A.expand_identifier(), B);
        } else if (B.* == .identifier and B.symbol().?.is_alias() and B != B.expand_identifier()) {
            // std.debug.print("{f} => {f}\n", .{ B, B.expand_identifier() });
            // If B is a type alias, expand
            return types_match(A, B.expand_identifier());
        }
        if (A.* == .identifier and B.* != .identifier and A != A.expand_identifier()) {
            // If only A is an identifier, and A isn't an atom type, dive
            return types_match(A.expand_identifier(), B);
        } else if (A.* != .identifier and B.* == .identifier and B != B.expand_identifier()) {
            // If only B is an identifier, and B isn't an atom type, dive
            return types_match(A, B.expand_identifier());
        }
        if (A.* == .poison or B.* == .poison) {
            return true; // Whatever
        }
        if (A.is_ident_type("Void")) {
            return true; // Bottom type - vacuously true
        }
        // std.debug.assert(A.common().validation_state == .valid);
        // std.debug.assert(B.common().validation_state == .valid);

        if (@intFromEnum(A.*) != @intFromEnum(B.*)) {
            return false;
        }

        switch (A.*) {
            .identifier => {
                // std.debug.print("{s} == {s}\n", .{ A.symbol().?.name, B.symbol().?.name });
                return A.symbol().? == B.symbol().?;
            },
            .addr_of => return (!B.addr_of.mut or B.addr_of.mut == A.addr_of.mut) and (B.addr_of.multiptr == A.addr_of.multiptr) and types_match(A.child(), B.child()),
            .array_of => return types_match(A.child(), B.child()) and A.array_of.len.int.data == B.array_of.len.int.data,
            .anyptr_type => return B.* == .anyptr_type,
            .unit_type => return true,
            .struct_type, .tuple_type, .context_type => {
                if (!lengths_match(A.children(), B.children())) return false;
                var retval = true;
                for (A.children().items, B.children().items) |term, B_term| {
                    if (term.* == .annotation and B_term.* == .annotation and term.annotation.pattern.* == .identifier and B_term.annotation.pattern.* == .identifier) {
                        if (!std.mem.eql(u8, term.annotation.pattern.token().data, B_term.annotation.pattern.token().data)) {
                            return false;
                        }
                    }
                    retval = retval and term.types_match(B_term);
                }
                return retval;
            },
            .generic_apply => {
                if (!lengths_match(A.children(), B.children())) return false;
                var retval = A.lhs().symbol() == B.lhs().symbol();
                for (A.children().items, B.children().items) |term, B_term| {
                    retval = retval and term.types_match(B_term);
                }
                return retval;
            },
            .enum_type, .untagged_sum_type => {
                if (!lengths_match(A.children(), B.children())) return false;
                var retval = true;
                // need to make sure the types and variant names match
                for (A.children().items, B.children().items) |term, B_term| {
                    const this_name = term.annotation.pattern.token().data;
                    const B_name = B_term.annotation.pattern.token().data;
                    retval = retval and std.mem.eql(u8, this_name, B_name) and term.types_match(B_term);
                }
                return retval;
            },
            .function => {
                if (!lengths_match(A.children(), B.children())) return false;
                var retval = true;
                for (A.children().items, B.children().items) |term, other_term| {
                    retval = retval and term.types_match(other_term);
                }
                return retval and A.rhs().types_match(B.rhs());
            },
            .dyn_type => {
                return (!B.dyn_type.mut or B.dyn_type.mut == A.dyn_type.mut) and (A.child().symbol() == B.child().symbol());
            },
            else => std.debug.panic("compiler error: unimplemented types_match() for {s}", .{@tagName(A.*)}),
        }
    }

    fn lengths_match(as: *const std.array_list.Managed(*Type_AST), bs: *const std.array_list.Managed(*Type_AST)) bool {
        return bs.items.len == as.items.len;
    }

    /// Returns whether the term A is convertible to B
    pub fn convertible_to(from: *Type_AST, to: *Type_AST) bool {
        const from_expanded = from.expand_identifier();
        const to_expanded = to.expand_identifier();

        const from_info = prelude_.info_from_ast(from_expanded);
        const to_info = prelude_.info_from_ast(to_expanded);

        if (from_info) |_from_info| {
            if (_from_info.type_class == .int and to_expanded.* == .addr_of and to_expanded.addr_of.multiptr) {
                return true;
            } else if (to_info) |_to_info| {
                if (_from_info.type_class == _to_info.type_class) {
                    return true;
                }
            }
        }

        if (@intFromEnum(from_expanded.*) != @intFromEnum(to_expanded.*)) {
            return false;
        }

        return switch (from_expanded.*) {
            .addr_of => from_expanded.addr_of.multiptr and to_expanded.addr_of.multiptr and (!to_expanded.addr_of.mut or from_expanded.addr_of.mut),
            .dyn_type => is_sub_trait(from.child(), to.child()) and (!to_expanded.dyn_type.mut or from_expanded.dyn_type.mut),
            else => false,
        };
    }

    pub fn satisfies_all_constraints(self: *Type_AST, constraints: []const *Type_AST) ?*Symbol {
        for (constraints) |constraint| {
            if (constraint.symbol() != null) {
                const trait = constraint.symbol().?;
                const res = constraint.symbol().?.scope.impl_trait_lookup(self, trait);
                if (res.count == 0) {
                    return trait;
                }
            }
        }
        return null;
    }

    fn is_sub_trait(maybe_sub: *Type_AST, maybe_super: *Type_AST) bool {
        const maybe_sub_symbol = maybe_sub.symbol().?;
        const maybe_super_symbol = maybe_super.symbol().?;
        if (maybe_sub_symbol == maybe_super_symbol) return true;
        for (maybe_sub.symbol().?.decl.?.trait.super_traits.items) |super_trait| {
            if (is_sub_trait(super_trait, maybe_super)) return true;
        }
        return false;
    }

    /// Checks whether two AST types would generate to the same C type.
    pub fn c_types_match(self: *Type_AST, other: *Type_AST) bool {
        // FIXME: High Cyclo
        // std.debug.print("{t} == {t}\n", .{ self.*, other.* });
        // std.debug.print("{f} == {f}\n\n", .{ self, other });
        if (self.* == .annotation) {
            return c_types_match(self.child(), other);
        } else if (other.* == .annotation) {
            return c_types_match(self, other.child());
        }
        if (self.* == .access) {
            return c_types_match(self.symbol().?.init_typedef().?, other);
        } else if (other.* == .access) {
            return c_types_match(self, other.symbol().?.init_typedef().?);
        }
        if (other.* == .anyptr_type and self.* == .addr_of) {
            return true;
        }
        // std.debug.assert(self.common().validation_state == .valid);
        // std.debug.assert(other.common().validation_state == .valid);

        if (@intFromEnum(self.*) != @intFromEnum(other.*)) {
            return false;
        }

        switch (self.*) {
            .identifier => return std.mem.eql(u8, self.token().data, other.token().data),
            .addr_of => return c_types_match(self.child(), other.child()),
            .unit_type => return other.* == .unit_type,
            .anyptr_type => return other.* == .anyptr_type,
            .dyn_type => return self.child().symbol() == other.child().symbol(),
            .struct_type, .tuple_type, .enum_type, .untagged_sum_type, .context_type => {
                if (other.children().items.len != self.children().items.len) {
                    return false;
                }
                var retval = true;
                for (self.children().items, other.children().items) |term, other_term| {
                    retval = retval and term.c_types_match(other_term);
                }
                return retval;
            },
            .generic_apply => {
                if (other.children().items.len != self.children().items.len) {
                    return false;
                }
                var retval = self.lhs().symbol() == other.lhs().symbol();
                for (self.children().items, other.children().items) |term, B_term| {
                    retval = retval and term.c_types_match(B_term);
                }
                return retval;
            },
            .function => {
                if (other.children().items.len != self.children().items.len) {
                    return false;
                }
                var retval = true;
                for (self.children().items, other.children().items) |term, other_term| {
                    retval = retval and term.c_types_match(other_term);
                }
                return retval and self.rhs().c_types_match(other.rhs());
            },
            .array_of => return self.child().c_types_match(other.child()) and self.array_of.len.int.data == other.array_of.len.int.data,
            else => std.debug.panic("compiler error: c_types_match(): unimplemented for {s}", .{@tagName(self.*)}),
        }
    }

    pub fn clone(self: *Type_AST, substs: *unification_.Substitutions, allocator: std.mem.Allocator) *Type_AST {
        switch (self.*) {
            .anyptr_type, .dyn_type, .unit_type => return self,
            .identifier => if (substs.get(self.token().data)) |replacement| {
                return replacement;
            } else {
                return self;
            },
            .access => return create_type_access(self.token(), self.access.inner_access.clone(substs, allocator), allocator),
            .type_of => {
                const _expr = self.expr().clone(substs, allocator);
                return create_type_of(self.token(), _expr, allocator);
            },
            .addr_of => {
                const _expr = clone(self.child(), substs, allocator);
                return create_addr_of_type(self.token(), _expr, self.addr_of.mut, self.addr_of.multiptr, allocator);
            },
            .array_of => {
                const _expr = clone(self.child(), substs, allocator);
                return create_array_of(self.token(), _expr, self.array_of.len, allocator);
            },
            .annotation => {
                const _type = clone(self.child(), substs, allocator);
                return create_annotation(self.token(), self.annotation.pattern, _type, self.annotation.init, allocator);
            },
            .function => {
                const args = clone_types(self.function.args, substs, allocator);
                const _rhs = clone(self.rhs(), substs, allocator);
                const contexts = clone_types(self.function.contexts, substs, allocator);
                return create_function(self.token(), args, _rhs, contexts, allocator);
            },
            .enum_type => {
                const new_children = clone_types(self.children().*, substs, allocator);
                var retval = create_enum_type(self.token(), new_children, allocator);
                retval.enum_type.from = self.enum_type.from;
                // NOTE: Do NOT copy over the `all_unit` type, as Self could be unit. Leave it null to be re-evaluated.
                return retval;
            },
            .untagged_sum_type => {
                const _child = clone(self.child(), substs, allocator);
                return create_untagged_sum_type(self.token(), _child, allocator);
            },
            .struct_type => {
                const new_children = clone_types(self.children().*, substs, allocator);
                var retval = create_struct_type(self.token(), new_children, allocator);
                retval.struct_type.was_slice = self.struct_type.was_slice;
                return retval;
            },
            .tuple_type => {
                const new_children = clone_types(self.children().*, substs, allocator);
                return create_tuple_type(self.token(), new_children, allocator);
            },
            .generic_apply => {
                const new_args = clone_types(self.children().*, substs, allocator);
                return create_generic_apply_type(
                    self.token(),
                    self.lhs().clone(substs, allocator),
                    new_args,
                    allocator,
                );
            },
            .eq_constraint => return create_eq_constraint(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            else => std.debug.panic("compiler error: clone doesn't support trait type AST `{s}`", .{@tagName(self.*)}),
        }
    }

    pub fn clone_types(children_terms: std.array_list.Managed(*Type_AST), substs: *unification_.Substitutions, allocator: std.mem.Allocator) std.array_list.Managed(*Type_AST) {
        var retval = std.array_list.Managed(*Type_AST).init(allocator);
        for (children_terms.items) |_child| {
            retval.append(_child.clone(substs, allocator)) catch unreachable;
        }
        return retval;
    }

    pub fn refers_to_self(_type: *Type_AST) bool {
        return switch (_type.*) {
            .anyptr_type, .unit_type, .dyn_type => false,
            .identifier => std.mem.eql(u8, _type.token().data, "Self") or _type.symbol().?.decl.?.* == .type_param_decl,
            .addr_of, .array_of => _type.child().refers_to_self(),
            .annotation => _type.child().refers_to_self(),
            .function => {
                for (_type.function.args.items) |arg| {
                    if (arg.refers_to_self()) {
                        return true;
                    }
                }
                return _type.rhs().refers_to_self();
            },
            .struct_type, .tuple_type, .enum_type => {
                for (_type.children().items) |item| {
                    if (item.refers_to_self()) {
                        return true;
                    }
                }
                return false;
            },
            // I think everything above covers everything, but just in case, error out
            else => true,
        };
    }

    pub fn base_symbol(self: *Type_AST) ?*Symbol {
        return switch (self.*) {
            else => return null,

            .generic_apply => return self.lhs().base_symbol(),

            .identifier, .access => return self.symbol(),
        };
    }

    /// Determines if an expanded AST type can represent a floating-point number value
    pub fn can_expanded_represent_int(self: *Type_AST) bool {
        const info = prelude_.info_from_ast(self) orelse return false;
        return info.type_kind == .signed_integer or info.type_kind == .unsigned_integer;
    }

    /// Determines if an AST type can represent a floating-point number value
    pub fn can_represent_float(self: *Type_AST) bool {
        return can_expanded_represent_float(self.expand_identifier());
    }

    /// Determines if an expanded AST type can represent a floating-point number value
    pub fn can_expanded_represent_float(self: *Type_AST) bool {
        const info = prelude_.info_from_ast(self) orelse return false;
        return info.type_kind == .floating_point;
    }

    /// Determines if an AST type has the operators `==` and `!=` defined
    /// TODO: Replace with trait impl lookup
    pub fn is_eq_type(self: *Type_AST) bool {
        const expanded = self.expand_identifier();
        if (expanded.* == .addr_of) {
            return true;
        } else if (expanded.* == .tuple_type) {
            for (expanded.children().items) |term| {
                if (!term.is_eq_type()) {
                    return false;
                }
            }
            return true;
        } else if (expanded.* == .enum_type) {
            return true;
        }
        const info = prelude_.info_from_ast(expanded) orelse return false;
        return info.type_class == .eq or expanded.is_ord_type();
    }

    /// Determines if an AST type has the operators `<` and `>` defined.
    /// Ord <: Eq
    /// TODO: Replace with trait impl lookup
    pub fn is_ord_type(self: *Type_AST) bool {
        const expanded = self.expand_identifier();
        const info = prelude_.info_from_ast(expanded) orelse return false;
        return info.type_class == .ord or expanded.is_num_type();
    }

    /// Determines if an AST type has the operators `+`, `-`, `/` and `*` defined.
    /// Num <: Ord
    /// TODO: Replace with trait impl lookup
    pub fn is_num_type(self: *Type_AST) bool {
        const expanded = self.expand_identifier();
        const info = prelude_.info_from_ast(expanded) orelse return false;
        return info.type_class == .num or expanded.is_int_type();
    }

    /// Determines if an AST type has the operator `%` defined.
    /// Int <: Num
    /// TODO: Replace with trait impl lookup
    pub fn is_int_type(self: *Type_AST) bool {
        const expanded = self.expand_identifier();
        const info = prelude_.info_from_ast(expanded) orelse return false;
        return info.type_class == .int;
    }

    /// TODO: Replace with trait impl lookup
    pub fn is_bits_type(self: *Type_AST) bool {
        const expanded = self.expand_identifier();
        const info = prelude_.info_from_ast(expanded) orelse return false;
        return info.type_kind == .unsigned_integer;
    }

    pub fn is_c_void_type(self: *Type_AST) bool {
        return prelude_.unit_type.c_types_match(self);
    }

    pub fn is_ident_type(self: *Type_AST, name: []const u8) bool {
        return self.* == .identifier and std.mem.eql(u8, self.token().data, name);
    }

    pub fn is_indirect(self: *const Type_AST) bool {
        return switch (self.*) {
            .addr_of, .dyn_type => true,
            else => false,
        };
    }
};
