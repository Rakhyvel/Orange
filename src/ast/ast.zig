// TODO: Can this file be split up?? At ALL?
// 1. keep AST union in this file
// 2. `pub using namespace @import("type-check.zig");`
// 3. `pub using namespace @import("type-properties.zig");`
// 4. `pub using namespace @import("validate-ast.zig");`
// 5. `pub using namespace @import("type-representation.zig");`, or a better name, for `can_represent_X`, `is_ord`, etc
// 6. `pub using namespace @import("poisoned-ast.zig");

const std = @import("std");
const module_ = @import("../hierarchy/module.zig");
const core_ = @import("../hierarchy/core.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const process_state_ = @import("../util/process_state.zig");
const Token = @import("../lexer/token.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Monomorph_Map = @import("../types/type_map.zig").Monomorph_Map;
const validation_state_ = @import("../util/validation_state.zig");
const unification_ = @import("../types/unification.zig");
const fmt_ = @import("../util/fmt.zig");
const union_fields_ = @import("../util/union_fields.zig");

pub const AST_Validation_State = validation_state_.Validation_State;

pub const Receiver_Kind = enum {
    value, // Receiver is taken by value
    addr_of, // Receiver is taken by immutable address
    mut_addr_of, // Receiver is taken by mutable address

    pub fn to_string(self: @This()) []const u8 {
        return switch (self) {
            .value => "self",
            .addr_of => "&self",
            .mut_addr_of => "&mut self",
        };
    }
};

pub const Sum_From = enum {
    optional, // Sum-type comes from an optional constructor (ex: `?T`)
    @"error", // Sum-type comes from an error constructor (ex: `E!T`)
    none, // Regular, plain sum-type
};

/// Contains common properties of all AST nodes
pub const AST_Common = struct {
    /// Token that defined the AST node in text.
    _token: Token,

    /// The AST's validation status.
    validation_state: AST_Validation_State = .unvalidated,

    ok_for_comptime: bool = false, // HACK: generic functions that return a type have this set to true, passes valid_type check
};

/// Represents an Abstract Syntax Tree.
pub const AST = union(enum) {
    poison: struct { common: AST_Common },
    /// Unit value
    unit_value: struct { common: AST_Common },
    /// Integer constant
    int: struct {
        common: AST_Common,
        data: i128,
    },
    /// Floating-point constant
    float: struct {
        common: AST_Common,
        data: f64,
    },
    /// Character constant
    // char: struct { common: AST_Common },
    /// String constant
    string: struct {
        common: AST_Common,
        data: []const u8,
    },
    /// True constant
    true: struct { common: AST_Common },
    /// False constant
    false: struct { common: AST_Common },
    /// Field
    /// This tag is used for the right-hand-side of selects. They do not refer to symbols.
    field: struct { common: AST_Common },
    /// Identifier
    identifier: struct {
        common: AST_Common,
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null,
    },

    not: struct { common: AST_Common, _expr: *AST },
    negate: struct { common: AST_Common, _expr: *AST },
    dereference: struct { common: AST_Common, _expr: *AST },
    @"try": struct {
        common: AST_Common,
        _expr: *AST,
        _symbol: ?*Symbol, // `try`'s must be in functions. This is the function's symbol.
    },
    @"comptime": struct {
        common: AST_Common,
        _expr: *AST,
        name: ?*AST = null,
        result: ?*AST = null,
        _scope: ?*Scope = null, // Surrounding scope. Filled in at symbol-tree creation. Used to create a comptime symbol
    },

    // Binary operators
    // TODO: We could de-duplicate this, I suppose.
    assign: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    @"or": struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    @"and": struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    add: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    sub: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    mult: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    div: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    mod: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    equal: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    not_equal: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    greater: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    lesser: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    greater_equal: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    lesser_equal: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    @"catch": struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    @"orelse": struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    call: struct {
        common: AST_Common,
        _lhs: *AST,
        _args: std.array_list.Managed(*AST),
        context_args: std.array_list.Managed(*Symbol),
        _scope: ?*Scope = null, // Surrounding scope. Filled in at symbol-tree creation.
    },
    bit_and: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    bit_or: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    bit_xor: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    bit_not: struct { common: AST_Common, _expr: *AST },
    left_shift: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    right_shift: struct { common: AST_Common, _lhs: *AST, _rhs: *AST },
    index: struct { common: AST_Common, _lhs: *AST, _children: std.array_list.Managed(*AST) },
    generic_apply: struct {
        common: AST_Common,
        _lhs: *AST,
        _children: std.array_list.Managed(*Type_AST),
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null,
        state: process_state_.Process_State = .unprocessed,
    },
    select: struct {
        common: AST_Common,
        _lhs: *AST,
        _rhs: *AST,
        _pos: ?usize,
    },
    access: struct {
        common: AST_Common,
        _lhs: *AST,
        _rhs: *AST,
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null, // Surrounding scope. Filled in at symbol-tree creation.
    },
    type_access: struct {
        common: AST_Common,
        _lhs_type: *Type_AST,
        _rhs: *AST,
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null, // Surrounding scope. Filled in at symbol-tree creation.
    },
    trait: struct {
        common: AST_Common,
        name: *AST, //
        _generic_params: std.array_list.Managed(*AST),
        super_traits: std.array_list.Managed(*Type_AST),
        super_trait_impls: std.array_list.Managed(*AST),
        method_decls: std.array_list.Managed(*AST),
        const_decls: std.array_list.Managed(*AST),
        type_decls: std.array_list.Managed(*AST),
        num_virtual_methods: i64 = 0,
        _symbol: ?*Symbol = null, // Filled by symbol-tree pass.
        _scope: ?*Scope = null, // Filled by symbol-tree pass.

        pub fn find_method(self: @This(), name: []const u8) ?*AST {
            for (self.method_decls.items) |decl| {
                if (std.mem.eql(u8, decl.method_decl.name.token().data, name)) {
                    return decl;
                }
            }
            for (self.super_traits.items) |super_trait| {
                const super_trait_decl = super_trait.symbol().?.decl.?;
                if (super_trait_decl.trait.find_method(name)) |res| return res;
            }
            return null;
        }
    },
    impl: struct {
        common: AST_Common,
        trait: ?*AST, // Identifier-like ast which refers to the trait
        _type: *Type_AST, // The `for` type for this impl
        method_defs: std.array_list.Managed(*AST),
        const_defs: std.array_list.Managed(*AST),
        type_defs: std.array_list.Managed(*AST),
        _generic_params: std.array_list.Managed(*AST), // list of annotations
        num_virtual_methods: i64 = 0,
        _scope: ?*Scope = null, // Scope used for `impl` methods, rooted in `impl`'s scope.
        impls_anon_trait: bool = false, // true when this impl implements an anonymous trait
        instantiations: Monomorph_Map(*AST),
    },
    invoke: struct {
        common: AST_Common,
        _lhs: *AST, // The subject of the invoke
        _rhs: *AST, // The method, usually a field AST
        _args: std.array_list.Managed(*AST), // The args of the invoke
        context_args: std.array_list.Managed(*Symbol),
        _scope: ?*Scope = null, // Surrounding scope. Filled in at symbol-tree creation.
        method_decl: ?*AST = null,
        prepended: bool = false,
    },
    /// A struct-like-value of pointers to the vtable, and to the receiver
    dyn_value: struct {
        common: AST_Common,
        dyn_type: *Type_AST, // reference to the type of this value, since it is only created using address-ofs
        _expr: *AST,
        mut: bool,
        impl: ?*AST = null, // The implementation AST, whose vtable should be used
        _scope: ?*Scope = null, // Surrounding scope. Filled in when addr-of is converted
    },
    enum_value: struct {
        common: AST_Common,
        domain: ?*Type_AST = null, // kept in it's annotation form, for compatability with calls
        init: ?*AST = null,
        base: ?*Type_AST = null,
        _pos: ?usize = null,

        pub fn get_name(self: *const @This()) []const u8 {
            return self.common._token.data;
        }
    },
    context_value: struct {
        common: AST_Common,
        parent: *Type_AST,
        _terms: std.array_list.Managed(*AST),
    },
    struct_value: struct {
        common: AST_Common,
        parent: *Type_AST,
        _terms: std.array_list.Managed(*AST),
        was_slice: bool,
    },
    tuple_value: struct {
        common: AST_Common,
        _terms: std.array_list.Managed(*AST),
    },
    array_value: struct {
        common: AST_Common,
        _terms: std.array_list.Managed(*AST),
    },

    // Fancy operators
    as: struct {
        common: AST_Common,
        _expr: *AST,
        _type: *Type_AST,
    },
    addr_of: struct {
        common: AST_Common,
        _expr: *AST,
        mut: bool,
        multiptr: bool = false,
        anytptr: bool = false, // When this is true, the addr_of should output as a void*, and should be cast whenever dereferenced
        _scope: ?*Scope = null, // Surrounding scope. Filled in at symbol-tree creation.
    },
    slice_of: struct {
        common: AST_Common,
        _expr: *AST,
        mut: bool,
    },
    sub_slice: struct { common: AST_Common, super: *AST, lower: ?*AST, upper: ?*AST },
    range: struct { common: AST_Common, lower: *AST, upper: *AST },
    receiver: struct {
        common: AST_Common,
        kind: Receiver_Kind,
        _type: ?*Type_AST = null,
        value_derefd: bool = false,
    },
    default: struct {
        common: AST_Common,
        _type: *Type_AST,
    },
    size_of: struct {
        common: AST_Common,
        _type: *Type_AST,
    },
    /// Print a formatted string to a writer
    write: struct {
        common: AST_Common,
        writer: *AST,
        _children: std.array_list.Managed(*AST),
        _scope: ?*Scope = null,
    },
    /// Print a formatted string to IO.writer
    /// Both `@print` and `@println` use this node, with `@println` having a newline appended
    print: struct {
        common: AST_Common,
        _children: std.array_list.Managed(*AST),
        _scope: ?*Scope = null,
    },
    format_args: struct {
        common: AST_Common,
        _children: std.array_list.Managed(*AST),
        _scope: ?*Scope = null,
    },
    variant_tag: struct {
        common: AST_Common,
        _expr: *AST,
    },
    variant_name: struct {
        common: AST_Common,
        _expr: *AST,
    },

    // Control-flow expressions
    @"if": struct {
        common: AST_Common,
        _scope: ?*Scope,
        let: ?*AST,
        condition: *AST,
        _body_block: *AST,
        _else_block: ?*AST,
    },
    match: struct {
        common: AST_Common,
        _scope: ?*Scope,
        let: ?*AST,
        _expr: *AST,
        _mappings: std.array_list.Managed(*AST),
    },
    mapping: struct {
        common: AST_Common,
        _lhs: *AST,
        _rhs: *AST,
        captures: std.array_list.Managed(*AST),
        _scope: ?*Scope, // Scope used for `match` mappings, rooted in `match`'s scope. Captures, rhs live in this scope
    },
    @"while": struct {
        common: AST_Common,
        _scope: ?*Scope,
        let: ?*AST,
        condition: *AST,
        post: ?*AST,
        _body_block: *AST,
        _else_block: ?*AST,
    },
    @"for": struct {
        common: AST_Common,
        _scope: ?*Scope,
        let: ?*AST,
        elem: *AST,
        into_iter: *AST,
        _body_block: *AST,
        _else_block: ?*AST,
        into_iter_into_iter_method_decl: ?*AST = null,
        into_iter_next_method_decl: ?*AST = null,
    },
    with: struct {
        common: AST_Common,
        contexts: std.array_list.Managed(*AST),
        _body_block: *AST,
        _scope: ?*Scope = null,
    },
    block: struct {
        common: AST_Common,
        _scope: ?*Scope,
        _statements: std.array_list.Managed(*AST),
        defers: std.array_list.Managed(*AST),
        final: ?*AST, // either `return`, `continue`, or `break`
    },

    // Control-flow statements
    @"break": struct { common: AST_Common },
    @"continue": struct { common: AST_Common },
    @"unreachable": struct { common: AST_Common },
    @"return": struct {
        common: AST_Common,
        _ret_expr: ?*AST,
        _symbol: ?*Symbol, // `retrun`'s must be in functions. This is the function's symbol.
    },
    /// This type is a special version of an identifier. It is needed to encapsulate information about a
    /// symbol declaration that is needed before the symbol is even constructed. An identifier AST cannot be
    /// used for this purpose, because it refers to a symbol _after_ symbol decoration.
    pattern_symbol: struct {
        common: AST_Common,
        name: []const u8,
        kind: Symbol.Kind,
        storage: Symbol.Storage,
        _symbol: ?*Symbol = null,
    },
    binding: struct {
        common: AST_Common,
        pattern: *AST, // Pattern of the binding
        type: *Type_AST, // Conceptually, the type for the binding as a whole
        init: ?*AST, // The init value for the binding as a whole. Possible to be null if `undefiend`.
        decls: std.array_list.Managed(*AST),
    },
    decl: struct {
        common: AST_Common,
        name: *AST, // The pattern symbol AST for this decl. Access the symbol through this
        type: *Type_AST,
        init: ?*AST,
    },
    fn_decl: struct {
        common: AST_Common,
        _generic_params: std.array_list.Managed(*AST),
        name: ?*AST, //
        _params: std.array_list.Managed(*AST), // Parameters' decl ASTs
        _param_symbols: std.array_list.Managed(*Symbol), // Parameters' symbols
        context_param_symbols: std.array_list.Managed(*Symbol), // Context parameters' symbols
        ret_type: *Type_AST,
        context_decls: std.array_list.Managed(*AST),
        _decl_type: ?*Type_AST = null,
        refinement: ?*AST,
        init: *AST,
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null,
        infer_error: bool,
    },
    context_value_decl: struct {
        common: AST_Common,
        parent: *Type_AST,
        init: ?*AST,
        _symbol: ?*Symbol = null,
    },
    context_decl: struct {
        common: AST_Common,
        name: *AST,
        fields: std.array_list.Managed(*Type_AST),
        // TODO: generic params?
        _type: *Type_AST,
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null,
    },
    type_param_decl: struct {
        common: AST_Common,
        rigid: bool,
        _symbol: ?*Symbol = null,
        constraints: std.array_list.Managed(*Type_AST),
    },
    struct_decl: struct {
        common: AST_Common,
        name: *AST,
        fields: std.array_list.Managed(*Type_AST),
        _generic_params: std.array_list.Managed(*AST),
        _type: *Type_AST,
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null,
    },
    enum_decl: struct {
        common: AST_Common,
        name: *AST,
        fields: std.array_list.Managed(*Type_AST),
        _generic_params: std.array_list.Managed(*AST),
        _type: *Type_AST,
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null,
    },
    type_alias: struct {
        common: AST_Common,
        name: *AST,
        init: ?*Type_AST,
        _generic_params: std.array_list.Managed(*AST),
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null,
    },
    method_decl: struct {
        common: AST_Common,
        name: *AST,
        is_virtual: bool,
        receiver: ?*AST,
        _params: std.array_list.Managed(*AST), // Parameters' decl ASTs
        _param_symbols: std.array_list.Managed(*Symbol), // Parameters' symbols
        context_param_symbols: std.array_list.Managed(*Symbol), // Context parameters' symbols
        c_type: ?*Type_AST = null,
        ret_type: *Type_AST,
        context_decls: std.array_list.Managed(*AST),
        _decl_type: ?*Type_AST = null,
        refinement: ?*AST,
        init: ?*AST,
        impl: ?*AST = null, // The surrounding `impl`. Null for method_decls in traits.
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null,
    },
    @"test": struct {
        common: AST_Common,
        name: ?*AST,
        context_decls: std.array_list.Managed(*AST),
        init: *AST,
        _decl_type: ?*Type_AST = null,
        context_param_symbols: std.array_list.Managed(*Symbol), // Context parameters' symbols
        _symbol: ?*Symbol = null,
        _scope: ?*Scope = null,
    },
    module: struct {
        common: AST_Common,
        _scope: ?*Scope, // Only null for compatibility. Always present.
        module: *module_.Module,
    },
    import: struct {
        common: AST_Common,
        pattern: *AST,
    },
    cinclude: struct {
        common: AST_Common,
        _expr: *AST,
    },
    @"defer": struct { common: AST_Common, _statement: *AST },
    @"errdefer": struct { common: AST_Common, _statement: *AST },

    pub fn create_poison(_token: Token, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .poison = .{ .common = AST_Common{
            ._token = _token,
        } } }, allocator);
    }

    pub fn create_unit_value(_token: Token, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .unit_value = .{ .common = AST_Common{
            ._token = _token,
        } } }, allocator);
    }

    pub fn create_int(_token: Token, data: i128, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .int = .{
            .common = _common,
            .data = data,
        } }, allocator);
    }

    pub fn create_float(_token: Token, data: f64, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .float = .{
            .common = _common,
            .data = data,
        } }, allocator);
    }

    pub fn create_string(
        _token: Token,
        data: []const u8, // The string's raw utf8 bytes, not containing `"`'s, with escapes escaped
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .string = .{ .common = AST_Common{
            ._token = _token,
        }, .data = data } }, allocator);
    }

    pub fn create_field(_token: Token, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .field = .{ .common = AST_Common{
            ._token = _token,
        } } }, allocator);
    }

    pub fn create_identifier(_token: Token, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .identifier = .{ .common = AST_Common{
            ._token = _token,
        } } }, allocator);
    }

    pub fn create_unreachable(_token: Token, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .@"unreachable" = .{ .common = AST_Common{
            ._token = _token,
        } } }, allocator);
    }

    pub fn create_true(_token: Token, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .true = .{ .common = AST_Common{
            ._token = _token,
        } } }, allocator);
    }

    pub fn create_false(_token: Token, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .false = .{ .common = AST_Common{
            ._token = _token,
        } } }, allocator);
    }

    pub fn create_not(_token: Token, _expr: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .not = .{
            .common = AST_Common{ ._token = _token },
            ._expr = _expr,
        } }, allocator);
    }

    pub fn create_negate(_token: Token, _expr: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .negate = .{
            .common = AST_Common{ ._token = _token },
            ._expr = _expr,
        } }, allocator);
    }

    pub fn create_dereference(_token: Token, _expr: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .dereference = .{
            .common = AST_Common{ ._token = _token },
            ._expr = _expr,
        } }, allocator);
    }

    pub fn create_try(_token: Token, _expr: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .@"try" = .{ .common = _common, ._expr = _expr, ._symbol = null } }, allocator);
    }

    pub fn create_comptime(_token: Token, _expr: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(
            AST{ .@"comptime" = .{ .common = _common, ._expr = _expr, .name = null } },
            allocator,
        );
    }

    pub fn create_default(_token: Token, _expr: *Type_AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .default = .{
            .common = AST_Common{ ._token = _token },
            ._type = _expr,
        } }, allocator);
    }

    pub fn create_size_of(_token: Token, _expr: *Type_AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .size_of = .{
            .common = AST_Common{ ._token = _token },
            ._type = _expr,
        } }, allocator);
    }

    pub fn create_write(_token: Token, writer: *AST, _children: std.array_list.Managed(*AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .write = .{
            .common = AST_Common{ ._token = _token },
            .writer = writer,
            ._children = _children,
        } }, allocator);
    }

    pub fn create_print(_token: Token, _children: std.array_list.Managed(*AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .print = .{
            .common = AST_Common{ ._token = _token },
            ._children = _children,
        } }, allocator);
    }

    pub fn create_format_args(_token: Token, _children: std.array_list.Managed(*AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .format_args = .{
            .common = AST_Common{ ._token = _token },
            ._children = _children,
        } }, allocator);
    }

    pub fn create_variant_tag(_token: Token, _expr: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .variant_tag = .{
            .common = AST_Common{ ._token = _token },
            ._expr = _expr,
        } }, allocator);
    }

    pub fn create_variant_name(_token: Token, _expr: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .variant_name = .{
            .common = AST_Common{ ._token = _token },
            ._expr = _expr,
        } }, allocator);
    }

    pub fn create_assign(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .assign = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_or(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .@"or" = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_and(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .@"and" = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_equal(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .equal = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_not_equal(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .not_equal = .{
            .common = _common,
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_greater(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .greater = .{
            .common = _common,
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_lesser(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .lesser = .{
            .common = _common,
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_greater_equal(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .greater_equal = .{
            .common = _common,
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_lesser_equal(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .lesser_equal = .{
            .common = _common,
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_add(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .add = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_sub(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .sub = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_mult(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .mult = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_div(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .div = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_mod(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .mod = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_catch(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .@"catch" = .{ .common = _common, ._lhs = _lhs, ._rhs = _rhs } }, allocator);
    }

    pub fn create_orelse(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .@"orelse" = .{ .common = _common, ._lhs = _lhs, ._rhs = _rhs } }, allocator);
    }

    pub fn create_call(_token: Token, _lhs: *AST, args: std.array_list.Managed(*AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .call = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._args = args,
            .context_args = std.array_list.Managed(*Symbol).init(allocator),
        } }, allocator);
    }

    pub fn create_bit_and(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .bit_and = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_bit_or(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .bit_or = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_bit_xor(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .bit_xor = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_bit_not(_token: Token, _expr: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .bit_not = .{
            .common = AST_Common{ ._token = _token },
            ._expr = _expr,
        } }, allocator);
    }

    pub fn create_left_shift(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .left_shift = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_right_shift(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .right_shift = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_index(_token: Token, _lhs: *AST, _children: std.array_list.Managed(*AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .index = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._children = _children,
        } }, allocator);
    }

    pub fn create_generic_apply(_token: Token, _lhs: *AST, _children: std.array_list.Managed(*Type_AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .generic_apply = .{
            .common = AST_Common{ ._token = _token },
            ._lhs = _lhs,
            ._children = _children,
        } }, allocator);
    }

    pub fn create_select(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .select = .{
            .common = _common,
            ._lhs = _lhs,
            ._rhs = _rhs,
            ._pos = null,
        } }, allocator);
    }

    pub fn create_access(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .access = .{
            .common = _common,
            ._lhs = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_type_access(_token: Token, _lhs: *Type_AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .type_access = .{
            .common = _common,
            ._lhs_type = _lhs,
            ._rhs = _rhs,
        } }, allocator);
    }

    pub fn create_enum_value(_token: Token, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .enum_value = .{ .common = _common } }, allocator);
    }

    pub fn create_trait(
        _token: Token,
        name: *AST,
        _generic_params: std.array_list.Managed(*AST),
        super_traits: std.array_list.Managed(*Type_AST),
        method_decls: std.array_list.Managed(*AST),
        const_decls: std.array_list.Managed(*AST),
        type_decls: std.array_list.Managed(*AST),
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .trait = .{
            .common = AST_Common{ ._token = _token },
            .name = name,
            ._generic_params = _generic_params,
            .super_traits = super_traits,
            .super_trait_impls = std.array_list.Managed(*AST).init(allocator),
            .method_decls = method_decls,
            .const_decls = const_decls,
            .type_decls = type_decls,
        } }, allocator);
    }

    pub fn create_impl(
        _token: Token,
        _trait: ?*AST,
        _type: *Type_AST,
        method_defs: std.array_list.Managed(*AST),
        const_defs: std.array_list.Managed(*AST),
        type_defs: std.array_list.Managed(*AST),
        _generic_params: std.array_list.Managed(*AST),
        allocator: std.mem.Allocator,
    ) *AST {
        const retval = AST.box(
            AST{ .impl = .{
                .common = AST_Common{ ._token = _token },
                .trait = _trait,
                ._type = _type,
                .method_defs = method_defs,
                .const_defs = const_defs,
                .type_defs = type_defs,
                ._generic_params = _generic_params,
                .instantiations = Monomorph_Map(*AST).init(allocator),
            } },
            allocator,
        );
        for (method_defs.items) |method_def| {
            method_def.method_decl.impl = retval;
        }
        return retval;
    }

    pub fn create_invoke(
        _token: Token,
        _lhs: *AST,
        _rhs: *AST,
        args: std.array_list.Managed(*AST),
        allocator: std.mem.Allocator,
    ) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .invoke = .{
            .common = _common,
            ._lhs = _lhs,
            ._rhs = _rhs,
            ._args = args,
            .context_args = std.array_list.Managed(*Symbol).init(allocator),
        } }, allocator);
    }

    pub fn create_dyn_value(
        _token: Token,
        dyn_type: *Type_AST,
        _expr: *AST,
        _scope: *Scope,
        _mut: bool,
        allocator: std.mem.Allocator,
    ) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .dyn_value = .{
            .common = _common,
            .dyn_type = dyn_type,
            ._expr = _expr,
            ._scope = _scope,
            .mut = _mut,
        } }, allocator);
    }

    pub fn create_context_value(_token: Token, parent: *Type_AST, terms: std.array_list.Managed(*AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .context_value = .{
            .common = AST_Common{ ._token = _token },
            .parent = parent,
            ._terms = terms,
        } }, allocator);
    }

    pub fn create_struct_value(_token: Token, parent: *Type_AST, terms: std.array_list.Managed(*AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .struct_value = .{
            .common = AST_Common{ ._token = _token },
            .parent = parent,
            ._terms = terms,
            .was_slice = false,
        } }, allocator);
    }

    pub fn create_tuple_value(_token: Token, terms: std.array_list.Managed(*AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .tuple_value = .{
            .common = AST_Common{ ._token = _token },
            ._terms = terms,
        } }, allocator);
    }

    pub fn create_array_value(_token: Token, terms: std.array_list.Managed(*AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .array_value = .{
            .common = AST_Common{ ._token = _token },
            ._terms = terms,
        } }, allocator);
    }

    pub fn create_as(_token: Token, _expr: *AST, _type: *Type_AST, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .as = .{
            .common = _common,
            ._expr = _expr,
            ._type = _type,
        } }, allocator);
    }

    pub fn create_addr_of(_token: Token, _expr: *AST, _mut: bool, multiptr: bool, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .addr_of = .{
            .common = _common,
            ._expr = _expr,
            .mut = _mut,
            .multiptr = multiptr,
        } }, allocator);
    }

    pub fn create_slice_of(_token: Token, _expr: *AST, _mut: bool, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .slice_of = .{
            .common = _common,
            ._expr = _expr,
            .mut = _mut,
        } }, allocator);
    }

    pub fn create_sub_slice(
        _token: Token,
        super: *AST,
        lower: ?*AST,
        upper: ?*AST,
        allocator: std.mem.Allocator,
    ) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .sub_slice = .{
            .common = _common,
            .super = super,
            .lower = lower,
            .upper = upper,
        } }, allocator);
    }

    pub fn create_range(
        _token: Token,
        lower: *AST,
        upper: *AST,
        allocator: std.mem.Allocator,
    ) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(AST{ .range = .{
            .common = _common,
            .lower = lower,
            .upper = upper,
        } }, allocator);
    }

    pub fn create_receiver(_token: Token, kind: Receiver_Kind, allocator: std.mem.Allocator) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(
            AST{ .receiver = .{ .common = _common, .kind = kind } },
            allocator,
        );
    }

    pub fn create_if(
        _token: Token,
        let: ?*AST,
        condition: *AST,
        _body_block: *AST,
        _else_block: ?*AST,
        allocator: std.mem.Allocator,
    ) *AST {
        const _common: AST_Common = .{ ._token = _token };
        return AST.box(
            AST{ .@"if" = .{
                .common = _common,
                ._scope = null,
                .let = let,
                .condition = condition,
                ._body_block = _body_block,
                ._else_block = _else_block,
            } },
            allocator,
        );
    }

    pub fn create_match(
        _token: Token,
        let: ?*AST,
        _expr: *AST,
        mappings: std.array_list.Managed(*AST),
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(
            AST{ .match = .{
                .common = AST_Common{ ._token = _token },
                ._scope = null,
                .let = let,
                ._expr = _expr,
                ._mappings = mappings,
            } },
            allocator,
        );
    }

    pub fn create_mapping(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(
            AST{ .mapping = .{
                .common = AST_Common{ ._token = _token },
                ._lhs = _lhs,
                ._rhs = _rhs,
                ._scope = null,
                .captures = std.array_list.Managed(*AST).init(allocator),
            } },
            allocator,
        );
    }

    pub fn create_while(
        _token: Token,
        let: ?*AST,
        condition: *AST,
        post: ?*AST,
        _body_block: *AST,
        _else_block: ?*AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(
            AST{ .@"while" = .{
                .common = AST_Common{ ._token = _token },
                ._scope = null,
                .let = let,
                .condition = condition,
                .post = post,
                ._body_block = _body_block,
                ._else_block = _else_block,
            } },
            allocator,
        );
    }

    pub fn create_for(
        _token: Token,
        let: ?*AST,
        elem: *AST,
        into_iter: *AST,
        _body_block: *AST,
        _else_block: ?*AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(
            AST{ .@"for" = .{
                .common = AST_Common{ ._token = _token },
                ._scope = null,
                .let = let,
                .elem = elem,
                .into_iter = into_iter,
                ._body_block = _body_block,
                ._else_block = _else_block,
            } },
            allocator,
        );
    }

    pub fn create_with(
        _token: Token,
        contexts: std.array_list.Managed(*AST),
        _body_block: *AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(
            AST{ .with = .{
                .common = AST_Common{ ._token = _token },
                .contexts = contexts,
                ._body_block = _body_block,
            } },
            allocator,
        );
    }

    pub fn create_block(
        _token: Token,
        statements: std.array_list.Managed(*AST),
        final: ?*AST,
        allocator: std.mem.Allocator,
    ) *AST {
        if (final) |_final| {
            std.debug.assert(_final.* == .@"return" or _final.* == .@"continue" or _final.* == .@"break");
        }
        return AST.box(
            AST{ .block = .{
                .common = AST_Common{ ._token = _token },
                ._scope = null,
                ._statements = statements,
                .defers = std.array_list.Managed(*AST).init(allocator),
                .final = final,
            } },
            allocator,
        );
    }

    pub fn create_break(_token: Token, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{
            .@"break" = .{ .common = AST_Common{ ._token = _token } },
        }, allocator);
    }

    pub fn create_continue(_token: Token, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .@"continue" = .{
            .common = AST_Common{ ._token = _token },
        } }, allocator);
    }

    pub fn create_return(_token: Token, _ret_expr: ?*AST, allocator: std.mem.Allocator) *AST {
        return AST.box(
            AST{ .@"return" = .{
                .common = AST_Common{ ._token = _token },
                ._ret_expr = _ret_expr,
                ._symbol = null,
            } },
            allocator,
        );
    }

    pub fn create_pattern_symbol(
        _token: Token,
        kind: Symbol.Kind,
        storage: Symbol.Storage,
        name: []const u8,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(
            AST{ .pattern_symbol = .{
                .common = AST_Common{ ._token = _token },
                .kind = kind,
                .storage = storage,
                .name = name,
            } },
            allocator,
        );
    }

    pub fn create_binding(
        _token: Token,
        pattern: *AST,
        _type: *Type_AST,
        init: ?*AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(
            AST{ .binding = .{
                .common = AST_Common{ ._token = _token },
                .pattern = pattern,
                .type = _type,
                .init = init,
                .decls = std.array_list.Managed(*AST).init(allocator),
            } },
            allocator,
        );
    }

    pub fn create_decl(
        _token: Token,
        name: *AST,
        _type: *Type_AST,
        init: ?*AST,
        allocator: std.mem.Allocator,
    ) *AST {
        std.debug.assert(name.* == .pattern_symbol);
        return AST.box(
            AST{ .decl = .{
                .common = AST_Common{ ._token = _token },
                .name = name,
                .type = _type,
                .init = init,
            } },
            allocator,
        );
    }

    pub fn create_fn_decl(
        _token: Token,
        name: ?*AST,
        _generic_params: std.array_list.Managed(*AST),
        params: std.array_list.Managed(*AST),
        ret_type: *Type_AST,
        context_decls: std.array_list.Managed(*AST),
        refinement: ?*AST,
        init: *AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .fn_decl = .{
            .common = AST_Common{ ._token = _token },
            .name = name,
            ._generic_params = _generic_params,
            ._params = params,
            ._param_symbols = std.array_list.Managed(*Symbol).init(allocator),
            .context_param_symbols = std.array_list.Managed(*Symbol).init(allocator),
            .ret_type = ret_type,
            .context_decls = context_decls,
            .refinement = refinement,
            .init = init,
            .infer_error = false,
        } }, allocator);
    }

    pub fn create_context_value_decl(
        _token: Token,
        parent: *Type_AST,
        init: ?*AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .context_value_decl = .{
            .common = AST_Common{ ._token = _token },
            .parent = parent,
            .init = init,
        } }, allocator);
    }

    pub fn create_context_decl(
        _token: Token,
        name: *AST,
        fields: std.array_list.Managed(*Type_AST),
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .context_decl = .{
            .common = AST_Common{ ._token = _token },
            .name = name,
            .fields = fields,
            ._type = Type_AST.create_context_type(_token, fields, allocator),
        } }, allocator);
    }

    pub fn create_type_param_decl(_token: Token, rigid: bool, constraints: std.array_list.Managed(*Type_AST), allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .type_param_decl = .{
            .common = AST_Common{ ._token = _token },
            .rigid = rigid,
            .constraints = constraints,
        } }, allocator);
    }

    pub fn create_struct_decl(
        _token: Token,
        name: *AST,
        fields: std.array_list.Managed(*Type_AST),
        _generic_params: std.array_list.Managed(*AST),
        allocator: std.mem.Allocator,
    ) *AST {
        var retval = AST.box(AST{ .struct_decl = .{
            .common = AST_Common{ ._token = _token },
            .name = name,
            .fields = fields,
            ._generic_params = _generic_params,
            ._type = Type_AST.create_struct_type(_token, fields, allocator),
        } }, allocator);
        retval.struct_decl._type.struct_type.decl = retval;
        return retval;
    }

    pub fn create_enum_decl(
        _token: Token,
        name: *AST,
        fields: std.array_list.Managed(*Type_AST),
        _generic_params: std.array_list.Managed(*AST),
        allocator: std.mem.Allocator,
    ) *AST {
        var retval = AST.box(AST{ .enum_decl = .{
            .common = AST_Common{ ._token = _token },
            .name = name,
            .fields = fields,
            ._generic_params = _generic_params,
            ._type = Type_AST.create_enum_type(_token, fields, allocator),
        } }, allocator);
        retval.enum_decl._type.enum_type.decl = retval;
        return retval;
    }

    pub fn create_type_alias(
        _token: Token,
        name: *AST,
        init: ?*Type_AST,
        _generic_params: std.array_list.Managed(*AST),
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .type_alias = .{
            .common = AST_Common{ ._token = _token },
            .name = name,
            .init = init,
            ._generic_params = _generic_params,
        } }, allocator);
    }

    pub fn create_method_decl(
        _token: Token,
        name: *AST,
        is_virtual: bool,
        receiver: ?*AST,
        params: std.array_list.Managed(*AST),
        ret_type: *Type_AST,
        context_decls: std.array_list.Managed(*AST),
        refinement: ?*AST,
        init: ?*AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .method_decl = .{
            .common = AST_Common{ ._token = _token },
            .name = name,
            .is_virtual = is_virtual,
            .receiver = receiver,
            ._params = params,
            ._param_symbols = std.array_list.Managed(*Symbol).init(allocator),
            .ret_type = ret_type,
            .context_decls = context_decls,
            .refinement = refinement,
            .init = init,
            .context_param_symbols = std.array_list.Managed(*Symbol).init(allocator),
        } }, allocator);
    }

    pub fn create_test(
        _token: Token,
        name: ?*AST,
        context_decls: std.array_list.Managed(*AST),
        init: *AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .@"test" = .{
            .common = AST_Common{ ._token = _token },
            .name = name,
            .context_decls = context_decls,
            .init = init,
            .context_param_symbols = std.array_list.Managed(*Symbol).init(allocator),
        } }, allocator);
    }

    pub fn create_module(
        _token: Token,
        _scope: *Scope,
        module: *module_.Module,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .module = .{
            .common = AST_Common{ ._token = _token },
            ._scope = _scope,
            .module = module,
        } }, allocator);
    }

    pub fn create_import(
        _token: Token,
        pattern: *AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .import = .{
            .common = AST_Common{ ._token = _token },
            .pattern = pattern,
        } }, allocator);
    }

    pub fn create_cinclude(
        _token: Token,
        _expr: *AST,
        allocator: std.mem.Allocator,
    ) *AST {
        return AST.box(AST{ .cinclude = .{
            .common = AST_Common{ ._token = _token },
            ._expr = _expr,
        } }, allocator);
    }

    pub fn create_defer(_token: Token, _statement: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(AST{ .@"defer" = .{
            .common = AST_Common{ ._token = _token },
            ._statement = _statement,
        } }, allocator);
    }

    pub fn create_errdefer(_token: Token, _statement: *AST, allocator: std.mem.Allocator) *AST {
        return AST.box(
            AST{ .@"errdefer" = .{
                .common = AST_Common{ ._token = _token },
                ._statement = _statement,
            } },
            allocator,
        );
    }

    /// Clones an AST given a substitution map. If any identifiers match a substitution, they are substituted.
    ///
    /// Deliberately does _not_ copy over symbols and scopes. These need to be re-decorated.
    pub fn clone(self: *AST, substs: *unification_.Substitutions, allocator: std.mem.Allocator) *AST {
        switch (self.*) {
            .poison => unreachable,
            .unit_value => return create_unit_value(self.token(), allocator),
            .int => return create_int(self.token(), self.int.data, allocator),
            .float => return create_float(self.token(), self.float.data, allocator),
            .string => return create_string(self.token(), self.string.data, allocator),
            .field => return create_field(self.token(), allocator),
            .identifier => {
                if (self.refers_to_type()) {
                    if (substs.get(self.token().data)) |replacement| {
                        return create_identifier(replacement.token(), allocator);
                    }
                }
                return create_identifier(self.token(), allocator);
            },
            .@"unreachable" => return create_unreachable(self.token(), allocator),
            .true => return create_true(self.token(), allocator),
            .false => return create_false(self.token(), allocator),

            .not => return create_not(self.token(), self.expr().clone(substs, allocator), allocator),
            .negate => return create_negate(self.token(), self.expr().clone(substs, allocator), allocator),
            .dereference => return create_dereference(
                self.token(),
                self.expr().clone(substs, allocator),
                allocator,
            ),
            .@"try" => return create_try(self.token(), self.expr().clone(substs, allocator), allocator),
            .default => return create_default(self.token(), self.default._type.clone(substs, allocator), allocator),
            .size_of => return create_size_of(self.token(), self.size_of._type.clone(substs, allocator), allocator),
            .write => {
                const cloned_args = clone_children(self.children().*, substs, allocator);
                return create_write(self.token(), self.write.writer.clone(substs, allocator), cloned_args, allocator);
            },
            .print => {
                const cloned_args = clone_children(self.children().*, substs, allocator);
                return create_print(self.token(), cloned_args, allocator);
            },
            .format_args => {
                const cloned_args = clone_children(self.children().*, substs, allocator);
                return create_format_args(self.token(), cloned_args, allocator);
            },
            .variant_tag => return create_variant_tag(self.token(), self.expr().clone(substs, allocator), allocator),
            .variant_name => return create_variant_name(self.token(), self.expr().clone(substs, allocator), allocator),

            .@"comptime" => return create_comptime(self.token(), self.expr().clone(substs, allocator), allocator),

            .assign => return create_assign(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .@"or" => return create_or(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .@"and" => return create_and(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .add => return create_add(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .sub => return create_sub(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .mult => return create_mult(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .div => return create_div(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .mod => return create_mod(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .equal => return create_equal(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .not_equal => return create_not_equal(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .greater => return create_greater(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .lesser => return create_lesser(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .greater_equal => return create_greater_equal(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .lesser_equal => return create_lesser_equal(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .@"catch" => return create_catch(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .@"orelse" => return create_orelse(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .call => {
                const cloned_args = clone_children(self.children().*, substs, allocator);
                return create_call(self.token(), self.lhs().clone(substs, allocator), cloned_args, allocator);
            },
            .bit_and => return create_bit_and(self.token(), self.lhs().clone(substs, allocator), self.rhs().clone(substs, allocator), allocator),
            .bit_or => return create_bit_or(self.token(), self.lhs().clone(substs, allocator), self.rhs().clone(substs, allocator), allocator),
            .bit_xor => return create_bit_xor(self.token(), self.lhs().clone(substs, allocator), self.rhs().clone(substs, allocator), allocator),
            .bit_not => return create_bit_not(self.token(), self.expr().clone(substs, allocator), allocator),
            .left_shift => return create_left_shift(self.token(), self.lhs().clone(substs, allocator), self.rhs().clone(substs, allocator), allocator),
            .right_shift => return create_right_shift(self.token(), self.lhs().clone(substs, allocator), self.rhs().clone(substs, allocator), allocator),
            .index => return create_index(
                self.token(),
                self.lhs().clone(substs, allocator),
                clone_children(self.children().*, substs, allocator),
                allocator,
            ),
            .generic_apply => return create_generic_apply(
                self.token(),
                self.lhs().clone(substs, allocator),
                Type_AST.clone_types(self.generic_apply._children, substs, allocator),
                allocator,
            ),
            .select => return create_select(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .access => return create_access(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .type_access => return create_type_access(
                self.token(),
                self.type_access._lhs_type.clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .trait => {
                const cloned_generic_params = clone_children(self.trait._generic_params, substs, allocator);
                const cloned_super_traits = Type_AST.clone_types(self.trait.super_traits, substs, allocator);
                const cloned_method_decls = clone_children(self.trait.method_decls, substs, allocator);
                const cloned_const_decls = clone_children(self.trait.const_decls, substs, allocator);
                const cloned_type_decls = clone_children(self.trait.type_decls, substs, allocator);
                return create_trait(
                    self.token(),
                    self.trait.name.clone(substs, allocator),
                    cloned_generic_params,
                    cloned_super_traits,
                    cloned_method_decls,
                    cloned_const_decls,
                    cloned_type_decls,
                    allocator,
                );
            },
            .impl => {
                const cloned_method_defs = clone_children(self.impl.method_defs, substs, allocator);
                const cloned_const_defs = clone_children(self.impl.const_defs, substs, allocator);
                const cloned_type_defs = clone_children(self.impl.type_defs, substs, allocator);
                const cloned_generic_params = clone_children(self.impl._generic_params, substs, allocator);
                var retval = create_impl(
                    self.token(),
                    if (self.impl.trait) |_trait| _trait.clone(substs, allocator) else null,
                    self.impl._type.clone(substs, allocator),
                    cloned_method_defs,
                    cloned_const_defs,
                    cloned_type_defs,
                    cloned_generic_params,
                    allocator,
                );
                for (cloned_method_defs.items) |method_def| {
                    method_def.method_decl.impl = retval;
                }
                retval.impl.impls_anon_trait = self.impl.impls_anon_trait;
                return retval;
            },
            .invoke => {
                const cloned_args = clone_children(self.children().*, substs, allocator);
                var retval = create_invoke(
                    self.token(),
                    self.lhs().clone(substs, allocator),
                    self.rhs().clone(substs, allocator),
                    cloned_args,
                    allocator,
                );
                retval.invoke.prepended = self.invoke.prepended;
                return retval;
            },
            .dyn_value => unreachable, // Shouldn't exist yet... have to clone scope?
            .enum_value => {
                var retval = create_enum_value(self.token(), allocator);
                retval.enum_value.init = if (self.enum_value.init) |init| init.clone(substs, allocator) else null;
                return retval;
            },
            .type_param_decl => return create_type_param_decl(
                self.token(),
                self.type_param_decl.rigid,
                Type_AST.clone_types(self.type_param_decl.constraints, substs, allocator),
                allocator,
            ),
            .struct_value => {
                const cloned_terms = clone_children(self.children().*, substs, allocator);
                var retval = create_struct_value(
                    self.token(),
                    self.struct_value.parent.clone(substs, allocator),
                    cloned_terms,
                    allocator,
                );
                retval.struct_value.was_slice = self.struct_value.was_slice;
                return retval;
            },
            .tuple_value => {
                const cloned_terms = clone_children(self.children().*, substs, allocator);
                return create_tuple_value(
                    self.token(),
                    cloned_terms,
                    allocator,
                );
            },
            .array_value => {
                const cloned_terms = clone_children(self.children().*, substs, allocator);
                return create_array_value(
                    self.token(),
                    cloned_terms,
                    allocator,
                );
            },
            .context_value => return create_context_value(
                self.token(),
                self.context_value.parent.clone(substs, allocator),
                clone_children(self.context_value._terms, substs, allocator),
                allocator,
            ),
            .context_value_decl => return create_context_value_decl(
                self.token(),
                self.context_value_decl.parent.clone(substs, allocator),
                if (self.context_value_decl.init) |init| init.clone(substs, allocator) else null,
                allocator,
            ),
            .context_decl => {
                const cloned_terms = Type_AST.clone_types(self.context_decl.fields, substs, allocator);
                const retval = create_context_decl(
                    self.token(),
                    self.context_decl.name.clone(substs, allocator),
                    cloned_terms,
                    allocator,
                );
                return retval;
            },
            .struct_decl => {
                const cloned_terms = Type_AST.clone_types(self.struct_decl.fields, substs, allocator);
                const cloned_generic_params = clone_children(self.struct_decl._generic_params, substs, allocator);
                const retval = create_struct_decl(
                    self.token(),
                    self.struct_decl.name.clone(substs, allocator),
                    cloned_terms,
                    cloned_generic_params,
                    allocator,
                );
                return retval;
            },
            .enum_decl => {
                const cloned_terms = Type_AST.clone_types(self.enum_decl.fields, substs, allocator);
                const cloned_generic_params = clone_children(self.enum_decl._generic_params, substs, allocator);
                const retval = create_enum_decl(
                    self.token(),
                    self.enum_decl.name.clone(substs, allocator),
                    cloned_terms,
                    cloned_generic_params,
                    allocator,
                );
                return retval;
            },
            .type_alias => {
                const cloned_generic_params = clone_children(self.type_alias._generic_params, substs, allocator);
                const retval = create_type_alias(
                    self.token(),
                    self.type_alias.name.clone(substs, allocator),
                    if (self.type_alias.init == null) null else self.type_alias.init.?.clone(substs, allocator),
                    cloned_generic_params,
                    allocator,
                );
                return retval;
            },
            .as => return create_as(
                self.token(),
                self.expr().clone(substs, allocator),
                self.type().clone(substs, allocator),
                allocator,
            ),
            .addr_of => return create_addr_of(
                self.token(),
                self.expr().clone(substs, allocator),
                self.mut(),
                self.addr_of.multiptr,
                allocator,
            ),
            .slice_of => return create_slice_of(
                self.token(),
                self.expr().clone(substs, allocator),
                self.slice_of.mut,
                allocator,
            ),
            .sub_slice => return create_sub_slice(
                self.token(),
                self.sub_slice.super.clone(substs, allocator),
                if (self.sub_slice.lower) |lower| lower.clone(substs, allocator) else null,
                if (self.sub_slice.upper) |upper| upper.clone(substs, allocator) else null,
                allocator,
            ),
            .range => return create_range(
                self.token(),
                self.range.lower.clone(substs, allocator),
                self.range.upper.clone(substs, allocator),
                allocator,
            ),
            .receiver => {
                var retval = create_receiver(
                    self.token(),
                    self.receiver.kind,
                    allocator,
                );
                retval.receiver._type = if (retval.receiver._type) |_type| _type.clone(substs, allocator) else null;
                retval.receiver.value_derefd = self.receiver.value_derefd;
                return retval;
            },

            .@"if" => return create_if(
                self.token(),
                if (self.@"if".let) |let| let.clone(substs, allocator) else null,
                self.@"if".condition.clone(substs, allocator),
                self.body_block().clone(substs, allocator),
                if (self.else_block()) |_else| _else.clone(substs, allocator) else null,
                allocator,
            ),
            .match => {
                const cloned_mappings = clone_children(self.children().*, substs, allocator);
                return create_match(
                    self.token(),
                    if (self.match.let) |let| let.clone(substs, allocator) else null,
                    self.expr().clone(substs, allocator),
                    cloned_mappings,
                    allocator,
                );
            },
            .mapping => return create_mapping(
                self.token(),
                self.lhs().clone(substs, allocator),
                self.rhs().clone(substs, allocator),
                allocator,
            ),
            .@"while" => return create_while(
                self.token(),
                if (self.@"while".let) |let| let.clone(substs, allocator) else null,
                self.@"while".condition.clone(substs, allocator),
                if (self.@"while".post) |post| post.clone(substs, allocator) else null,
                self.body_block().clone(substs, allocator),
                if (self.else_block()) |_else| _else.clone(substs, allocator) else null,
                allocator,
            ),
            .@"for" => return create_for(
                self.token(),
                if (self.@"for".let) |let| let.clone(substs, allocator) else null,
                self.@"for".elem.clone(substs, allocator),
                self.@"for".into_iter.clone(substs, allocator),
                self.@"for"._body_block.clone(substs, allocator),
                if (self.@"for"._else_block) |_else_block| _else_block.clone(substs, allocator) else null,
                allocator,
            ),
            .with => {
                const cloned_contexts = clone_children(self.with.contexts, substs, allocator);
                return create_with(
                    self.token(),
                    cloned_contexts,
                    self.with._body_block.clone(substs, allocator),
                    allocator,
                );
            },
            .block => {
                const cloned_statements = clone_children(self.children().*, substs, allocator);
                return create_block(
                    self.token(),
                    cloned_statements,
                    if (self.block.final) |final| final.clone(substs, allocator) else null,
                    allocator,
                );
            },
            .@"break" => return create_break(self.token(), allocator),
            .@"continue" => return create_continue(self.token(), allocator),
            .@"return" => return create_return(
                self.token(),
                if (self.@"return"._ret_expr) |ret_expr| ret_expr.clone(substs, allocator) else null,
                allocator,
            ),
            .pattern_symbol => return create_pattern_symbol(
                self.token(),
                self.pattern_symbol.kind,
                self.pattern_symbol.storage,
                self.pattern_symbol.name,
                allocator,
            ),
            .binding => {
                const retval = create_binding(
                    self.token(),
                    self.binding.pattern.clone(substs, allocator),
                    self.binding.type.clone(substs, allocator),
                    if (self.binding.init) |init| init.clone(substs, allocator) else null,
                    allocator,
                );
                return retval;
            },
            .decl => {
                const retval = create_decl(
                    self.token(),
                    self.decl.name.clone(substs, allocator),
                    self.decl.type.clone(substs, allocator),
                    if (self.decl.init) |init| init.clone(substs, allocator) else null,
                    allocator,
                );
                return retval;
            },
            .fn_decl => {
                const cloned_generic_params = clone_children(self.fn_decl._generic_params, substs, allocator);
                const cloned_contexts = clone_children(self.fn_decl.context_decls, substs, allocator);
                const cloned_params = clone_children(self.children().*, substs, allocator);
                var retval = create_fn_decl(
                    self.token(),
                    if (self.fn_decl.name) |name| name.clone(substs, allocator) else null,
                    cloned_generic_params,
                    cloned_params,
                    self.fn_decl.ret_type.clone(substs, allocator),
                    cloned_contexts,
                    if (self.fn_decl.refinement) |refinement| refinement.clone(substs, allocator) else null,
                    self.fn_decl.init.clone(substs, allocator),
                    allocator,
                );
                retval.fn_decl._decl_type = if (self.fn_decl._decl_type != null) self.fn_decl._decl_type.?.clone(substs, allocator) else null;
                return retval;
            },
            .method_decl => {
                const cloned_params = clone_children(self.children().*, substs, allocator);
                const cloned_contexts = clone_children(self.method_decl.context_decls, substs, allocator);
                var retval = create_method_decl(
                    self.token(),
                    self.method_decl.name.clone(substs, allocator),
                    self.method_decl.is_virtual,
                    if (self.method_decl.receiver) |receiver| receiver.clone(substs, allocator) else null,
                    cloned_params,
                    self.method_decl.ret_type.clone(substs, allocator),
                    cloned_contexts,
                    if (self.method_decl.refinement) |refinement| refinement.clone(substs, allocator) else null,
                    if (self.method_decl.init) |init| init.clone(substs, allocator) else null,
                    allocator,
                );
                retval.method_decl.impl = self.method_decl.impl;
                retval.method_decl._decl_type = if (self.method_decl._decl_type != null) self.method_decl._decl_type.?.clone(substs, allocator) else null;
                return retval;
            },
            .@"test" => return create_test(
                self.token(),
                self.@"test".name,
                clone_children(self.@"test".context_decls, substs, allocator),
                self.@"test".init,
                allocator,
            ),
            .import => return create_import(self.token(), self.import.pattern.clone(substs, allocator), allocator),
            .cinclude => return create_cinclude(self.token(), self.cinclude._expr.clone(substs, allocator), allocator),
            .module => return create_module(self.token(), self.scope().?, self.module.module, allocator),
            .@"defer" => return create_defer(self.token(), self.statement().clone(substs, allocator), allocator),
            .@"errdefer" => return create_errdefer(self.token(), self.statement().clone(substs, allocator), allocator),
        }
    }

    pub fn clone_children(children_terms: std.array_list.Managed(*AST), substs: *unification_.Substitutions, allocator: std.mem.Allocator) std.array_list.Managed(*AST) {
        var retval = std.array_list.Managed(*AST).init(allocator);
        for (children_terms.items) |child| {
            retval.append(child.clone(substs, allocator)) catch unreachable;
        }
        return retval;
    }

    /// Boxes an AST value into an allocator.
    fn box(ast: AST, alloc: std.mem.Allocator) *AST {
        const retval = alloc.create(AST) catch unreachable;
        retval.* = ast;
        return retval;
    }

    pub fn common(self: *AST) *AST_Common {
        return union_fields_.get_field_ref(self, "common");
    }

    pub fn token(self: *AST) Token {
        return self.common()._token;
    }

    pub fn expr(self: AST) *AST {
        return union_fields_.get_struct_field(self, "_expr");
    }

    pub fn set_expr(self: *AST, val: *AST) void {
        union_fields_.set_field(self, "_expr", val);
    }

    pub fn @"type"(self: AST) *Type_AST {
        return switch (self) {
            .as => self.as._type,
            .size_of => self.size_of._type,
            .default => self.default._type,
            else => std.debug.panic("can't call type on {s}\n", .{@tagName(self)}),
        };
    }

    pub fn symbol(self: AST) ?*Symbol {
        return union_fields_.get_struct_field(self, "_symbol");
    }

    pub fn set_symbol(self: *AST, val: ?*Symbol) void {
        union_fields_.set_field(self, "_symbol", val);
    }

    pub fn scope(self: AST) ?*Scope {
        return union_fields_.get_struct_field(self, "_scope");
    }

    pub fn set_scope(self: *AST, val: ?*Scope) void {
        union_fields_.set_field(self, "_scope", val);
    }

    pub fn lhs(self: AST) *AST {
        return union_fields_.get_struct_field(self, "_lhs");
    }

    pub fn set_lhs(self: *AST, val: *AST) void {
        union_fields_.set_field(self, "_lhs", val);
    }

    pub fn rhs(self: AST) *AST {
        return union_fields_.get_struct_field(self, "_rhs");
    }

    pub fn set_rhs(self: *AST, val: *AST) void {
        union_fields_.set_field(self, "_rhs", val);
    }

    pub fn children(self: *AST) *std.array_list.Managed(*AST) {
        return switch (self.*) {
            .call => &self.call._args,
            .index => &self.index._children,
            .context_value => &self.context_value._terms,
            .struct_value => &self.struct_value._terms,
            .tuple_value => &self.tuple_value._terms,
            .array_value => &self.array_value._terms,
            .match => &self.match._mappings,
            .block => &self.block._statements,
            .fn_decl => &self.fn_decl._params,
            .method_decl => &self.method_decl._params,
            .invoke => &self.invoke._args,
            .write => &self.write._children,
            .print => &self.print._children,
            .format_args => &self.format_args._children,
            .binding => &self.binding.decls,
            else => std.debug.panic("compiler error: cannot call `.children()` on the AST `{f}`", .{self.*}),
        };
    }

    pub fn set_children(self: *AST, val: std.array_list.Managed(*AST)) void {
        switch (self.*) {
            .call => self.call._args = val,
            .context_value => self.context_value._terms = val,
            .struct_value => self.struct_value._terms = val,
            .tuple_value => self.tuple_value._terms = val,
            .array_value => self.array_value._terms = val,
            .match => self.match._mappings = val,
            .block => self.block._statements = val,
            .fn_decl => self.fn_decl._params = val,
            .method_decl => self.method_decl._params = val,
            .invoke => self.invoke._args = val,
            .binding => self.binding.decls = val,
            else => std.debug.panic("compiler error: cannot call `.set_children()` on the AST `{s}`", .{@tagName(self.*)}),
        }
    }

    pub fn context_args(self: *AST) *std.array_list.Managed(*Symbol) {
        return switch (self.*) {
            .call => &self.call.context_args,
            .invoke => &self.invoke.context_args,
            else => std.debug.panic("compiler error: cannot call `.context_args()` on the AST `{s}`", .{@tagName(self.*)}),
        };
    }

    pub fn decl_type(self: *AST) *Type_AST {
        return switch (self.*) {
            .decl => self.decl.type,
            .fn_decl => self.fn_decl._decl_type.?,
            .method_decl => self.method_decl._decl_type.?,
            .@"test" => self.@"test"._decl_type.?,
            .module, .trait => prelude_.unit_type,
            .receiver => self.receiver._type.?,
            .context_value_decl => self.context_value_decl.parent,
            else => std.debug.panic("compiler error: cannot call `.decl_type()` on the AST `{s}`", .{@tagName(self.*)}),
        };
    }

    pub fn decl_init(self: *AST) ?*AST {
        return switch (self.*) {
            .decl => self.decl.init,
            .fn_decl => self.fn_decl.init,
            .method_decl => self.method_decl.init,
            .@"test" => self.@"test".init,
            .module, .trait => self,
            .struct_decl, .enum_decl, .type_alias, .receiver, .type_param_decl, .context_decl, .context_value => null,
            .context_value_decl => self.context_value_decl.init,
            else => std.debug.panic("compiler error: cannot call `.decl_init()` on the AST `{s}`", .{@tagName(self.*)}),
        };
    }

    pub fn set_decl_init(self: *AST, init: *AST) void {
        return switch (self.*) {
            .decl => self.decl.init = init,
            .fn_decl => self.fn_decl.init = init,
            .method_decl => self.method_decl.init = init,
            .@"test" => self.@"test".init = init,
            .trait => self.* = init.*,
            .module => {},
            else => std.debug.panic("compiler error: cannot call `.set_decl_init()` on the AST `{s}`", .{@tagName(self.*)}),
        };
    }

    pub fn decl_typedef(self: *AST) ?*Type_AST {
        return switch (self.*) {
            .context_decl => self.context_decl._type,
            .struct_decl => self.struct_decl._type,
            .enum_decl => self.enum_decl._type,
            .type_alias => self.type_alias.init,
            .type_param_decl => null, // No type... yet!
            .module => null,
            else => std.debug.panic("compiler error: cannot call `.decl_typedef()` on the AST `{s}`", .{@tagName(self.*)}),
        };
    }

    pub fn decl_name(self: *AST) *AST {
        return switch (self.*) {
            .struct_decl => self.struct_decl.name,
            .enum_decl => self.enum_decl.name,
            .type_alias => self.type_alias.name,
            .context_decl => self.context_decl.name,
            else => std.debug.panic("compiler error: cannot call `.decl_name()` on the AST `{s}`", .{@tagName(self.*)}),
        };
    }

    pub fn set_decl_name(self: *AST, name: *AST) void {
        switch (self.*) {
            .struct_decl => self.struct_decl.name = name,
            .enum_decl => self.enum_decl.name = name,
            .type_alias => self.type_alias.name = name,
            .fn_decl => self.fn_decl.name = name,
            .context_decl => self.context_decl.name = name,
            .trait => self.trait.name = name,
            else => std.debug.panic("compiler error: cannot call `.decl_name()` on the AST `{s}`", .{@tagName(self.*)}),
        }
    }

    pub fn param_symbols(self: *AST) ?*std.array_list.Managed(*Symbol) {
        return switch (self.*) {
            .fn_decl => &self.fn_decl._param_symbols,
            .method_decl => &self.method_decl._param_symbols,
            .@"test" => null,
            else => std.debug.panic("compiler error: cannot call `.param_symbols()` on the AST `{s}`", .{@tagName(self.*)}),
        };
    }

    pub fn context_param_symbols(self: *AST) ?*std.array_list.Managed(*Symbol) {
        return switch (self.*) {
            .fn_decl => &self.fn_decl.context_param_symbols,
            .@"test" => &self.@"test".context_param_symbols,
            .method_decl => &self.method_decl.context_param_symbols,
            else => std.debug.panic("compiler error: cannot call `.context_param_symbols()` on the AST `{s}`", .{@tagName(self.*)}),
        };
    }

    pub fn num_generic_params(self: *AST) usize {
        return switch (self.*) {
            .struct_decl, .enum_decl, .type_alias, .impl, .fn_decl, .trait => self.generic_params().items.len,
            else => 0,
        };
    }

    pub fn generic_params(self: *AST) *std.array_list.Managed(*AST) {
        return switch (self.*) {
            .struct_decl => &self.struct_decl._generic_params,
            .enum_decl => &self.enum_decl._generic_params,
            .type_alias => &self.type_alias._generic_params,
            .impl => &self.impl._generic_params,
            .fn_decl => &self.fn_decl._generic_params,
            .trait => &self.trait._generic_params,
            else => std.debug.panic("compiler error: cannot call `.generic_params()` on the AST `{t}`", .{self.*}),
        };
    }

    pub fn set_generic_params(self: *AST, _generic_params: std.array_list.Managed(*AST)) void {
        switch (self.*) {
            .struct_decl => self.struct_decl._generic_params = _generic_params,
            .enum_decl => self.enum_decl._generic_params = _generic_params,
            .type_alias => self.type_alias._generic_params = _generic_params,
            .impl => self.impl._generic_params = _generic_params,
            .fn_decl => self.fn_decl._generic_params = _generic_params,
            .trait => self.trait._generic_params = _generic_params,
            else => std.debug.panic("compiler error: cannot call `.set_generic_params()` on the AST `{t}`", .{self.*}),
        }
    }

    pub fn top_level(self: *AST) bool {
        return switch (self.*) {
            .decl, .context_value_decl => false,
            .fn_decl, .method_decl, .@"test", .enum_decl => true,
            else => std.debug.panic("compiler error: cannot call `.top_level()` on the AST `{s}`", .{@tagName(self.*)}),
        };
    }

    pub fn get_field(self: *AST, _type: *Type_AST, field_name: []const u8) *AST {
        for (0.., _type.children().items) |i, child| {
            if (child.* == .annotation and std.mem.eql(u8, field_name, child.annotation.pattern.token().data)) {
                return self.children().items[i];
            }
        }
        std.debug.panic("compiler error: {s} didn't have field `{s}`", .{ @tagName(self.*), field_name });
    }

    pub fn pos(self: AST) ?usize {
        return union_fields_.get_struct_field(self, "_pos");
    }

    pub fn set_pos(self: *AST, val: ?usize) void {
        union_fields_.set_field(self, "_pos", val);
    }

    pub fn statement(self: AST) *AST {
        return union_fields_.get_struct_field(self, "_statement");
    }

    pub fn set_statement(self: *AST, val: *AST) void {
        union_fields_.set_field(self, "_statement", val);
    }

    pub fn body_block(self: AST) *AST {
        return union_fields_.get_struct_field(self, "_body_block");
    }

    pub fn set_body_block(self: *AST, val: *AST) void {
        union_fields_.set_field(self, "_body_block", val);
    }

    pub fn else_block(self: AST) ?*AST {
        return union_fields_.get_struct_field(self, "_else_block");
    }

    pub fn set_else_block(self: *AST, val: ?*AST) void {
        union_fields_.set_field(self, "_else_block", val);
    }

    pub fn mut(self: AST) bool {
        return union_fields_.get_struct_field(self, "mut");
    }

    pub fn set_mut(self: *AST, val: bool) void {
        union_fields_.set_field(self, "mut", val);
    }

    // Expr must be an array value of length `l`. Slice value is `(&expr[0], l)`.
    pub fn create_slice_value(_expr: *AST, _mut: bool, expr_type: *Type_AST, allocator: std.mem.Allocator) *AST {
        var new_terms = std.array_list.Managed(*AST).init(allocator);
        const zero = (AST.create_int(_expr.token(), 0, allocator));
        var index_rhs = std.array_list.Managed(*AST).init(allocator);
        index_rhs.append(zero) catch unreachable;
        const index = (AST.create_index(
            _expr.token(),
            _expr,
            index_rhs,
            allocator,
        ));
        const addr = (AST.create_addr_of(
            _expr.token(),
            index,
            _mut,
            true,
            allocator,
        ));
        new_terms.append(addr) catch unreachable;

        const length = expr_type.array_of.len;
        new_terms.append(length) catch unreachable;

        const elem_type = expr_type.child();
        const slice_type = Type_AST.create_slice_type(elem_type, _mut, allocator);
        var retval = AST.create_struct_value(_expr.token(), slice_type, new_terms, allocator);
        retval.struct_value.was_slice = true;
        return retval;
    }

    pub fn create_some_value(opt_type: *Type_AST, value: *AST, allocator: std.mem.Allocator) *AST {
        const member = create_enum_value(value.token(), allocator);
        member.enum_value.base = opt_type;
        member.enum_value.init = value;
        member.set_pos(opt_type.get_pos("some"));
        return member.assert_ast_valid();
    }

    pub fn create_none_value(opt_type: *Type_AST, allocator: std.mem.Allocator) *AST {
        const member = create_enum_value(Token.init_simple("none"), allocator);
        member.enum_value.base = opt_type;
        member.set_pos(opt_type.get_pos("none"));
        return member.assert_ast_valid();
    }

    pub fn create_binop(_token: Token, _lhs: *AST, _rhs: *AST, allocator: std.mem.Allocator) *AST {
        switch (_token.kind) {
            .plus_equals => return create_add(_token, _lhs, _rhs, allocator),
            .minus_equals => return create_sub(_token, _lhs, _rhs, allocator),
            .star_equals => return create_mult(_token, _lhs, _rhs, allocator),
            .slash_equals => return create_div(_token, _lhs, _rhs, allocator),
            .percent_equals => return create_mod(_token, _lhs, _rhs, allocator),
            .double_lesser_equals => return create_left_shift(_token, _lhs, _rhs, allocator),
            .double_greater_equals => return create_right_shift(_token, _lhs, _rhs, allocator),
            .bar_equals => return create_bit_or(_token, _lhs, _rhs, allocator),
            .ampersand_equals => return create_bit_and(_token, _lhs, _rhs, allocator),
            .tilde_equals => return create_bit_xor(_token, _lhs, _rhs, allocator),
            else => std.debug.panic("compiler error: {s} is not a operator-assign token", .{@tagName(_token.kind)}),
        }
    }

    /// Determines if an AST expression can be evaluated at compile-time without having to specify a `comptime`
    /// keyword.
    pub fn is_comptime_expr(self: *AST) bool {
        // It's easier to list all the ASTs that AREN'T comptime! :-)
        return !(self.* == .@"try" or
            self.* == .block or
            self.* == .call);
        // AST kinds like `and` and `or` are technically control-flow, but conceptually, they terminate and
        // are pure.
        // `try` breaks normal control-flow. `block` allows for unpure statements like `continue`, `break`,
        // `return`, and `try`. `call` is non-comptime because functions are not pure in Orange.
        // Kinds like `and`, `or`, `orelse`, `catch`, `if`, `match`, and `while` are in a way "purer", and it
        // would be annoying to have to wrap these in comptime.
    }

    pub fn refers_to_type(self: *AST) bool {
        return switch (self.*) {
            else => false,

            .identifier, .access, .type_access => if (self.symbol()) |sym| sym.is_type() else false,

            .index => self.lhs().refers_to_type(), // generic type
            .generic_apply => self.lhs().refers_to_type(),
        };
    }

    /// Used to poison an AST node. Marks as valid, so any attempt to validate is memoized to return poison.
    pub fn enpoison(self: *AST) void {
        self.common().validation_state = .invalid;
    }

    /// Sets an ASTs validation status to valid.
    pub fn assert_ast_valid(self: *AST) *AST {
        self.common().validation_state = .valid;
        return self;
    }

    // TODO: Use Tree Writer, don't call writer print, recursively call pprint
    pub fn pprint(self: AST, allocator: std.mem.Allocator) ![]const u8 {
        var out = std.array_list.Managed(u8).init(allocator);
        defer out.deinit();

        switch (self) {
            .poison => try out.print("poison", .{}),
            .unit_value => try out.print("unit_value", .{}),
            .int => try out.print("int({})", .{self.int.data}),
            .float => try out.print("float()", .{}),
            .string => try out.print("string({s})", .{self.string.data}),
            .field => try out.print("field(\"{s}\")", .{self.field.common._token.data}),
            .identifier => try out.print("identifier(\"{s}\")", .{self.identifier.common._token.data}),
            .@"unreachable" => try out.print("unreachable", .{}),
            .true => try out.print("true", .{}),
            .false => try out.print("false", .{}),

            .not => try out.print("not()", .{}),
            .negate => try out.print("negate({f})", .{self.expr()}),
            .dereference => try out.print("dereference()", .{}),
            .@"try" => try out.print("try()", .{}),
            .default => {
                try out.print("default({f})", .{self.default._type});
            },
            .size_of => try out.print("size_of({f})", .{self.type()}),
            .variant_tag => try out.print("variant_tag({f})", .{self.expr()}),
            .variant_name => try out.print("variant_name({f})", .{self.expr()}),
            .@"comptime" => try out.print("comptime({f})", .{self.expr()}),

            .assign => {
                try out.print("assign({f}, {f})", .{ self.lhs(), self.rhs() });
            },
            .@"or" => try out.print("or()", .{}),
            .@"and" => try out.print("and()", .{}),
            .add => try out.print("add()", .{}),
            .sub => try out.print("sub()", .{}),
            .mult => try out.print("mult()", .{}),
            .div => try out.print("div()", .{}),
            .mod => try out.print("mod()", .{}),
            .equal => try out.print("equal()", .{}),
            .not_equal => try out.print("not_equal()", .{}),
            .greater => try out.print("greater()", .{}),
            .lesser => try out.print("lesser()", .{}),
            .greater_equal => try out.print("greater_equal()", .{}),
            .lesser_equal => try out.print("lesser_equal()", .{}),
            .@"catch" => try out.print("catch()", .{}),
            .@"orelse" => try out.print("orelse()", .{}),
            .call => {
                try out.print("call(lhs={f},args=[", .{self.call._lhs});
                for (self.call._args.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.call._args.items.len - 1) {
                        try out.print(",", .{});
                    }
                }
                try out.print("])", .{});
            },
            .write => {
                try out.print("write(", .{});
                for (self.write._children.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.write._children.items.len - 1) {
                        try out.print(",", .{});
                    }
                }
                try out.print(")", .{});
            },
            .print => {
                try out.print("print(", .{});
                for (self.print._children.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.print._children.items.len - 1) {
                        try out.print(",", .{});
                    }
                }
                try out.print(")", .{});
            },
            .format_args => {
                try out.print("format_args(", .{});
                for (self.print._children.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.print._children.items.len - 1) {
                        try out.print(",", .{});
                    }
                }
                try out.print(")", .{});
            },
            .bit_and => try out.print("bit_and({f}, {f})", .{ self.lhs(), self.rhs() }),
            .bit_or => try out.print("bit_or({f}, {f})", .{ self.lhs(), self.rhs() }),
            .bit_xor => try out.print("bit_xor({f}, {f})", .{ self.lhs(), self.rhs() }),
            .bit_not => try out.print("bit_not({f})", .{self.expr()}),
            .left_shift => try out.print("left_shift({f}, {f})", .{ self.lhs(), self.rhs() }),
            .right_shift => try out.print("right_shift({f}, {f})", .{ self.lhs(), self.rhs() }),
            .index => {
                try out.print("index(lhs={f},args=[", .{self.index._lhs});
                for (self.index._children.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.index._children.items.len - 1) {
                        try out.print(",", .{});
                    }
                }
                try out.print("])", .{});
            },
            .generic_apply => {
                try out.print("generic_apply(lhs={f},args=[", .{self.generic_apply._lhs});
                for (self.generic_apply._children.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.generic_apply._children.items.len - 1) {
                        try out.print(",", .{});
                    }
                }
                try out.print("])", .{});
            },
            .select => {
                try out.print("select({f},{f})", .{ self.lhs(), self.rhs() });
            },
            .access => {
                try out.print("access({f},{f})", .{ self.lhs(), self.rhs() });
            },
            .type_access => try out.print("type_access({f},{f})", .{ self.type_access._lhs_type, self.rhs() }),
            .trait => try out.print("trait({*})", .{if (self.symbol() != null) self.symbol() else null}),
            .impl => {
                try out.print("impl(\n    .trait={?f},\n    .type={f}\n    .method_defs=[\n", .{ self.impl.trait, self.impl._type });
                for (self.impl.method_defs.items) |method| {
                    try out.print("        {f}\n", .{method});
                }
                try out.print("    ],\n    .generic_params=[\n", .{});
                for (self.impl._generic_params.items) |gen_param| {
                    try out.print("        {f}\n", .{gen_param});
                }
                try out.print("    ],\n)", .{});
            },
            .invoke => try out.print("invoke()", .{}),
            .dyn_value => try out.print("dyn_value()", .{}),
            .enum_value => {
                try out.print("enum_value(.name={s}, .init={?f}, .tag={?})", .{ self.enum_value.get_name(), self.enum_value.init, self.enum_value._pos });
            },
            .context_value => try out.print("context_value({s})", .{self.context_value.common._token.data}),
            .context_value_decl => try out.print("context_value_decl()", .{}),
            .context_decl => {
                try out.print("context_decl(\n", .{});
                try out.print("\t.name = {f}\n", .{self.context_decl.name});
                try out.print("\t.fields = {f}\n", .{fmt_.List_Printer(Type_AST){ .list = &self.context_decl.fields }});
                try out.print(")", .{});
            },
            .type_param_decl => try out.print("type_param_decl({s})", .{self.type_param_decl.common._token.data}),
            .struct_decl => {
                try out.print("struct_decl(\n", .{});
                try out.print("\t.name = {f}\n", .{self.struct_decl.name});
                try out.print("\t.fields = {f}\n", .{fmt_.List_Printer(Type_AST){ .list = &self.struct_decl.fields }});
                try out.print("\t.generic_params = {f}\n", .{fmt_.List_Printer(AST){ .list = &self.struct_decl._generic_params }});
                try out.print(")", .{});
            },
            .enum_decl => {
                try out.print("enum()", .{});
            },
            .type_alias => {
                try out.print("type_alias()", .{});
            },
            .struct_value => {
                try out.print("struct_value(", .{});
                for (self.struct_value._terms.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.struct_value._terms.items.len - 1) {
                        try out.print(",", .{});
                    }
                }
                try out.print(")", .{});
            },
            .tuple_value => {
                try out.print("tuple_value(", .{});
                for (self.tuple_value._terms.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.tuple_value._terms.items.len - 1) {
                        try out.print(",", .{});
                    }
                }
                try out.print(")", .{});
            },
            .array_value => {
                try out.print("array_value(", .{});
                for (self.array_value._terms.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.array_value._terms.items.len - 1) {
                        try out.print(",", .{});
                    }
                }
                try out.print(")", .{});
            },

            .as => try out.print("as({f}, {f})", .{ self.expr(), self.type() }),
            .addr_of => try out.print("addr_of({f})", .{self.expr()}),
            .slice_of => try out.print("slice_of()", .{}),
            .sub_slice => try out.print("sub_slice()", .{}),
            .range => try out.print("range()", .{}),
            .receiver => try out.print("receiver({?f})", .{self.receiver._type}),

            .@"if" => try out.print("if()", .{}),
            .match => try out.print("match()", .{}),
            .mapping => try out.print("mapping()", .{}),
            .@"while" => try out.print("while()", .{}),
            .@"for" => try out.print("for()", .{}),
            .with => try out.print("with()", .{}),
            .block => {
                try out.print("block(", .{});
                for (self.block._statements.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.block._statements.items.len) {
                        try out.print(",", .{});
                    }
                }
                try out.print(".final={?f})", .{self.block.final});
            },
            .@"break" => try out.print("break", .{}),
            .@"continue" => try out.print("continue", .{}),
            .@"return" => try out.print("return()", .{}),
            .pattern_symbol => try out.print("pattern_symbol({s}, {s}@{?})", .{
                @tagName(self.pattern_symbol.kind),
                self.pattern_symbol.name,
                if (self.pattern_symbol._symbol) |sym| sym.scope.uid else null,
            }),
            .binding => {
                try out.print("binding(.type={f})", .{self.binding.type});
            },
            .decl => {
                try out.print("decl(\n", .{});
                try out.print("    .pattern = {f},\n", .{self.decl.name});
                try out.print("    .type = {f},\n", .{self.decl.type});
                try out.print("    .init = {?f},\n", .{self.decl.init});
                try out.print(")", .{});
            },
            .fn_decl => {
                try out.print("fn_decl(\n", .{});
                try out.print("    .name = {?f},\n", .{self.fn_decl.name});
                try out.print("    ._params = [", .{});
                for (self.fn_decl._params.items, 0..) |item, i| {
                    try out.print("{f}", .{item});
                    if (i < self.fn_decl._params.items.len) {
                        try out.print(",", .{});
                    }
                }
                try out.print("],\n", .{});
                try out.print("    .param_symbols = [", .{});
                for (self.fn_decl._param_symbols.items, 0..) |item, i| {
                    try out.print("{s}", .{item.name});
                    if (i < self.fn_decl._param_symbols.items.len) {
                        try out.print(",", .{});
                    }
                }
                try out.print("],\n", .{});
                try out.print("    .ret_type = {f},\n", .{self.fn_decl.ret_type});
                try out.print("    .refinement = {?f},\n", .{self.fn_decl.refinement});
                // try out.print("    .init = {f},\n", .{self.fn_decl.init});
                try out.print("    ._symbol = {?s},\n", .{if (self.fn_decl._symbol) |sym| sym.name else null});
                try out.print("    .infer_error = {},\n", .{self.fn_decl.infer_error});
                try out.print(")", .{});
            },
            .module => try out.print("module()", .{}),
            .import => try out.print("import({f})", .{self.import.pattern}),
            .cinclude => try out.print("cinclude({f})", .{self.cinclude._expr}),
            .method_decl => {
                try out.print("method_decl(.name={f}, .receiver={?f}, . .params=[", .{
                    self.method_decl.name,
                    self.method_decl.receiver,
                });
                for (self.method_decl._params.items, 0..) |param, i| {
                    try out.print("{f}", .{param});
                    if (i < self.method_decl._params.items.len) {
                        try out.print(",", .{});
                    }
                }
                try out.print("], .ret_type={f})", .{self.method_decl.ret_type});
            },
            .@"test" => try out.print("test(.name={?f})", .{self.@"test".name}),
            .@"defer" => try out.print("defer()", .{}),
            .@"errdefer" => try out.print("errdefer()", .{}),
        }

        return try out.toOwnedSlice();
    }

    pub fn format(self: AST, writer: *std.io.Writer) !void {
        try fmt_.indirect_format(self, writer);
    }
};
