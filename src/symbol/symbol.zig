const std = @import("std");
const anon_name_ = @import("../util/anon_name.zig");
const ast_ = @import("../ast/ast.zig");
const CFG = @import("../ir/cfg.zig");
const errs_ = @import("../util/errors.zig");
const fmt_ = @import("../util/fmt.zig");
const Scope = @import("../symbol/scope.zig");
const Span = @import("../util/span.zig");
const Token = @import("../lexer/token.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const generic_arg_ = @import("../ast/generic_arg.zig");
const Monomorph_Map = @import("../types/type_map.zig").Monomorph_Map;
const unification_ = @import("../types/unification.zig");
const validation_state_ = @import("../util/validation_state.zig");

const Self = @This();

pub const Kind = union(enum) {
    @"fn",
    @"const",
    @"comptime",
    let,
    mut,
    type,
    trait,
    import: struct { // Refers indirectly to modules, or to refinements on modules.
        // Real name of the module, as oposed to the `as` name
        real_name: []const u8,
        real_symbol: ?*Self = null,
    },
    import_inner, // Created from the inner expressions of qualified import statements, similar to consts
    module, // Refers to modules. The init is the `module` AST, which refers to the module and to the scope. `Module`s have their symbol
    @"test",
    context,
};

pub const Storage = union(enum) {
    local,
    @"extern": struct { c_name: ?*ast_.AST },
};

pub const Symbol_Validation_State = validation_state_.Validation_State;

scope: *Scope, // Enclosing parent scope
name: []const u8,
kind: Kind,
cfg: ?*CFG,
decl: ?*ast_.AST,
storage: Storage,

monomorphs: Monomorph_Map(*Self),

// Use-def
aliases: u64 = 0, // How many times the symbol is taken as a mutable address
roots: u64 = 0, // How many times the symbol is the root of an lvaue tree
uses: u64 = 0,
defs: u64 = 0,

defined: bool, // Used for decorating identifiers. True when the symbol is defined at the identifier
validation_state: Symbol_Validation_State,
init_validation_state: Symbol_Validation_State,
param: bool, // True when the symbol is a parameter in a function
is_temp: bool = false, // Whether this symbol is a temporary when lowered
is_monomorphed: bool = false, // Whether this symbol came from monomorphing a generic
in_generic_impl: bool = false, // Whether this symbol is declared inside a generic impl template, whose init cannot be comptime evaluated

// Offset
offset: ?i64, // The offset from the BP that this symbol, for local variables and parameters

pub fn init(
    scope: *Scope,
    name: []const u8,
    decl: ?*ast_.AST,
    kind: Kind,
    storage: Storage,
    allocator: std.mem.Allocator,
) *Self {
    const retval = allocator.create(Self) catch unreachable;
    retval.* = .{
        .scope = scope,
        .name = name,
        .decl = decl,
        .kind = kind,
        .storage = storage,
        .monomorphs = Monomorph_Map(*Self).init(allocator),
        .cfg = null,
        .defined = kind == .@"fn" or kind == .@"const",
        .validation_state = .unvalidated,
        .init_validation_state = .unvalidated,
        .param = false,
        .offset = null,
    };
    return retval;
}

pub fn assert_symbol_valid(self: *Self) *Self {
    self.validation_state = .valid;
    return self;
}

pub fn assert_init_valid(self: *Self) *Self {
    self.init_validation_state = .valid;
    return self;
}

/// Whether or not this symbol represents a type or not
pub fn is_type(self: *const Self) bool {
    return self.decl.?.* == .struct_decl or self.decl.?.* == .enum_decl or self.decl.?.* == .type_alias or self.decl.?.is_typelike_param_decl();
}

pub fn is_nominal_type(self: *const Self) bool {
    if (self.storage == .@"extern") return true;
    const decl = self.decl orelse return false;
    return decl.* == .struct_decl or decl.* == .enum_decl or decl.* == .context_decl;
}

pub fn is_trait(self: *const Self) bool {
    return self.kind == .trait;
}

pub fn @"type"(self: *const Self) *Type_AST {
    return self.decl.?.decl_type();
}

pub fn init_value(self: *const Self) ?*ast_.AST {
    return self.decl.?.decl_init();
}

pub fn init_typedef(self: *const Self) ?*Type_AST {
    return self.decl.?.decl_typedef();
}

pub fn span(self: *const Self) Span {
    return self.decl.?.token().span;
}

pub fn set_span(self: *Self, _span: Span) void {
    self.decl.?.common()._token.span = _span;
}

pub fn expanded_type(self: *const Self) *Type_AST {
    var expanded = self.type().expand_identifier();
    // context values are represented as their underlying value
    while (expanded.* == .context_type) expanded = expanded.child().expand_identifier();
    return expanded;
}

/// when this is true, this symbol is a type-alias, and should be expanded before use
pub fn is_alias(self: *Self) bool {
    if (self.decl != null and self.decl.?.* == .type_alias) return true;
    return false;
}

pub fn lvalue_is_symbol(self: *Self, return_symbol: *Self) bool {
    return !self.is_temp and // isnt temporary
        self != return_symbol // isnt the function's return value
    ;
}

pub fn err_if_undefined(self: *Self, errors: *errs_.Errors) error{CompileError}!void {
    if (self.kind != .import and !self.defined) {
        errors.add_error(errs_.Error{ .use_before_def = .{ .identifier = Token.init_simple(self.name) } });
        return error.CompileError;
    }
}

/// Throws an `error.CompileError` if a symbol is not used.
pub fn err_if_unused(self: *Self, errors: *errs_.Errors) error{CompileError}!void {
    if (self.kind != .@"const" and self.uses == 0) {
        // TODO: Add a better error for contexts, say `context My_Context is unused` or something
        errors.add_error(errs_.Error{ .symbol_error = .{
            .span = self.span(),
            .context_span = null,
            .name = self.name,
            .problem = "is never used",
            .context_message = "",
        } });
        return error.CompileError;
    }
}

pub fn err_if_undefd(self: *Self, errors: *errs_.Errors, use: Span) error{CompileError}!void {
    // std.debug.print("{s} uses:{} defs:{}\n", .{ self.name, self.uses, self.defs });
    if (self.uses != 0 and // symbol has been used somewhere
        self.defs == 0 and // symbol hasn't been defined anywhere
        self.aliases == 0 and // no aliases anywhere
        !self.param and // symbol isn't a parameter (these don't have defs!)
        self.storage != .@"extern" // symbol isn't an extern (these also don't have defs!)
    ) {
        errors.add_error(errs_.Error{ .symbol_error = .{
            .span = use,
            .context_span = self.span(),
            .name = self.name,
            .problem = "is never defined",
            .context_message = "declared here",
        } });
        return error.CompileError;
    }
}

/// Throws an `error.CompileError` if a symbol is marked `mut`, but is never mutated.
///
/// Symbols are mutated when:
/// - They are the root of at least one Instruction's destination's L-Value tree, OR
/// - They are aliased with `&mut`.
pub fn err_if_var_not_mutated(self: *Self, errors: *errs_.Errors) error{CompileError}!void {
    if (self.kind == .mut and
        self.aliases == 0 and
        self.roots == 0)
    {
        errors.add_error(errs_.Error{ .symbol_error = .{
            .span = self.span(),
            .context_span = null,
            .name = self.name,
            .problem = "is marked `mut` but is never mutated",
            .context_message = "",
        } });
        return error.CompileError;
    }
}

pub fn set_offset(self: *Self, local_offsets: i64) i64 {
    self.offset = local_offsets;
    return @as(i64, @intCast(self.expanded_type().sizeof().?));
}

pub fn represents_method(self: *Self, impl_for_type: *Type_AST, method_name: []const u8) bool {
    return self.decl != null and
        self.decl.?.* == .method_decl and
        self.decl.?.method_decl.impl != null and
        self.decl.?.method_decl.impl.?.impl._type.types_match(impl_for_type) and
        std.mem.eql(u8, self.name, method_name);
}

const Compiler_Context = @import("../hierarchy/compiler.zig");

pub fn monomorphize(
    self: *Self,
    key: std.array_list.Managed(generic_arg_.GenericArg),
    ctx: *Compiler_Context,
) error{ OutOfMemory, CompileError }!*Self {
    // std.debug.print("monomorphize {s} ({*})\n", .{ self.name, self });
    if (key.items.len == 0) {
        // std.debug.print("nothing to do\n", .{});
        return self;
    }

    for (key.items) |k| {
        switch (k) {
            .type_arg => |ty| if (ty.* == .identifier and ty.symbol().?.decl.?.is_typelike_param_decl()) return self,
            .const_arg => |v| if (v.is_const_param_ref()) return self,
        }
    }

    const Symbol_Tree = @import("../ast/symbol-tree.zig");
    const Decorate = @import("../ast/decorate.zig");
    const walker_ = @import("../ast/walker.zig");

    if (self.monomorphs.get(key)) |retval| {
        // std.debug.print("eat slop ({*})\n", .{retval});
        return retval;
    } else {
        // Create a substitution map that subs the param names for the given arg types
        var subst = unification_.Substitutions.init(ctx.allocator());
        defer subst.deinit();
        for (self.decl.?.generic_params().items, key.items) |param, arg| {
            switch (param.*) {
                .type_param_decl => {
                    var to_be_substd = arg.type_arg;
                    if (param.type_param_decl.constraints.items.len > 0) {
                        to_be_substd = Type_AST.create_as_trait(to_be_substd.token(), to_be_substd, param.type_param_decl.constraints, ctx.allocator());
                    }
                    try subst.put_type(param.symbol().?.name, to_be_substd);
                },
                .const_param_decl => {
                    try subst.put_const(param.symbol().?.name, arg.const_arg);
                },
                .context_param_decl => {
                    try subst.put_type(param.symbol().?.name, arg.type_arg);
                },
                else => unreachable,
            }
        }

        // Inline function-local type aliases so references resolve to the monomorph's concrete type, not the shared template alias
        if (self.decl.?.decl_init()) |body| {
            if (body.* == .block) {
                for (body.block._statements.items) |stmt| {
                    if (stmt.* == .type_alias) {
                        if (stmt.type_alias.init) |aliased| {
                            try subst.put_type(stmt.type_alias.name.token().data, aliased.clone(&subst, ctx.allocator()));
                        }
                    }
                }
            }
        }

        try walker_.walk_ast(self.decl.?, Decorate.new(ctx));

        // Clone the decl with the substitution
        // unification_.print_substitutions(&subst);
        const name = anon_name_.next_anon_name(self.name, ctx.allocator());
        const decl = self.decl.?.clone(&subst, ctx.allocator());

        decl.set_generic_params(std.array_list.Managed(*ast_.AST).init(ctx.allocator()));

        const scope = self.decl.?.scope().?.parent.?;

        const symbol_tree_context = Symbol_Tree.new(scope, &ctx.errors, ctx.allocator());
        const decorate_context = Decorate.new(ctx);

        decl.set_decl_name(ast_.AST.create_pattern_symbol(
            Token.init_simple(name),
            self.kind,
            self.storage,
            name,
            ctx.allocator(),
        ));

        try walker_.walk_ast(decl, symbol_tree_context);

        const clone = decl.symbol().?;
        std.debug.assert(clone.cfg == null);
        try self.monomorphs.put(try key.clone(), clone);
        clone.is_monomorphed = true;
        try walker_.walk_ast(decl, decorate_context);

        // std.debug.print("brand new one for ya ({*} aka {s})\n", .{ clone, clone.name });

        return clone;
    }
}
