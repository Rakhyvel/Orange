//! This file defines a generic way to walk over ASTs, applying a walk context to each node.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

pub const Error = error{ OutOfMemory, CompileError };

pub fn Apply_Ast_Walk(Context_Type: type) type {
    return struct {
        const Self: type = @This();
        context: Context_Type,

        pub fn init(context: Context_Type) Self {
            return Self{ .context = context };
        }

        pub fn run(self: *Self, asts: *std.array_list.Managed(*ast_.AST)) Error!*std.array_list.Managed(*ast_.AST) {
            try walk_asts(asts, self.context);
            return asts;
        }
    };
}

pub fn Apply_Flat_Ast_Walk(Context_Type: type) type {
    return struct {
        const Self: type = @This();
        context: Context_Type,

        pub fn init(context: Context_Type) Self {
            return Self{ .context = context };
        }

        pub fn run(self: *Self, asts: *std.array_list.Managed(*ast_.AST)) Error!*std.array_list.Managed(*ast_.AST) {
            try walk_asts_flat(asts, self.context);
            return asts;
        }
    };
}

pub fn walk_asts(asts: *const std.array_list.Managed(*ast_.AST), context: anytype) Error!void {
    for (asts.items) |ast| {
        try walk_ast(ast, context);
    }
}

pub fn walk_asts_flat(asts: *std.array_list.Managed(*ast_.AST), context: anytype) Error!void {
    var i: usize = 0;
    while (i < asts.items.len) : (i += 1) {
        const ast = asts.items[i];
        i += try context.flat(ast, asts, i);
    }
}

// A function that walks over an AST, applying prefix, postfix's function to each one
pub fn walk_ast(maybe_ast: ?*ast_.AST, context: anytype) Error!void {
    if (maybe_ast == null) {
        return;
    }
    const ast = maybe_ast.?;

    var new_context = context;
    if (@hasDecl(@TypeOf(context), "prefix")) {
        const maybe_new_context = try new_context.prefix(ast);
        if (maybe_new_context == null) {
            return;
        } else {
            new_context = maybe_new_context.?;
        }
    }

    switch (ast.*) {
        .unit_value,
        .int,
        .float,
        .string,
        .field,
        .@"unreachable",
        .true,
        .false,
        .@"break",
        .@"continue",
        .poison,
        .pattern_symbol,
        .receiver,
        .identifier,
        .import,
        => {},

        .type_param_decl => try walk_type(ast.type_param_decl.constraint, new_context),

        .context_value_decl => {
            try walk_ast(ast.context_value_decl.init, new_context);
            try walk_type(ast.context_value_decl.parent, new_context);
        },
        .context_decl => try walk_type(ast.decl_typedef(), new_context),

        .struct_decl,
        .enum_decl,
        .type_alias,
        => {
            try walk_asts(ast.generic_params(), new_context);
            try walk_type(ast.decl_typedef(), new_context);
        },

        .module => {}, // std.debug.panic("compiler error: walking over modules not implemented!\n", .{}),

        .size_of, .default => try walk_type(ast.type(), new_context),

        .not,
        .negate,
        .dereference,
        .@"try",
        .@"comptime",
        .addr_of,
        .slice_of,
        .dyn_value,
        .bit_not,
        .cinclude,
        => try walk_ast(ast.expr(), new_context),

        .as => {
            try walk_ast(ast.expr(), new_context);
            try walk_type(ast.type(), new_context);
        },

        .assign,
        .@"or",
        .@"and",
        .add,
        .sub,
        .mult,
        .div,
        .mod,
        .equal,
        .not_equal,
        .greater,
        .lesser,
        .greater_equal,
        .lesser_equal,
        .@"catch",
        .@"orelse",
        .select,
        .access,
        .left_shift,
        .right_shift,
        => {
            try walk_ast(ast.lhs(), new_context);
            try walk_ast(ast.rhs(), new_context);
        },
        .invoke => {
            try walk_ast(ast.lhs(), new_context);
            try walk_ast(ast.rhs(), new_context);
            try walk_asts(ast.children(), new_context);
        },
        .call, .index => {
            try walk_ast(ast.lhs(), new_context);
            try walk_asts(ast.children(), new_context);
        },
        .generic_apply => {
            try walk_ast(ast.lhs(), new_context);
            try walk_types(&ast.generic_apply._children, new_context);
        },

        .context_value => {
            try walk_asts(ast.children(), new_context);
            try walk_type(ast.context_value.parent, new_context);
        },

        .struct_value,
        .tuple_value,
        .array_value,
        .print,
        .bit_and,
        .bit_or,
        .bit_xor,
        => try walk_asts(ast.children(), new_context),

        .write => {
            try walk_ast(ast.write.writer, new_context);
            try walk_asts(ast.children(), new_context);
        },

        .binding => {
            try walk_asts(ast.children(), new_context);
            try walk_ast(ast.binding.init, new_context);
            try walk_type(ast.binding.type, new_context);
        },
        .enum_value => {
            try walk_ast(ast.enum_value.init, new_context);
            if (ast.enum_value.base != null) {
                try walk_type(ast.enum_value.base, new_context);
            }
        },
        .sub_slice => {
            try walk_ast(ast.sub_slice.super, new_context);
            try walk_ast(ast.sub_slice.lower, new_context);
            try walk_ast(ast.sub_slice.upper, new_context);
        },
        .@"if" => {
            try walk_ast(ast.@"if".let, new_context);
            try walk_ast(ast.@"if".condition, new_context);
            try walk_ast(ast.body_block(), new_context);
            try walk_ast(ast.else_block(), new_context);
        },
        .match => {
            try walk_ast(ast.match.let, new_context);
            try walk_ast(ast.expr(), new_context);
            try walk_asts(ast.children(), new_context);
        },
        .mapping => {
            try walk_ast(ast.lhs(), new_context);
            try walk_ast(ast.rhs(), new_context);
            try walk_asts(&ast.mapping.captures, new_context);
        },
        .@"while" => {
            try walk_ast(ast.@"while".let, new_context);
            try walk_ast(ast.@"while".condition, new_context);
            try walk_ast(ast.@"while".post, new_context);
            try walk_ast(ast.body_block(), new_context);
            try walk_ast(ast.else_block(), new_context);
        },
        .@"for" => {
            try walk_ast(ast.@"for".let, new_context);
            try walk_ast(ast.@"for".elem, new_context);
            try walk_ast(ast.@"for".iterable, new_context);
            try walk_ast(ast.body_block(), new_context);
            try walk_ast(ast.else_block(), new_context);
        },
        .with => {
            try walk_asts(&ast.with.contexts, new_context);
            try walk_ast(ast.with._body_block, new_context);
        },
        .block => {
            try walk_asts(ast.children(), new_context);
            if (ast.block.final) |final| {
                try walk_ast(final, new_context);
            }
        },
        .@"return" => try walk_ast(ast.@"return"._ret_expr, new_context),
        .decl => {
            try walk_type(ast.decl.type, new_context);
            if (!(ast.decl.init != null and ast.decl.init.?.* == .default and ast.decl.init.?.default._type == ast.decl.type)) {
                // Don't double-walk when decl's init is a default of its type
                try walk_ast(ast.decl.init, new_context);
            }
        },
        .fn_decl => {
            try walk_asts(&ast.fn_decl.context_decls, new_context);
            try walk_asts(ast.generic_params(), new_context);
            try walk_type(ast.fn_decl._decl_type, new_context);
            try walk_ast(ast.fn_decl.init, new_context);
            try walk_asts(ast.children(), new_context);
            try walk_type(ast.fn_decl.ret_type, new_context);
        },
        .@"test" => {
            try walk_asts(&ast.@"test".context_decls, new_context);
            try walk_ast(ast.@"test".name, new_context);
            try walk_ast(ast.@"test".init, new_context);
        },
        .trait => {
            try walk_asts(&ast.trait.method_decls, new_context);
            try walk_asts(&ast.trait.const_decls, new_context);
        },
        .impl => {
            try walk_asts(ast.generic_params(), new_context);
            try walk_type(ast.impl._type, new_context);
            try walk_ast(ast.impl.trait, new_context);
            try walk_asts(&ast.impl.method_defs, new_context);
            try walk_asts(&ast.impl.const_defs, new_context);
        },
        .method_decl => {
            try walk_asts(&ast.method_decl.context_decls, new_context);
            try walk_ast(ast.method_decl.init, new_context);
            try walk_asts(ast.children(), new_context);
            try walk_type(ast.method_decl._decl_type, new_context);
            try walk_type(ast.method_decl.ret_type, new_context);
        },
        .@"defer", .@"errdefer" => try walk_ast(ast.statement(), new_context),
    }

    if (@hasDecl(@TypeOf(context), "postfix")) {
        try context.postfix(ast);
    }
}

pub fn walk_types(types: *const std.array_list.Managed(*Type_AST), context: anytype) Error!void {
    for (types.items) |_type| {
        try walk_type(_type, context);
    }
}

pub fn walk_types_flat(types: *std.array_list.Managed(*Type_AST), context: anytype) Error!void {
    var i: usize = 0;
    while (i < types.items.len) : (i += 1) {
        const _type = types.items[i];
        i += try context.flat_type(_type, types, i);
    }
}

// A function that walks over an AST, applying prefix, postfix's function to each one
pub fn walk_type(maybe_type: ?*Type_AST, context: anytype) Error!void {
    if (maybe_type == null) {
        return;
    }
    const _type = maybe_type.?;

    var new_context = context;
    if (@hasDecl(@TypeOf(context), "prefix_type")) {
        const maybe_new_context = try context.prefix_type(_type);
        if (maybe_new_context == null) {
            return;
        } else {
            new_context = maybe_new_context.?;
        }
    }

    switch (_type.*) {
        .poison, .anyptr_type, .unit_type, .identifier => {},

        .type_of => try walk_ast(_type.type_of._expr, new_context),

        .access => try walk_ast(_type.access.inner_access, new_context),

        .generic_apply => {
            try walk_type(_type.lhs(), new_context);
            try walk_types(_type.children(), new_context);
        },

        .array_of => {
            try walk_type(_type.child(), new_context);
            try walk_ast(_type.array_of.len, new_context);
        },

        .annotation => {
            try walk_type(_type.child(), new_context);
            try walk_ast(_type.annotation.init, new_context);
        },

        .untagged_sum_type,
        .addr_of,
        .dyn_type,
        .index,
        .domain_of,
        => {
            try walk_type(_type.child(), new_context);
        },

        .function => {
            try walk_type(_type.lhs(), new_context);
            try walk_type(_type.rhs(), new_context);
            try walk_types(&_type.function.contexts, new_context);
        },

        .context_type,
        .enum_type,
        .struct_type,
        .tuple_type,
        => {
            try walk_types(_type.children(), new_context);
        },
    }

    if (@hasDecl(@TypeOf(context), "postfix_type")) {
        try context.postfix_type(_type);
    }
}
