const std = @import("std");
const AST = @import("../ast/ast.zig").AST;
const CFG = @import("../ir/cfg.zig");
const core_ = @import("../hierarchy/core.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Type_Map = @import("../types/type_map.zig").Type_Map;
const Symbol = @import("../symbol/symbol.zig");
const Canonical_Type_Fmt = @import("../types/canonical_type_fmt.zig");

pub const CodeGen_Error = error{ OutOfMemory, WriteFailed };

const Self: type = @This();

writer: *std.array_list.Managed(u8),

pub fn init(writer: *std.array_list.Managed(u8)) Self {
    return Self{
        .writer = writer,
    };
}

/// Outputs the C type which corresponds to an AST type.
pub fn output_type(self: *Self, old_type: *Type_AST) CodeGen_Error!void {
    if (old_type.unexpand_type() != old_type) {
        return try self.output_type(old_type.unexpand_type());
    }
    if (old_type.* == .identifier and old_type.symbol() != null and old_type.symbol().?.storage == .@"extern") {
        try self.writer.print("{s}", .{old_type.symbol().?.storage.@"extern".c_name.?.string.data});
        return;
    }
    var _type = old_type.expand_identifier();

    switch (_type.*) {
        .identifier => if (_type.common()._expanded_type != null and _type.common()._expanded_type.? != _type) {
            try self.output_type(_type.common()._expanded_type.?);
        } else {
            const info = prelude_.info_from_name(_type.token().data);
            if (info != null) {
                try self.writer.print("{s}", .{info.?.c_name});
            } else {
                try self.writer.print(
                    "{s} /*{?f}*/",
                    .{ _type.token().data, _type.symbol().?.init_typedef() },
                );
            }
        },
        .addr_of => {
            try self.output_type(_type.child());
            try self.writer.print(" *", .{});
        },
        .anyptr_type => try self.writer.print("void *", .{}),
        .unit_type => try self.writer.print("void", .{}),
        .annotation => try self.output_type(_type.child()),

        .function => try self.writer.print("{f}", .{Canonical_Type_Fmt{ .type = _type }}),
        .enum_type,
        .tuple_type,
        .struct_type,
        .array_of,
        .dyn_type,
        .context_type,
        => try self.writer.print("struct {f}", .{Canonical_Type_Fmt{ .type = _type }}),
        .untagged_sum_type => try self.writer.print("union {f}", .{Canonical_Type_Fmt{ .type = _type }}),
        else => std.debug.panic("compiler error: unimplemented output_type() for {f}", .{_type.*}),
    }
}

/// Applies a function to all CFGs in a list of CFGs.
///
/// ### Params:
/// - `self`: The Emitter object
/// - `sub`: Reference back to the sub-emitter (ie source, header, type, etc)
/// - `cfgs`: Slice of CFGs
/// - `header_comment`: Header comment to output, if `cfgs` is non-empy
/// - `f`: Function to apply to each CFG in `cfgs`
pub fn forall_functions(
    self: *Self,
    sub: anytype,
    cfgs: []const *CFG,
    header_comment: []const u8,
    comptime f: fn (@TypeOf(sub), *CFG) CodeGen_Error!void,
) CodeGen_Error!void {
    if (cfgs.len > 0) {
        try self.writer.print("\n/* {s} */\n", .{header_comment});
    }

    // apply the function `f` to all CFGs in the `cfgs` list
    for (cfgs) |cfg| {
        if (!cfg.needed_at_runtime) {
            continue;
        }
        if (cfg.symbol.decl.?.* == .fn_decl or cfg.symbol.decl.?.* == .method_decl) { // Don't output for `_comptime` decls
            try f(sub, cfg);
        }
    }
}

/// Outputs the prototype of a function
pub fn output_function_prototype(
    self: *Self,
    cfg: *CFG, // TODO: Accept cfg symbol
) CodeGen_Error!void {
    // Output function return type and name
    try self.output_type(cfg.symbol.expanded_type().rhs());
    try self.writer.print(" ", .{});
    try self.output_symbol(cfg.symbol);

    // Output function parameters
    const decl = cfg.symbol.decl.?;
    const param_symbols = decl.param_symbols();
    const context_param_symbols = decl.context_param_symbols();

    const has_params = param_symbols != null and param_symbols.?.items.len > 0;
    const has_contexts = context_param_symbols != null and context_param_symbols.?.items.len > 0;

    var num_non_unit_params: i64 = 0;
    try self.writer.print("(", .{});
    if (param_symbols != null) {
        for (param_symbols.?.items, 0..) |term, i| {
            if (!term.expanded_type().is_c_void_type()) {
                if (decl.* == .method_decl and decl.method_decl.receiver != null and i == 0) {
                    // Print out method receiver
                    try self.writer.print("void *", .{});
                    try self.output_symbol(term);
                } else {
                    // Print out parameter declarations
                    try self.output_var_decl(term, true);
                }
                if (i + 1 < param_symbols.?.items.len and !param_symbols.?.items[i + 1].expanded_type().is_c_void_type()) {
                    try self.writer.print(", ", .{});
                }
                num_non_unit_params += 1;
            }
        }
    }
    if (context_param_symbols != null) {
        if (has_contexts and has_params) {
            try self.writer.print(", ", .{});
        }
        for (context_param_symbols.?.items, 0..) |term, i| {
            try self.output_var_decl(term, true);
            if (i + 1 < context_param_symbols.?.items.len and !context_param_symbols.?.items[i + 1].expanded_type().is_c_void_type()) {
                try self.writer.print(", ", .{});
            }
            num_non_unit_params += 1;
        }
    }

    if (num_non_unit_params == 0) {
        // If there are no parameters, mark parameter list as void
        try self.writer.print("void", .{});
    }
    try self.writer.print(")", .{});
}

/// Outputs a symbol's declaration. Parameters are not formatted with a preceding tab, nor a
/// semicolon nor newline.
pub fn output_var_decl(
    self: *Self,
    symbol: *Symbol,
    is_parameter: bool, // TODO: New function, that surrounds output_var_decl
) CodeGen_Error!void {
    if (!is_parameter) {
        try self.writer.print("    ", .{});
    }
    try self.output_type(symbol.type());
    var str = try Canonical_Type_Fmt.canonical_rep(symbol.type());
    defer str.deinit();
    try self.writer.print(" /*{s}*/ ", .{str.str()});
    try self.output_symbol(symbol);
    if (!is_parameter) {
        try self.writer.print(";\n", .{});
    }
}

/// Outputs a symbol with it's file's unique identifier
pub fn output_symbol(self: *Self, symbol: *Symbol) CodeGen_Error!void {
    if (symbol.storage == .@"extern") {
        try self.writer.print("{s}", .{symbol.storage.@"extern".c_name.?.string.data});
    } else if (symbol.decl != null and symbol.decl.?.* != .receiver and symbol.decl.?.top_level()) {
        if (symbol.decl.?.* == .method_decl or !symbol.decl.?.top_level()) {
            try self.writer.print("{s}__{s}__{}_{s}", .{ symbol.scope.module.?.package_name, symbol.scope.module.?.name(), symbol.scope.uid, symbol.name });
        } else {
            try self.writer.print("{s}__{s}__{s}", .{ symbol.scope.module.?.package_name, symbol.scope.module.?.name(), symbol.name });
        }
    } else {
        // The leading underscore here should be OK, C spec allows leading underscores for auto storage variables
        try self.writer.print("_{}_{s}", .{ symbol.scope.uid, symbol.name });
    }
}

pub fn output_context_includes(self: *Self, contexts_used: *Type_Map(void)) CodeGen_Error!void {
    for (contexts_used.pairs.items) |pair| {
        var ctx = pair.key.child();
        if (ctx.types_match(core_.allocating_context)) {
            try self.writer.print("#include \"alloc.inc\"\n", .{});
        } else if (ctx.types_match(core_.io_context)) {
            try self.writer.print("#include \"io.inc\"\n", .{});
        } else if (ctx.types_match(core_.args_context)) {
            try self.writer.print("#include \"args.inc\"\n", .{});
        } else if (ctx.types_match(core_.file_io_context)) {
            try self.writer.print("#include \"file_io.inc\"\n", .{});
        }
    }
}

pub fn output_context_defs(self: *Self, contexts_used: *Type_Map(void)) CodeGen_Error!void {
    for (contexts_used.pairs.items) |pair| {
        const ctx = pair.key.child();
        if (ctx.types_match(core_.allocating_context)) {
            try self.output_type(core_.allocating_context);
            try self.writer.print(
                \\ allocator_context = {{._0 = {{.data_ptr = NULL, .vtable = &orange__core__allocator_vtable}}}};
                \\    
            , .{});
        } else if (ctx.types_match(core_.io_context)) {
            try self.output_type(core_.io_context);
            try self.writer.print(
                \\ io_context = {{._0 = {{.data_ptr = NULL, .vtable = &orange__core__writer_vtable}}, ._1 = {{.data_ptr = NULL, .vtable = &orange__core__reader_vtable}}}};
                \\    
            , .{});
        } else if (ctx.types_match(core_.args_context)) {
            try self.output_type(core_.args_context);
            try self.writer.print(
                \\ args_context = orange__core__convert_args(argc, argv);
                \\    
            , .{});
        } else if (ctx.types_match(core_.file_io_context)) {
            try self.output_type(core_.file_io_context);
            try self.writer.print(
                \\ file_io_context = {{._0 = {{.data_ptr = NULL, .vtable = &orange__core__file_system_vtable}}}};
                \\    
            , .{});
        }
    }
}

pub fn output_context_args(self: *Self, contexts: []const *Type_AST) CodeGen_Error!void {
    for (contexts, 0..) |ctx, i| {
        std.debug.assert(ctx.* == .addr_of);
        if (ctx.child().types_match(core_.allocating_context)) {
            try self.writer.print("&allocator_context", .{});
        } else if (ctx.child().types_match(core_.io_context)) {
            try self.writer.print("&io_context", .{});
        } else if (ctx.child().types_match(core_.args_context)) {
            try self.writer.print("&args_context", .{});
        } else if (ctx.child().types_match(core_.file_io_context)) {
            try self.writer.print("&file_io_context", .{});
        }
        if (i + 1 < contexts.len) {
            try self.writer.print(", ", .{});
        }
    }
}
