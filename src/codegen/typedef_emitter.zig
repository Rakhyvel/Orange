// This file contains the implementation of the Orange compiler's C code generator.

const std = @import("std");
const Emitter = @import("emitter.zig");
const Dependency_Node = @import("../types/dependency_node.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Canonical_Type_Fmt = @import("../types/canonical_type_fmt.zig");

const Self = @This();

emitter: Emitter,
dep: *Dependency_Node,
writer: *std.array_list.Managed(u8),

pub const CodeGen_Error = error{ WriteFailed, OutOfMemory };

pub fn init(dep: *Dependency_Node, writer: *std.array_list.Managed(u8)) Self {
    const emitter = Emitter.init(writer);
    return Self{ .emitter = emitter, .dep = dep, .writer = writer };
}

/// Generates the header file for the given module
pub fn generate(self: *Self) CodeGen_Error!void {
    try self.output_include_guard_begin();
    try self.output_dependencies();
    try self.output_typedef();
    try self.output_include_guard_end();
}

fn output_include_guard_begin(self: *Self) CodeGen_Error!void {
    try self.writer.print(
        \\/* Code generated using the Orange compiler http://ornglang.org */
        \\
        \\#ifndef {0f}_H
        \\#define {0f}_H
        \\
        \\
    , .{Canonical_Type_Fmt{ .type = self.dep.base, .case = .upper }});
}

fn output_dependencies(self: *Self) CodeGen_Error!void {
    for (self.dep.dependencies.items) |depen| {
        switch (depen.base.*) {
            .struct_type,
            .context_type,
            .tuple_type,
            .array_of,
            .enum_type,
            .dyn_type,
            => {
                try self.writer.print("struct {f};\n", .{Canonical_Type_Fmt{ .type = depen.base }});
            },
            .untagged_sum_type => try self.writer.print("union {f};\n", .{Canonical_Type_Fmt{ .type = depen.base }}),
            else => {},
        }
    }
    if (self.dep.dependencies.items.len > 0) {
        try self.writer.print("\n", .{});
    }
}

fn output_include_guard_end(self: *Self) CodeGen_Error!void {
    try self.writer.print(
        \\#endif
        \\
    , .{});
}

/// Outputs a typedef declaration based on the provided `DAG`.
fn output_typedef(self: *Self) CodeGen_Error!void {
    if (self.dep.base.* == .function) {
        const has_contexts = self.dep.base.function.contexts.items.len > 0;
        const has_params = self.dep.base.function.args.items.len > 0;

        try self.writer.print("typedef ", .{});
        try self.emitter.output_type(self.dep.base.rhs());
        try self.writer.print("(*{f})(", .{Canonical_Type_Fmt{ .type = self.dep.base }});

        for (self.dep.base.function.args.items, 0..) |arg, i| {
            if (!arg.is_c_void_type()) {
                // Do not output `void` parameters
                try self.emitter.output_type(arg);
                try self.writer.print("/*{t}*/ ", .{arg.*});
                if (i + 1 < self.dep.base.function.args.items.len and !self.dep.base.function.args.items[i + 1].is_c_void_type()) {
                    try self.writer.print(", ", .{});
                }
            }
        }

        if (has_contexts) {
            if (has_params) {
                try self.writer.print(", ", .{});
            }
            for (self.dep.base.function.contexts.items, 0..) |ctx, i| {
                try self.emitter.output_type(ctx);
                if (i + 1 < self.dep.base.function.contexts.items.len) {
                    try self.writer.print(", ", .{});
                }
            }
        }
        try self.writer.print(");\n\n", .{});
    } else if (self.dep.base.* == .struct_type or self.dep.base.* == .tuple_type or self.dep.base.* == .context_type) {
        try self.writer.print("struct {f} {{\n", .{Canonical_Type_Fmt{ .type = self.dep.base }});
        try self.output_field_list(self.dep.base.children(), 4);
        try self.writer.print("}};\n\n", .{});
    } else if (self.dep.base.* == .array_of) {
        try self.writer.print("struct {f} {{\n    ", .{Canonical_Type_Fmt{ .type = self.dep.base }});
        try self.emitter.output_type(self.dep.base.child());
        try self.writer.print(" _0[{}];\n", .{self.dep.base.array_of.len.int.data});
        try self.writer.print("}};\n\n", .{});
    } else if (self.dep.base.* == .enum_type) {
        try self.writer.print("struct {f} {{\n    uint64_t tag;\n", .{Canonical_Type_Fmt{ .type = self.dep.base }});
        if (!self.dep.base.enum_type.is_all_unit()) {
            try self.writer.print("    union {{\n", .{});
            try self.output_field_list(self.dep.base.children(), 8);
            try self.writer.print("    }};\n", .{});
        }
        try self.writer.print("}};\n\n", .{});
    } else if (self.dep.base.* == .untagged_sum_type) {
        try self.writer.print("union {f} {{\n", .{Canonical_Type_Fmt{ .type = self.dep.base }});
        if (!self.dep.base.child().expand_identifier().enum_type.is_all_unit()) {
            try self.output_field_list(self.dep.base.children(), 4);
        }
        try self.writer.print("}};\n\n", .{});
    } else if (self.dep.base.* == .dyn_type) {
        const trait_symbol = self.dep.base.child().symbol().?;
        try self.writer.print("struct {f} {{\n    void* data_ptr;\n    const struct vtable_{s}__{s}__{}_{s}* vtable;\n}};\n\n", .{
            Canonical_Type_Fmt{ .type = self.dep.base },
            trait_symbol.scope.module.?.package_name,
            trait_symbol.scope.module.?.name(),
            trait_symbol.scope.uid,
            trait_symbol.name,
        });
    } else {
        std.debug.panic("cannot output typedef for typekind {t}", .{self.dep.base.*});
    }
}

/// Outputs the fields of a structure or union type based on the provided list of AST types.
fn output_field_list(self: *Self, fields: *const std.array_list.Managed(*Type_AST), spaces: usize) CodeGen_Error!void {
    // output each field in the list
    for (fields.items, 0..) |term, i| {
        if (!term.is_c_void_type()) {
            // Don't gen `void` structure/union fields
            for (0..spaces) |_| {
                try self.writer.print(" ", .{});
            }
            try self.emitter.output_type(term);
            try self.writer.print(" _{};\n", .{i});
        }
    }
}
