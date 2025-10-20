// This file contains the implementation of the Orange compiler's C code generator.
// TODO: Make this a context struct (to fix self.module)

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Basic_Block = @import("../ir/basic-block.zig");
const CFG = @import("../ir/cfg.zig");
const Emitter = @import("emitter.zig");
const Instruction = @import("../ir/instruction.zig");
const lval_ = @import("../ir/lval.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const module_ = @import("../hierarchy/module.zig");
const Span = @import("../util/span.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Type_Set = @import("../ast/type-set.zig");
const Dependency_Node = @import("../ast/dependency_node.zig");
const Symbol = @import("../symbol/symbol.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Canonical_Type_Fmt = @import("canonical_type_fmt.zig");

const Self = @This();

module: *module_.Module,
emitter: Emitter,
writer: *std.array_list.Managed(u8),

pub const CodeGen_Error = error{ WriteFailed, OutOfMemory };

pub fn init(module: *module_.Module, writer: *std.array_list.Managed(u8)) Self {
    const emitter = Emitter.init(writer);
    return Self{ .module = module, .emitter = emitter, .writer = writer };
}

/// Generates the header file for the given module
pub fn generate(self: *Self) CodeGen_Error!void {
    try self.output_include_guard_begin();
    try self.output_common_includes();
    try self.output_includes();
    try self.output_typedefs();
    try self.output_traits();
    try self.output_impls();
    try self.emitter.forall_functions(self, self.module.cfgs.items, "Function declarations", output_forward_function);
    try self.output_include_guard_end();
}

pub fn output_include_guard_begin(self: *Self) CodeGen_Error!void {
    var guard_macro = String.init(std.heap.page_allocator);
    defer guard_macro.deinit();

    var guard_macro_writer = guard_macro.writer(&.{});
    const guard_macro_writer_intfc = &guard_macro_writer.interface;
    try guard_macro_writer_intfc.print("{s}__{s}_H", .{ self.module.package_name, self.module.name() });

    guard_macro.toUppercase();

    try self.writer.print(
        \\/* Code generated using the Orange compiler http://ornglang.org */
        \\
        \\#ifndef {0s}
        \\#define {0s}
    , .{guard_macro.str()});
}

pub fn output_common_includes(self: *Self) CodeGen_Error!void {
    try self.writer.print(
        \\
        \\
        \\#include <stdio.h>
        \\#include <stdint.h>
        \\#include <stdlib.h>
        \\
        \\#include "debug.inc"
        \\
    , .{});
}

pub fn output_include_guard_end(self: *Self) CodeGen_Error!void {
    try self.writer.print(
        \\
        \\#endif
        \\
    , .{});
}

pub fn output_includes(self: *Self) CodeGen_Error!void {
    if (self.module.cincludes.items.len != 0) {
        try self.writer.print("\n/* Special module includes */\n", .{});
        for (self.module.cincludes.items) |cinclude| {
            try self.writer.print("#include \"{s}\"\n", .{cinclude.string.data});
        }
    }

    if (self.module.local_imported_modules.keys().len != 0) {
        try self.writer.print("\n/* Module imports */\n", .{});
        for (self.module.local_imported_modules.keys()) |module| {
            try self.writer.print("#include \"{s}-{s}.h\"\n", .{ module.package_name, module.name() });
        }
    }
}

/// Outputs typedefs based on the provided `Type_Set`.
fn output_typedefs(self: *Self) CodeGen_Error!void {
    if (self.module.type_set.types.items.len > 0) {
        // Don't generate typedefs header comment if there are no typedefs!
        try self.writer.print("\n/* Type definitions */\n", .{});
    }

    // Output all typedefs
    for (self.module.type_set.types.items) |dag| {
        try self.output_typedef(dag);
    }
}

/// Outputs a typedef declaration based on the provided `DAG`.
fn output_typedef(self: *Self, dep: *Dependency_Node) CodeGen_Error!void {
    // FIXME: High Cyclo
    if (dep.visited) {
        // only visit a DAG node once
        return;
    }
    dep.mark_visited();

    // output any types that this type depends on
    for (dep.dependencies.items) |depen| {
        try self.output_typedef(depen);
    }

    if (dep.base.sizeof() == 0) {
        // Do not output zero-sized types
        return;
    }

    var str = try Canonical_Type_Fmt.canonical_rep(dep.base);
    defer str.deinit();
    try self.writer.print("#include \"types/{f}.h\"\n", .{Canonical_Type_Fmt{ .type = dep.base }});
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

fn output_traits(self: *Self) CodeGen_Error!void {
    // FIXME: High Cyclo
    if (self.module.traits.items.len > 0) {
        // Do not output header comment if there are no traits!
        try self.writer.print("/* Trait vtable type definitions */\n", .{});
    }

    for (self.module.traits.items) |trait| {
        // TODO: Too long
        if (trait.trait.num_virtual_methods == 0) {
            continue;
        }
        try self.writer.print("struct vtable_{s}__{s}__{}_{s} {{\n", .{
            self.module.package_name,
            self.module.name(),
            trait.symbol().?.scope.uid,
            trait.symbol().?.name,
        });
        for (trait.trait.method_decls.items) |decl| {
            if (!decl.method_decl.is_virtual) {
                continue;
            }
            const method_decl_has_receiver = decl.method_decl.receiver != null;
            const num_method_params = decl.children().items.len;

            try self.writer.print("    ", .{});
            try self.emitter.output_type(decl.method_decl.ret_type);
            try self.writer.print("(*{s})(", .{decl.method_decl.name.token().data});

            // Output receiver parameter
            if (method_decl_has_receiver) {
                try self.writer.print("void *", .{});
                if (decl.children().items.len > 0 and !decl.children().items[0].binding.type.is_c_void_type()) {
                    try self.writer.print(", ", .{});
                }
            }

            // Output regular parameters
            for (decl.children().items, 0..) |param_decl, i| {
                if (!param_decl.binding.type.is_c_void_type()) {
                    // Do not output `void` parameters
                    try self.emitter.output_type(param_decl.binding.type);
                    if (i + 1 < num_method_params and !decl.children().items[i + 1].binding.type.is_c_void_type()) {
                        try self.writer.print(", ", .{});
                    }
                }
            }
            if (num_method_params + @intFromBool(method_decl_has_receiver) == 0) {
                // If there are no parameters and no receiver, mark parameter list as void
                try self.writer.print("void", .{});
            }
            try self.writer.print(");\n", .{});
        }
        try self.writer.print("}};\n\n", .{});
    }
}

fn output_impls(self: *Self) CodeGen_Error!void {
    var vtable_count: usize = 0;
    for (self.module.impls.items) |impl| {
        if (impl.impl.num_virtual_methods != 0) {
            vtable_count += 1;
        }
    }

    if (vtable_count > 0) {
        // TODO: Count impls that have virtual methods
        // Do not output header comment if there are no impls!
        try self.writer.print("/* Trait vtable implementation declarations */\n", .{});
    }

    for (self.module.impls.items) |impl| {
        if (impl.impl.num_virtual_methods == 0) {
            continue;
        }
        const trait = impl.impl.trait.?;
        const trait_module = trait.symbol().?.scope.module.?;
        try self.writer.print("extern const struct vtable_{s}__{s}__{}_{s} {s}__{s}_{}__vtable;\n", .{
            trait_module.package_name,
            trait_module.name(),
            trait.symbol().?.scope.uid,
            trait.symbol().?.name,
            //
            self.module.package_name,
            self.module.name(),
            impl.scope().?.uid,
        });
    }
}

/// Outputs the forward declaration of a function.
pub fn output_forward_function(self: *Self, cfg: *CFG) CodeGen_Error!void {
    try self.emitter.output_function_prototype(cfg);
    try self.writer.print(";\n", .{});
}
