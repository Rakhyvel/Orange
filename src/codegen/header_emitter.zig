// This file contains the implementation of the Orange compiler's C code generator.
// TODO: Make this a context struct (to fix self.module)

const std = @import("std");
const CFG = @import("../ir/cfg.zig");
const Emitter = @import("emitter.zig");
const module_ = @import("../hierarchy/module.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Type_Set = @import("../types/type_set.zig");
const Dependency_Node = @import("../types/dependency_node.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Canonical_Type_Fmt = @import("../types/canonical_type_fmt.zig");

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
    try self.output_variant_names();
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
        \\#include "debug.h"
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
        try self.output_typedef_include(dag);
    }
}

/// Outputs a typedef declaration based on the provided `DAG`.
fn output_typedef_include(self: *Self, dep: *Dependency_Node) CodeGen_Error!void {
    // FIXME: High Cyclo
    if (dep.visited) {
        // only visit a DAG node once
        return;
    }
    dep.mark_visited();

    // output any types that this type depends on
    for (dep.dependencies.items) |depen| {
        try self.output_typedef_include(depen);
    }

    if (dep.base.sizeof()) |size| {
        // Do not output zero-sized types
        if (size == 0) return;
    }

    var str = try Canonical_Type_Fmt.canonical_rep(dep.base);
    defer str.deinit();
    try self.writer.print("#include \"types/{f}.h\"\n", .{Canonical_Type_Fmt{ .type = dep.base }});
}

fn output_traits(self: *Self) CodeGen_Error!void {
    // FIXME: High Cyclo
    if (self.module.traits.keys().len > 0) {
        // Do not output header comment if there are no traits!
        try self.writer.print("\n/* Trait vtable type definitions */\n", .{});
    }

    for (self.module.traits.keys()) |trait| {
        // TODO: Too long
        if (trait.trait.num_virtual_methods == 0 and trait.trait.super_traits.items.len == 0) {
            continue;
        }
        if (trait.num_generic_params() > 0) {
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
            const has_params = num_method_params + @intFromBool(method_decl_has_receiver) == 0;
            const has_contexts = decl.method_decl.context_decls.items.len > 0;

            try self.writer.print("    ", .{});
            try self.emitter.output_type(decl.method_decl.ret_type);
            try self.writer.print("(*_{s})(", .{decl.method_decl.name.token().data});

            // Output receiver parameter
            if (method_decl_has_receiver) {
                try self.writer.print("void *", .{});
                if ((decl.children().items.len > 0 and Emitter.is_storable(decl.children().items[0].binding.type)) or has_contexts) {
                    try self.writer.print(", ", .{});
                }
            }

            // Output regular parameters
            for (decl.children().items, 0..) |param_decl, i| {
                if (Emitter.is_storable(param_decl.binding.type)) {
                    // Do not output `void` parameters
                    try self.emitter.output_type(param_decl.binding.type);
                    if ((i + 1 < num_method_params and Emitter.is_storable(decl.children().items[i + 1].binding.type)) or has_contexts) {
                        try self.writer.print(", ", .{});
                    }
                }
            }

            // Output context parameters
            for (decl.method_decl.context_decls.items) |param_decl| {
                if (has_params) {
                    try self.writer.print(", ", .{});
                }
                // Do not output `void` parameters
                try self.emitter.output_type(param_decl.context_value_decl.parent);
            }

            if (has_params) {
                // If there are no parameters and no receiver, mark parameter list as void
                try self.writer.print("void", .{});
            }
            try self.writer.print(");\n", .{});
        }
        for (trait.trait.super_traits.items, 0..) |super_trait, i| {
            const super_trait_symbol = super_trait.symbol().?;
            const super_trait_decl = super_trait_symbol.decl.?;
            if (super_trait_decl.trait.num_virtual_methods == 0 and super_trait_decl.trait.super_traits.items.len == 0) continue;
            try self.writer.print("    const struct vtable_{s}__{s}__{}_{s} *_{};\n", .{
                super_trait_symbol.scope.module.?.package_name,
                super_trait_symbol.scope.module.?.name(),
                super_trait_symbol.scope.uid,
                super_trait_symbol.name,
                i,
            });
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
        try self.writer.print("\n/* Trait vtable implementation declarations */\n", .{});
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

fn output_variant_names(self: *Self) CodeGen_Error!void {
    if (self.module.enums.keys().len > 0) {
        try self.writer.print("\n/* Enum variant name table declarations */\n", .{});
    }
    for (self.module.enums.keys()) |enum_decl| {
        try self.writer.print("extern const ", .{});
        try self.emitter.output_type(prelude_.string_type);
        try self.writer.print(" ", .{});
        try self.emitter.output_symbol(enum_decl.symbol().?);
        try self.writer.print("__{}_variant_names[{}];\n", .{ enum_decl.symbol().?.scope.uid, enum_decl.enum_decl.fields.items.len });
    }
}

/// Outputs the forward declaration of a function.
pub fn output_forward_function(self: *Self, cfg: *CFG) CodeGen_Error!void {
    if (cfg.symbol.is_monomorphed) {
        return;
    }
    try self.emitter.output_function_prototype(cfg);
    try self.writer.print(";\n", .{});
}
