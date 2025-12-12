// This file contains the implementation of the Orange compiler's C code generator.
// TODO: Make this a context struct (to fix cheat_module)

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const Basic_Block = @import("../ir/basic-block.zig");
const core_ = @import("../hierarchy/core.zig");
const CFG = @import("../ir/cfg.zig");
const Emitter = @import("emitter.zig");
const Interned_String_Set = @import("../ir/interned_string_set.zig");
const Instruction = @import("../ir/instruction.zig");
const lval_ = @import("../ir/lval.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const module_ = @import("../hierarchy/module.zig");
const Span = @import("../util/span.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const Type_Map = @import("../types/type_map.zig").Type_Map;
const Symbol = @import("../symbol/symbol.zig");

const Self = @This();

module: *module_.Module,
/// Interned strings for this module. Referenced when emitting a load_string instruction to retrieve the string information.
module_interned_strings: *const std.AutoArrayHashMap(u32, *Interned_String_Set),
emitter: Emitter,
writer: *std.array_list.Managed(u8),

pub const CodeGen_Error = error{ WriteFailed, OutOfMemory };

pub fn init(
    module: *module_.Module,
    module_interned_strings: *const std.AutoArrayHashMap(u32, *Interned_String_Set),
    writer: *std.array_list.Managed(u8),
) Self {
    const emitter = Emitter.init(writer);
    return Self{
        .module = module,
        .module_interned_strings = module_interned_strings,
        .emitter = emitter,
        .writer = writer,
    };
}

/// Generates C code for the provided Orange module and writes it to the given writer.
pub fn generate(self: *Self) CodeGen_Error!void {
    try self.output_header_include();
    try self.output_impls();
    try self.output_variant_names();
    try self.emitter.forall_functions(self, self.module.cfgs.items, "Monomorphed function declarations", output_monomprphed_decl);
    try self.emitter.forall_functions(self, self.module.cfgs.items, "Monomorphed function definitions", output_monomprphed_def);
    try self.emitter.forall_functions(self, self.module.cfgs.items, "Function definitions", output_non_monomorphed_def);
}

pub fn output_header_include(self: *Self) CodeGen_Error!void {
    try self.writer.print(
        \\/* Code generated using the Orange compiler http://ornglang.org */
        \\
        \\#include "{s}-{s}.h"
        \\
    , .{ self.module.package_name, self.module.name() });
}

fn output_impls(
    self: *Self,
) CodeGen_Error!void {
    var vtable_count: usize = 0;
    for (self.module.impls.items) |impl| {
        if (impl.impl.num_virtual_methods != 0) {
            vtable_count += 1;
        }
    }

    if (vtable_count > 0) {
        // TODO: Count impls that have virtual methods
        // Do not output header comment if there are no impls!
        try self.writer.print("\n/* Trait vtable implementations */\n", .{});
    }

    for (self.module.impls.items) |impl| {
        const trait = impl.impl.trait.?;
        const trait_symbol = trait.symbol().?;
        const trait_decl = trait_symbol.decl.?;
        const trait_module = trait_symbol.scope.module.?;
        if (impl.impl.num_virtual_methods == 0 and trait_decl.trait.super_traits.items.len == 0) {
            continue;
        }
        try self.writer.print("const struct vtable_{s}__{s}__{}_{s} {s}__{s}_{}__vtable = {{\n", .{
            trait_module.package_name,
            trait_module.name(),
            trait_symbol.scope.uid,
            trait_symbol.name,
            //
            self.module.package_name,
            self.module.name(),
            impl.scope().?.uid,
        });
        for (impl.impl.method_defs.items) |decl| {
            if (!decl.method_decl.is_virtual) {
                continue;
            }
            try self.writer.print("    ._{s} = ", .{decl.method_decl.name.token().data});
            try self.emitter.output_symbol(decl.symbol().?);
            try self.writer.print(",\n", .{});
        }
        for (trait_decl.trait.super_traits.items, 0..) |super_trait, i| {
            const super_trait_symbol = super_trait.symbol().?;
            const super_trait_decl = super_trait_symbol.decl.?;
            if (super_trait_decl.trait.num_virtual_methods == 0 and super_trait_decl.trait.super_traits.items.len == 0) continue;
            try self.writer.print("    ._{} = &{s}__{s}_{}__vtable,\n", .{
                i,
                self.module.package_name,
                self.module.name(),
                impl.scope().?.impl_trait_lookup(impl.impl._type, super_trait_symbol).ast.?.scope().?.uid,
            });
        }
        try self.writer.print("}};\n", .{});
    }
}

pub fn output_monomprphed_decl(self: *Self, cfg: *CFG) CodeGen_Error!void {
    if (!cfg.symbol.is_monomorphed) {
        return;
    }
    try self.writer.print("static ", .{});
    try self.emitter.output_function_prototype(cfg);
    try self.writer.print(";\n", .{});
}

pub fn output_monomprphed_def(self: *Self, cfg: *CFG) CodeGen_Error!void {
    if (!cfg.symbol.is_monomorphed) {
        return;
    }
    try self.writer.print("static ", .{});
    try self.output_function_definition(cfg);
}

fn output_non_monomorphed_def(self: *Self, cfg: *CFG) CodeGen_Error!void {
    if (cfg.symbol.is_monomorphed or cfg.symbol.scope.module != self.module) {
        return;
    }
    try self.output_function_definition(cfg);
}

/// Output the definition of a function.
pub fn output_function_definition(self: *Self, cfg: *CFG) CodeGen_Error!void {
    if (cfg != self.module.cfgs.items[0]) try self.writer.print("\n", .{});
    try self.emitter.output_function_prototype(cfg);
    try self.writer.print("\n{{\n", .{});

    // Declare local variables
    for (cfg.symbvers.items) |symbver| {
        if (symbver.symbol.expanded_type().sizeof() == 0 or // symbol's C type is `void`
            (symbver.symbol.uses == 0 and symbver.symbol.name[0] != '_' and symbver.symbol.aliases == 0) // non-bookkeeping symbol is not used
        ) {
            continue; // Do not output unit variables
        }
        try self.emitter.output_var_decl(symbver.symbol, false);
    }

    // Mark unused parameters as discarded
    const param_symbols = cfg.symbol.decl.?.param_symbols();
    if (param_symbols != null) {
        for (param_symbols.?.items) |param| {
            // Do this only if they aren't discarded in source
            // Users can discard parameters, however used parameters may also become unused through optimizations
            if (Emitter.is_storable(param.expanded_type()) and // unit-typed parameters aren't emitted
                param.uses == 0)
            {
                try self.writer.print("    (void)", .{});
                try self.emitter.output_symbol(param);
                try self.writer.print(";\n", .{});
            }
        }
    }

    // Generate the basic-block graph, starting at the init basic block
    if (cfg.block_graph_head) |block_graph_head| {
        try self.output_basic_block(cfg, block_graph_head, cfg.return_symbol);
        cfg.clear_visited_BBs();
    }

    try self.writer.print("}}\n", .{});
}

pub fn output_main_function(self: *Self) CodeGen_Error!void {
    if (self.module.entry == null) {
        return;
    }
    const symbol = self.module.entry.?.symbol;

    const codomain = symbol.expanded_type().rhs().expand_identifier();
    var string_access: []const u8 = "";
    var specifier: ?[]const u8 = null;
    switch (codomain.*) {
        .identifier => {
            const info = prelude_.info_from_name(codomain.token().data).?;
            specifier = switch (info.type_kind) {
                .boolean, .signed_integer => "d",
                .unsigned_integer => "u",
                .floating_point => "f",
                else => unreachable,
            };
        },
        .struct_type => |p| if (p.was_slice) {
            string_access = "._0";
            specifier = "s";
        },
        else => {},
    }

    try self.writer.print(
        \\int main(int argc, char *argv[]) {{
        \\    
    , .{});

    var contexts_used = Type_Map(void).init(std.heap.page_allocator);
    defer contexts_used.deinit();
    try contexts_used.put_many(symbol.type().function.contexts.items, void{});
    try self.emitter.output_context_defs(&contexts_used);

    if (specifier != null) {
        try self.writer.print("printf(\"%{s}{s}\", ", .{ if (codomain.sizeof() == 8) "l" else "", specifier.? });
        try self.emitter.output_symbol(symbol);
        try self.writer.print("(){s});", .{string_access});
    } else {
        if (codomain.* == .enum_type and codomain.enum_type.from == .@"error") {
            try self.emitter.output_type(codomain);
            try self.writer.print("  retcode = ", .{});
        }
        try self.emitter.output_symbol(symbol);
        try self.writer.print("(", .{});
        try self.emitter.output_context_args(symbol.type().function.contexts.items);
        try self.writer.print(");\n", .{});
    }

    if (codomain.* == .enum_type and codomain.enum_type.from == .@"error") {
        try self.writer.print(
            \\    return (int)retcode.tag;
            \\}}
            \\
        , .{});
    } else {
        try self.writer.print(
            \\    return 0;
            \\}}
            \\
        , .{});
    }
}

fn output_variant_names(self: *Self) CodeGen_Error!void {
    if (self.module.enums.keys().len > 0) {
        try self.writer.print("\n/* Enum variant name tables */\n", .{});
    }
    for (self.module.enums.keys(), 0..) |enum_decl, i| {
        if (i != 0) try self.writer.print("\n", .{});
        try self.writer.print("const ", .{});
        try self.emitter.output_type(prelude_.string_type);
        try self.writer.print(" ", .{});
        try self.emitter.output_symbol(enum_decl.symbol().?);
        try self.writer.print("__{}_variant_names[{}] = {{\n", .{ enum_decl.symbol().?.scope.uid, enum_decl.enum_decl.fields.items.len });
        for (enum_decl.enum_decl.fields.items) |field| {
            const variant_name = field.annotation.pattern.token().data;
            try self.writer.print("    {{(uint8_t *)\"{s}\", {}}},\n", .{ variant_name, variant_name.len });
        }
        try self.writer.print("}};\n", .{});
    }
}

/// Outputs the C code for a basic-block in the given control flow graph.
fn output_basic_block(
    self: *Self,
    cfg: *CFG, // TODO: Accept cfg block graph head
    start_bb: *Basic_Block,
    return_symbol: *Symbol,
) CodeGen_Error!void {
    // FIXME: High Cyclo
    var bb_queue = std.array_list.Managed(*Basic_Block).init(std.heap.page_allocator); // page alloc ok, immediately deinit'd
    defer bb_queue.deinit();
    try bb_queue.append(start_bb);
    start_bb.visited = true;

    // Output basic-blocks in BFS order
    var head: usize = 0;
    while (head < bb_queue.items.len) {
        const bb: *Basic_Block = bb_queue.items[head];
        head += 1;

        const is_head_bb = cfg.block_graph_head != null and cfg.block_graph_head.? == bb;
        const more_than_one_incoming = bb.number_predecessors > 1;
        if (!is_head_bb or more_than_one_incoming) {
            // Do not output a label for a block that is never jumped to (gcc complains)
            try self.writer.print("BB{}:\n", .{bb.uid});
        }

        // Output Instruction instructions for the basic-block
        for (bb.instructions.items) |instr| {
            try self.output_instruction(instr);
        }

        switch (bb.terminator) {
            .unconditional => {
                if (bb.terminator.unconditional) |next| {
                    if (!next.visited) {
                        try bb_queue.append(next);
                        next.visited = true;
                    }
                    try self.writer.print("    goto BB{};\n", .{next.uid});
                } else {
                    try self.output_return(return_symbol);
                }
            },
            .conditional => {
                // Generate the if
                try self.writer.print("    if (", .{});
                try self.output_rvalue(bb.terminator.conditional.condition, Instruction.Precedence.highest);
                try self.writer.print(")\n    {{\n", .{});

                // Generate the `next` BB
                if (bb.terminator.conditional.true_target) |next| {
                    try self.writer.print("        goto BB{};\n    }}", .{next.uid});
                    if (!next.visited) {
                        try bb_queue.append(next);
                        next.visited = true;
                    }
                } else {
                    try self.writer.print("    ", .{});
                    try self.output_return(return_symbol);
                    try self.writer.print("    }}", .{});
                }

                // Generate the `branch` BB if it isn't the next one up
                if (bb.terminator.conditional.false_target) |branch| {
                    if (!branch.visited) {
                        try bb_queue.append(branch);
                        branch.visited = true;
                    }
                    try self.writer.print("\n    else\n    {{\n        goto BB{};\n    }}\n", .{branch.uid});
                } else {
                    try self.writer.print("\n    else\n    {{\n    ", .{});
                    try self.output_return(return_symbol);
                    try self.writer.print("    }}\n", .{});
                }
            },
            .panic => {},
        }
    }
}

/// Outputs the C code for an instruction.
fn output_instruction(self: *Self, instr: *Instruction) CodeGen_Error!void {
    if (instr.dest != null) {
        try self.output_lvalue_check(instr.span, instr.dest.?);
    }
    if (instr.src1 != null) {
        try self.output_lvalue_check(instr.span, instr.src1.?);
    }
    if (instr.src2 != null) {
        try self.output_lvalue_check(instr.span, instr.src2.?);
    }
    if (instr.data == .lval_list) {
        for (instr.data.lval_list.items) |lval| {
            try self.output_lvalue_check(instr.span, lval);
        }
    }

    if (instr.dest != null and !Emitter.is_storable(instr.dest.?.get_expanded_type()) and instr.kind != .call and instr.kind != .invoke) {
        return;
    }

    try self.output_instruction_post_check(instr);
}

/// Outputs the C code for an Instruction instruction after runtime checks have run.
fn output_instruction_post_check(self: *Self, instr: *Instruction) CodeGen_Error!void {
    switch (instr.kind) {
        .load_unit => {
            // Do nothing!
        },
        .load_symbol => {
            if (instr.dest.?.get_expanded_type().* == .function) {
                try self.output_var_assign_cast(instr.dest.?, instr.dest.?.get_expanded_type());
            } else {
                try self.output_var_assign(instr.dest.?);
            }
            try self.emitter.output_symbol(instr.data.symbol);
            try self.writer.print(";\n", .{});
        },
        .load_int => {
            try self.output_var_assign(instr.dest.?);
            try self.writer.print("{}", .{instr.data.int});
            if (prelude_.info_from_ast(instr.dest.?.get_expanded_type()).?.type_kind == .unsigned_integer) {
                if (instr.dest.?.expanded_type_sizeof() == 8) {
                    try self.writer.print("ULL", .{});
                } else {
                    try self.writer.print("U", .{});
                }
            } else if (prelude_.info_from_ast(instr.dest.?.get_expanded_type()).?.type_kind == .signed_integer) {
                if (instr.dest.?.expanded_type_sizeof() == 8) {
                    try self.writer.print("LL", .{});
                }
            }
            try self.writer.print(";\n", .{});
        },
        .load_float => {
            try self.output_var_assign(instr.dest.?);
            try self.writer.print("{};\n", .{instr.data.float});
        },
        .load_string => {
            try self.output_var_assign_cast(instr.dest.?, instr.dest.?.get_expanded_type());
            const interned_strings = self.module_interned_strings.get(instr.data.string_id.module_uid).?;
            try self.writer.print("{{(uint8_t *)\"", .{});
            const str = interned_strings.items()[instr.data.string_id.string_idx];
            for (str) |byte| {
                try self.writer.print("\\x{X:0>2}", .{byte});
            }
            try self.writer.print("\", {}}};\n", .{interned_strings.items()[instr.data.string_id.string_idx].len});
        },
        .load_struct => {
            try self.output_var_assign_cast(instr.dest.?, instr.dest.?.get_expanded_type());
            try self.writer.print("{{", .{});
            const dest_type = instr.dest.?.get_expanded_type();
            if (dest_type.* == .struct_type or dest_type.* == .tuple_type) { // TODO: Likely need a `load_array` instruction
                const product_list = dest_type.children().*;
                for (instr.data.lval_list.items, product_list.items, 1..) |term, expected, i| {
                    if (Emitter.is_storable(expected)) {
                        // Don't use values of type `void` (don't exist in C! (Goobersville!))
                        try self.output_rvalue(term, instr.kind.precedence());
                        if (i < product_list.items.len and Emitter.is_storable(product_list.items[i - 1])) {
                            try self.writer.print(", ", .{});
                        }
                    }
                }
            } else {
                for (instr.data.lval_list.items, 1..) |term, i| {
                    // Don't use values of type `void` (don't exist in C! (Goobersville!))
                    try self.output_rvalue(term, instr.kind.precedence());
                    if (i < instr.data.lval_list.items.len) {
                        try self.writer.print(", ", .{});
                    }
                }
            }
            try self.writer.print("}};\n", .{});
        },
        .load_union => {
            try self.output_var_assign_cast(instr.dest.?, instr.dest.?.get_expanded_type());
            try self.writer.print("{{.tag = {}", .{instr.data.int});
            if (instr.src1 != null and Emitter.is_storable(instr.src1.?.get_expanded_type())) {
                try self.writer.print(", ._{} = ", .{instr.data.int});
                try self.output_rvalue(instr.src1.?, instr.kind.precedence());
            }
            try self.writer.print("}};\n", .{});
        },
        .copy => {
            try self.output_var_assign(instr.dest.?);
            try self.output_rvalue(instr.src1.?, instr.kind.precedence());
            try self.writer.print(";\n", .{});
        },
        .mut_addr_of, .addr_of => {
            try self.output_var_assign(instr.dest.?);
            try self.output_lvalue(instr.src1.?, instr.kind.precedence());
            try self.writer.print(";\n", .{});
        },
        .mut_dyn_value, .dyn_value => {
            try self.output_var_assign_cast(instr.dest.?, instr.dest.?.get_expanded_type());
            try self.writer.print("{{", .{});
            try self.output_lvalue(instr.src1.?, instr.kind.precedence());
            try self.writer.print(", ", .{});

            const dyn_type = instr.dest.?.get_expanded_type();
            const trait_decl = dyn_type.child().symbol().?.decl.?;
            if (trait_decl.trait.num_virtual_methods != 0) {
                try self.writer.print("&", .{});
                try self.output_vtable_impl(instr.data.dyn.impl);
            } else {
                try self.writer.print("NULL", .{});
            }
            try self.writer.print("}};\n", .{});
        },
        .not,
        .negate_int,
        .negate_float,
        .bit_not,
        .equal,
        .not_equal,
        .greater_int,
        .greater_float,
        .greater_equal_int,
        .greater_equal_float,
        .lesser_int,
        .lesser_float,
        .lesser_equal_int,
        .lesser_equal_float,
        .add_int,
        .add_float,
        .sub_int,
        .sub_float,
        .mult_int,
        .mult_float,
        .div_int,
        .div_float,
        .mod,
        .bit_and,
        .bit_or,
        .bit_xor,
        .left_shift,
        .right_shift,
        => try self.output_operator(instr),
        .get_tag => {
            try self.output_var_assign(instr.dest.?);
            try self.output_rvalue(instr.src1.?, instr.kind.precedence());
            try self.writer.print(".tag;\n", .{});
        },
        .get_name => {
            const enum_type = instr.src1.?.get_expanded_type();
            if (enum_type.enum_type.from == .optional) {
                try self.output_var_assign_cast(instr.dest.?, instr.dest.?.get_expanded_type());
                try self.writer.print("((", .{});
                try self.output_rvalue(instr.src1.?, instr.kind.precedence());
                try self.writer.print(".tag == 0) ? ((", .{});
                try self.emitter.output_type(instr.dest.?.get_expanded_type());
                try self.writer.print("){{(uint8_t *)\"some\", 4}}) : ((", .{});
                try self.emitter.output_type(instr.dest.?.get_expanded_type());
                try self.writer.print("){{(uint8_t *)\"none\", 4}}));\n", .{});
            } else if (enum_type.enum_type.from == .@"error") {
                try self.output_var_assign_cast(instr.dest.?, instr.dest.?.get_expanded_type());
                try self.writer.print("((", .{});
                try self.output_rvalue(instr.src1.?, instr.kind.precedence());
                try self.writer.print(".tag == 0) ? ((", .{});
                try self.emitter.output_type(instr.dest.?.get_expanded_type());
                try self.writer.print("){{(uint8_t *)\"ok\", 2}}) : ((", .{});
                try self.emitter.output_type(instr.dest.?.get_expanded_type());
                try self.writer.print("){{(uint8_t *)\"err\", 3}}));\n", .{});
            } else {
                try self.output_var_assign(instr.dest.?);
                try self.emitter.output_symbol(enum_type.enum_type.decl.?.symbol().?);
                try self.writer.print("__{}_variant_names[", .{enum_type.enum_type.decl.?.symbol().?.scope.uid});
                try self.output_rvalue(instr.src1.?, instr.kind.precedence());
                try self.writer.print(".tag", .{});
                try self.writer.print("];\n", .{});
            }
        },
        .cast => {
            if (instr.dest.?.get_expanded_type().* == .dyn_type) {
                // dyn type up-casting
                const super_dyn_type = instr.dest.?.get_expanded_type();
                const super_trait_symbol = super_dyn_type.child().symbol().?;
                const super_trait_decl = super_trait_symbol.decl.?;
                const sub_dyn_type = instr.src1.?.get_expanded_type();
                const sub_trait_decl = sub_dyn_type.child().symbol().?.decl.?;

                try self.output_var_assign_cast(instr.dest.?, instr.dest.?.get_expanded_type());
                try self.writer.print("{{", .{});
                try self.output_rvalue(instr.src1.?, instr.kind.precedence());
                try self.writer.print(".data_ptr, ", .{});

                if (super_trait_decl.trait.num_virtual_methods != 0) {
                    // super trait has a vtable, find path to it
                    var vtables = std.array_list.Managed(usize).init(std.heap.page_allocator);
                    defer vtables.deinit();
                    _ = try self.append_vtable_to_super(sub_trait_decl, super_trait_symbol, &vtables);

                    try self.output_rvalue(instr.src1.?, instr.kind.precedence());
                    try self.writer.print(".vtable", .{});
                    for (0..vtables.items.len) |i| {
                        try self.writer.print("->_{}", .{vtables.items[vtables.items.len - i - 1]});
                    }
                } else {
                    try self.writer.print("NULL", .{});
                }
                try self.writer.print("}};\n", .{});
            } else {
                try self.output_var_assign(instr.dest.?);
                try self.writer.print("(", .{});
                try self.emitter.output_type(instr.dest.?.get_expanded_type());
                try self.writer.print(")", .{});
                try self.output_rvalue(instr.src1.?, instr.kind.precedence());
                try self.writer.print(";\n", .{});
            }
        },
        .call => {
            // TODO: De-duplicate 2
            try self.output_call_prefix(instr);
            try self.output_rvalue(instr.src1.?, instr.kind.precedence());
            try self.writer.print("(", .{});
            for (instr.data.call.arg_lval_list.items, 0..) |term, i| {
                if (Emitter.is_storable(term.get_expanded_type())) {
                    // Do not output `void` arguments
                    try self.output_rvalue(term, Instruction.Precedence.highest);
                    if (i + 1 < instr.data.call.arg_lval_list.items.len and Emitter.is_storable(instr.data.call.arg_lval_list.items[i + 1].get_expanded_type())) {
                        try self.writer.print(", ", .{});
                    }
                }
            }
            try self.writer.print(");\n", .{});
        },
        .invoke => {
            // FIXME: High Cyclo
            try self.output_call_prefix(instr);
            if (instr.data.invoke.method_decl.method_decl.init != null and !instr.data.invoke.method_decl.method_decl.is_virtual) {
                // method is non-virtual
                // { method name }({ args })
                try self.output_rvalue(instr.data.invoke.method_decl_lval.?, Instruction.Precedence.prefix);
                try self.writer.print("(", .{});
            } else if (instr.data.invoke.dyn_value != null) {
                // method is virtual, dyn_value (receiver ptr + vtable ptr) isn't null
                // { dyn value }.vtable->{.super_vtable->}{ method name }({ args })
                try self.output_vtable_invoke(instr);
            } else {
                // method is virtual, dyn_value is null
                // { vtable impl }.{ method name }({ args })
                try self.output_vtable_impl(instr.data.invoke.method_decl.method_decl.impl.?);
                try self.writer.print("._{s}(", .{instr.data.invoke.method_decl.method_decl.name.token().data});
            }
            const num_invoke_args = instr.data.invoke.arg_lval_list.items.len;
            for (instr.data.invoke.arg_lval_list.items, 0..) |term, i| {
                if (Emitter.is_storable(term.get_expanded_type())) {
                    // Do not output `void` arguments
                    try self.output_rvalue(term, Instruction.Precedence.postfix);
                    if (instr.data.invoke.dyn_value != null and instr.data.invoke.dyn_value == term and i == 0) {
                        try self.writer.print(".data_ptr", .{});
                    }
                    if (i + 1 < num_invoke_args and Emitter.is_storable(instr.data.invoke.arg_lval_list.items[i + 1].get_expanded_type())) {
                        try self.writer.print(", ", .{});
                    }
                }
            }
            // TODO: context args
            try self.writer.print(");\n", .{});
        },
        .label,
        .jump,
        .branch_if_false,
        => {},
        .push_stack_trace => {
            var spaces = std.array_list.Managed(u8).init(std.heap.page_allocator); // page alloc ok, immediately deinit'd
            defer spaces.deinit();
            for (1..instr.span.col - 1) |_| {
                try spaces.print(" ", .{});
            }
            try self.writer.print("    orange_debug__lines[orange_debug__line_idx++] = ", .{});
            try instr.span.print_debug_line(self.writer, Span.c_format);
            try self.writer.print(";\n", .{});

            for (1..instr.span.col - 1) |_| {
                try spaces.print(" ", .{});
            }
            try self.writer.print("    orange_debug__err_line_idx = orange_debug__line_idx;\n", .{});
        },
        .pop_stack_trace => {
            try self.writer.print(
                \\    orange_debug__line_idx--;
                \\
            , .{});
        },
        .pop_err_trace => {
            try self.writer.print(
                \\    orange_debug__err_line_idx = orange_debug__line_idx;
                \\
            , .{});
        },
        .panic => {
            try self.writer.print(
                \\    orange_debug__panic("{s}\n");
                \\
            ,
                .{instr.data.string},
            );
        },
        else => std.debug.panic("compiler error: unimplemented output_instr() for: Kind.{s}", .{@tagName(instr.kind)}),
    }
}

/// Outputs C code for the prefix of a call
fn output_call_prefix(self: *Self, instr: *Instruction) !void {
    const nonvoid_fn = Emitter.is_storable(instr.dest.?.get_expanded_type());
    const symbol_used = if (instr.dest.?.* == .symbver) instr.dest.?.symbver.symbol.uses > 0 else false;
    if (!symbol_used) {
        try self.writer.print("    (void)", .{});
    } else if (nonvoid_fn) {
        try self.output_var_assign(instr.dest.?);
    } else {
        try self.writer.print("    ", .{});
    }
}

fn output_vtable_invoke(self: *Self, instr: *Instruction) !void {
    std.debug.assert(instr.data.invoke.dyn_value.?.get_expanded_type().* == .dyn_type);
    const method_name = instr.data.invoke.method_decl.method_decl.name.token().data;
    const dyn_type = instr.data.invoke.dyn_value.?.get_expanded_type();
    const trait_decl = dyn_type.child().symbol().?.decl.?;
    var vtables = std.array_list.Managed(usize).init(std.heap.page_allocator);
    defer vtables.deinit();
    _ = try self.append_vtable_indirection(trait_decl, method_name, &vtables);
    try self.output_rvalue(instr.data.invoke.dyn_value.?, Instruction.Precedence.postfix);
    try self.writer.print(".vtable->", .{});
    for (0..vtables.items.len) |i| {
        try self.writer.print("_{}->", .{vtables.items[vtables.items.len - i - 1]});
    }
    try self.writer.print("_{s}(", .{method_name});
}

fn append_vtable_indirection(self: *Self, trait_decl: *ast_.AST, method_name: []const u8, vtables: *std.array_list.Managed(usize)) !bool {
    for (trait_decl.trait.method_decls.items) |item| {
        if (std.mem.eql(u8, method_name, item.method_decl.name.token().data)) {
            return true;
        }
    }
    for (trait_decl.trait.super_traits.items, 0..) |super_trait, i| {
        const super_trait_decl = super_trait.symbol().?.decl.?;
        if (try self.append_vtable_indirection(super_trait_decl, method_name, vtables)) {
            try vtables.append(i);
            return true;
        }
    }
    return false;
}

fn append_vtable_to_super(self: *Self, sub_trait_decl: *ast_.AST, target_super_trait_symbol: *Symbol, vtables: *std.array_list.Managed(usize)) !bool {
    for (sub_trait_decl.trait.super_traits.items, 0..) |super_trait, i| {
        const super_trait_symbol = super_trait.symbol().?;
        const super_trait_decl = super_trait.symbol().?.decl.?;
        if (super_trait_symbol == target_super_trait_symbol) {
            try vtables.append(i);
            return true;
        }
        if (try self.append_vtable_to_super(super_trait_decl, target_super_trait_symbol, vtables)) {
            try vtables.append(i);
            return true;
        }
    }
    return false;
}

/// Outputs the C code for checking lvalue operations. The current checks are:
/// - Index bounds check
/// - Tag check on union selects
fn output_lvalue_check(self: *Self, span: Span, lvalue: *lval_.L_Value) CodeGen_Error!void {
    switch (lvalue.*) {
        .symbver => {},
        .dereference => try self.output_lvalue_check(span, lvalue.dereference.expr),
        .index => {
            try self.output_lvalue_check(span, lvalue.index.lhs);
            try self.output_lvalue_check(span, lvalue.index.rhs);

            if (lvalue.index.length != null) {
                try self.writer.print("    orange_debug__bounds_check(", .{});
                try self.output_rvalue(lvalue.index.rhs, Instruction.Precedence.highest); // idx
                try self.writer.print(", ", .{});
                try self.output_rvalue(lvalue.index.length.?, Instruction.Precedence.highest); // length
                try self.writer.print(", ", .{});
                try span.print_debug_line(self.writer, Span.c_format);
                try self.writer.print(");\n", .{});
            }
        },
        .select => {
            try self.output_lvalue_check(span, lvalue.select.lhs);

            if (lvalue.select.tag != null) {
                try self.writer.print("    orange_debug__tag_check(", .{});
                try self.output_rvalue(lvalue.select.tag.?, Instruction.Precedence.highest); // tag
                try self.writer.print(", {}, ", .{lvalue.select.field});
                try span.print_debug_line(self.writer, Span.c_format);
                try self.writer.print(");\n", .{});
            }
        },
        .raw_address => std.debug.panic("compiler error: cannot output raw address lvalue", .{}),
    }
}

/// Outputs the C code for an rvalue expression based on the provided lvalue.
fn output_rvalue(self: *Self, lvalue: *lval_.L_Value, outer_precedence: Instruction.Precedence) CodeGen_Error!void {
    if (@intFromEnum(outer_precedence) < @intFromEnum(lvalue.lval_precedence())) {
        // Opening paren, if needed by precedence
        try self.writer.print("(", .{});
    }
    switch (lvalue.*) {
        .dereference => {
            try self.writer.print("*", .{});
            if (lvalue.dereference.expr.get_expanded_type().addr_of.anytptr) {
                // Cast from `void*` to true pointer before dereferencing
                try self.writer.print("(", .{});
                try self.emitter.output_type(lvalue.get_expanded_type());
                try self.writer.print(" *)", .{});
            }
            try self.output_rvalue(lvalue.dereference.expr, lvalue.lval_precedence());
        },
        .index => {
            try self.writer.print("*", .{});
            try self.output_lvalue(lvalue, lvalue.lval_precedence());
        },
        .select => {
            try self.output_rvalue(lvalue.select.lhs, lvalue.lval_precedence()); // This will dereference, no need for `->`
            const unexpanded_type: ?*Type_AST = lvalue.select.lhs.get_expanded_type().common()._unexpanded_type;
            if (unexpanded_type != null and unexpanded_type.?.* == .identifier and unexpanded_type.?.symbol() != null and unexpanded_type.?.symbol().?.storage == .@"extern") {
                // Select the nominal C name
                const field_name = lvalue.select.lhs.get_expanded_type().children().items[@intCast(lvalue.select.field)].annotation.pattern.token().data;
                try self.writer.print(".{s}", .{field_name});
            } else {
                // Select the structural Orange name
                try self.writer.print("._{}", .{lvalue.select.field});
            }
        },
        .symbver => {
            if (lvalue.symbver.symbol.decl.?.* == .receiver) {
                try self.writer.print("(", .{});
                try self.emitter.output_type(lvalue.get_expanded_type());
                try self.writer.print(")", .{});
            }
            try self.emitter.output_symbol(lvalue.symbver.symbol);
        },
        .raw_address => std.debug.panic("compiler error: cannot output raw address lvalue", .{}),
    }
    if (@intFromEnum(outer_precedence) < @intFromEnum(lvalue.lval_precedence())) {
        // Closing paren, if needed by precedence
        try self.writer.print(")", .{});
    }
}

/// Outputs the C code for an lvalue expression.
fn output_lvalue(self: *Self, lvalue: *lval_.L_Value, outer_precedence: Instruction.Precedence) CodeGen_Error!void {
    if (lvalue.expanded_type_sizeof() == 0) {
        try self.writer.print("(void *)0xAAAAAAAA", .{});
        return;
    }
    switch (lvalue.*) {
        .dereference => {
            // lvalue of a dereference is just the rvalue of it's expression
            try self.output_rvalue(lvalue.dereference.expr, outer_precedence);
        },
        .index => {
            try self.writer.print("(", .{});

            // Generate reinterpret cast to pointer of elements
            try self.writer.print("(", .{});
            try self.emitter.output_type(lvalue.get_expanded_type());
            try self.writer.print(" *)", .{});
            if (lvalue.index.lhs.get_expanded_type().* == .addr_of) {
                // Index of a multiptr addr, lvalue is simply the rvalue
                std.debug.assert(lvalue.index.lhs.get_expanded_type().addr_of.multiptr);
                try self.output_rvalue(lvalue.index.lhs, Instruction.Precedence.prefix);
            } else {
                // Index a slice, usual lvalue of lhs
                try self.output_lvalue(lvalue.index.lhs, Instruction.Precedence.prefix);
            }

            // Only generate index add if index is non-zero
            // NOTE: Do not generate checked addition. The index is already checked.
            try self.writer.print(" + ", .{});
            try self.output_rvalue(lvalue.index.rhs, lvalue.lval_precedence());

            try self.writer.print(")", .{});
        },
        .symbver, .select => {
            // For symbvers and selects, just print out the rvalue version, and take the address of it
            if (@intFromEnum(outer_precedence) < @intFromEnum(Instruction.Precedence.prefix)) {
                try self.writer.print("(", .{});
            }
            try self.writer.print("&", .{});
            try self.output_rvalue(lvalue, Instruction.Precedence.prefix);
            if (@intFromEnum(outer_precedence) < @intFromEnum(Instruction.Precedence.prefix)) {
                try self.writer.print(")", .{});
            }
        },
        .raw_address => std.debug.panic("compiler error: cannot output raw address lvalue", .{}),
    }
}

/// Emits the return statement from a function
fn output_return(self: *Self, return_symbol: *Symbol) CodeGen_Error!void {
    try self.writer.print("    return", .{});
    if (return_symbol.defs > 0 and Emitter.is_storable(return_symbol.expanded_type())) {
        try self.writer.print(" ", .{});
        try self.emitter.output_symbol(return_symbol);
    }
    try self.writer.print(";\n", .{});
}

/// Outputs an assignment to a symbol template. It is expected that a value is output immediately
/// after this to complete the assignment.
fn output_var_assign(self: *Self, lval: *lval_.L_Value) CodeGen_Error!void {
    try self.writer.print("    ", .{});
    try self.output_rvalue(lval, Instruction.Precedence.highest);
    try self.writer.print(" = ", .{});
}

/// Outputs the C code for assigning a value to a variable with a cast.
fn output_var_assign_cast(self: *Self, lval: *lval_.L_Value, _type: *Type_AST) CodeGen_Error!void {
    try self.output_var_assign(lval);
    try self.writer.print("(", .{});
    try self.emitter.output_type(_type);
    try self.writer.print(")", .{});
}

/// Outputs the C code for an operator from an Instruction.
fn output_operator(self: *Self, instr: *Instruction) CodeGen_Error!void {
    try self.output_var_assign(instr.dest.?);
    const info = prelude_.info_from_ast(instr.dest.?.get_expanded_type());
    if (info != null and instr.kind.is_checked() and instr.dest.?.get_expanded_type().can_expanded_represent_int() and (info.?.type_kind != .unsigned_integer or instr.kind == .mod)) { // TODO: Check if checked operations are enabled, too
        try self.writer.print("orange_debug__{s}_{s}(", .{ instr.kind.checked_name(), info.?.c_name });
        try self.output_rvalue(instr.src1.?, instr.kind.precedence());
        try self.writer.print(", ", .{});
        if (instr.kind.arity() == .binop) {
            try self.output_rvalue(instr.src2.?, instr.kind.precedence());
            try self.writer.print(", ", .{});
        }
        try instr.span.print_debug_line(self.writer, Span.c_format);
        try self.writer.print(")", .{});
    } else if (instr.kind.arity() == .unop) {
        // Unop, no-check
        try self.writer.print("{s}", .{instr.kind.c_token()});
        try self.output_rvalue(instr.src1.?, instr.kind.precedence());
    } else {
        // Binop, no-check
        try self.output_rvalue(instr.src1.?, instr.kind.precedence());
        try self.writer.print(" {s} ", .{instr.kind.c_token()});
        try self.output_rvalue(instr.src2.?, instr.kind.precedence());
    }
    try self.writer.print(";\n", .{});
}

/// Prints out the vtable name given an impl AST
fn output_vtable_impl(self: *Self, impl: *ast_.AST) CodeGen_Error!void {
    const impl_module = impl.scope().?.module.?; // what makes you think the impl is in the same module??
    try self.writer.print("{s}__{s}_{}__vtable", .{ impl_module.package_name, impl_module.name(), impl.scope().?.uid });
}
