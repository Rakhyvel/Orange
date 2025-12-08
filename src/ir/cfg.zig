//! This file contains the definition of a Control Flow Graph (Self) data structure.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const alignment_ = @import("../util/alignment.zig");
const Basic_Block = @import("../ir/basic-block.zig");
const Instruction = @import("../ir/instruction.zig");
const lval_ = @import("../ir/lval.zig");
const Span = @import("../util/span.zig");
const Symbol = @import("../symbol/symbol.zig");
const Symbol_Version = @import("symbol_version.zig");
const Type_Set = @import("../types/type_set.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");

const Self = @This();

// bp offset of a frame's retval address
pub const retval_offset: i64 = -3 * @sizeOf(i64);
pub const locals_starting_offset = 8;

/// Initial basic block in the basic block graph
block_graph_head: ?*Basic_Block,

/// Flat list of all basic blocks
basic_blocks: std.array_list.Managed(*Basic_Block),

/// All other functions called by this function
children: std.AutoArrayHashMap(*Self, void),

/// All symbol versions that are parameters to the function this Self defines
/// TODO: Make this it's own type, in it's own file, with `put` and `get` methods
parameters: std.array_list.Managed(*Symbol_Version),

/// All symbol versions that are used. Should be filled in after optimizations.
/// TODO: Make this it's own type, in it's own file, with `put` and `get` methods
symbvers: std.array_list.Managed(*Symbol_Version),

/// The function that this Self represents
symbol: *Symbol,

/// The symbol that is used to store the return value of this function
return_symbol: *Symbol,

/// When true, Self is called from a runtime-context at least once, and should be generated
/// Set in the lowering stage when runtime access is needed
/// TODO: Remove for LCOM4
needed_at_runtime: bool,

/// Maps a module UID to the address of the first instruction of this CFG in the module's list of instructions
/// Used for Instruction interpretation
/// TODO: Remove for LCOM4
offset_table: std.array_hash_map.AutoArrayHashMap(u32, Instruction.Index),

/// Number of bytes required in order to store the local variables of the function
/// TODO: Remove for LCOM4
locals_size: ?i64,

allocator: std.mem.Allocator,

/// Initializes a Self
pub fn init(symbol: *Symbol, allocator: std.mem.Allocator) *Self {
    if (symbol.cfg) |cfg| {
        return cfg;
    }
    var retval = allocator.create(Self) catch unreachable;
    retval.block_graph_head = null;
    retval.basic_blocks = std.array_list.Managed(*Basic_Block).init(allocator);
    retval.children = std.AutoArrayHashMap(*Self, void).init(allocator);
    retval.symbvers = std.array_list.Managed(*Symbol_Version).init(allocator);
    retval.parameters = std.array_list.Managed(*Symbol_Version).init(allocator);
    retval.symbol = symbol;
    retval.return_symbol = Symbol.init(
        symbol.scope,
        "_retval",
        ast_.AST.create_decl(
            symbol.decl.?.token(),
            ast_.AST.create_pattern_symbol(symbol.decl.?.token(), .mut, .local, "_retval", allocator),
            symbol.type().rhs(),
            null,
            allocator,
        ),
        .mut,
        .local,
        allocator,
    );
    _ = retval.return_symbol.type().expand_identifier();
    _ = symbol.type().expand_identifier();
    retval.needed_at_runtime = false;
    retval.offset_table = std.array_hash_map.AutoArrayHashMap(u32, Instruction.Index).init(allocator);
    retval.locals_size = null;
    retval.allocator = allocator;
    symbol.cfg = retval;

    return retval;
}

/// Deinitializes a Self
pub fn deinit(self: *Self) void {
    for (self.basic_blocks.items) |bb| {
        bb.deinit();
    }
    self.basic_blocks.deinit();

    self.symbvers.deinit();

    for (self.parameters.items) |param| {
        param.deinit();
    }
    self.parameters.deinit();

    self.allocator.destroy(self);
}

pub fn assert_needed_at_runtime(self: *Self) void {
    if (self.needed_at_runtime) {
        return;
    }
    self.needed_at_runtime = true;

    for (self.children.keys()) |child| {
        child.assert_needed_at_runtime();
    }
}

/// Sets the `visited` flag for all basic-blocks in this Self to false.
///
/// BBs aren't trees, so `defer self.visited = false` won't work
/// Use this function instead
pub fn clear_visited_BBs(self: *Self) void {
    for (self.basic_blocks.items) |bb| {
        bb.visited = false;
    }
}

/// Fills in a the `symbvers` list with the symbol versions which are used in the Self
pub fn collect_generated_symbvers(self: *Self) void {
    for (self.basic_blocks.items) |bb| {
        for (bb.instructions.items) |instr| {
            if (instr.dest != null and
                instr.dest.?.extract_symbver().find_symbol_version_set(&self.parameters) == null)
            {
                _ = instr.dest.?.extract_symbver().put_symbol_version_set(&self.symbvers);
            }
            if (instr.kind == .mut_addr_of and instr.src1.?.extract_symbver().symbol.defs == 0 and !instr.src1.?.extract_symbver().symbol.param) {
                _ = instr.src1.?.extract_symbver().put_symbol_version_set(&self.symbvers);
            }
        }
    }
}

pub fn collect_cfg_types(self: *Self, type_set: *Type_Set) void {
    // Don't collect types from generic CFGs
    if (self.symbol.decl.?.* == .fn_decl and self.symbol.decl.?.num_generic_params() > 0) {
        return;
    }
    if (self.symbol.decl.?.* == .method_decl and self.symbol.decl.?.method_decl.impl.?.num_generic_params() > 0) {
        return;
    }

    // Add parameter types to type set
    const decl = self.symbol.decl.?;
    const param_symbols = decl.param_symbols();
    if (param_symbols != null) {
        for (param_symbols.?.items) |param| {
            _ = type_set.add_type(param.expanded_type());
        }
    }
    const context_param_symbols = decl.context_param_symbols();
    if (context_param_symbols != null) {
        for (context_param_symbols.?.items) |context_param| {
            _ = type_set.add_type(context_param.expanded_type());
        }
    }
    _ = type_set.add_type(self.return_symbol.expanded_type());

    // For all basic blocks in the cfg...
    for (self.basic_blocks.items) |bb| {
        bb.collect_basic_block_types(type_set);
    }
}

/// Converts the list of Instruction to a web of BB's
///
/// ### Parameters
/// - `self`: The CFG to fill the basic blocks for
/// - `instructions`: The list of instructions to convert to basic blocks
/// - `start_index`: The index into the instructions list to start the basic block at
/// - `allocator`: The allocator to use for the basic blocks
pub fn basic_block_from_instructions(self: *Self, instructions: std.array_list.Managed(*Instruction), start_index: ?usize) ?*Basic_Block {
    if (instructions.items.len == 0 or start_index == null) {
        return null;
    } else if (instructions.items[start_index.?].in_block) |in_block| {
        return in_block;
    }

    // Create block without instructions yet
    var block = Basic_Block.init(self.allocator);
    self.basic_blocks.append(block) catch unreachable;

    // Identify the end of the block
    var end_index: usize = start_index.?;
    while (end_index < instructions.items.len) {
        const instr = instructions.items[end_index];
        end_index += 1;
        instr.in_block = block;
        if (instr.kind == .label) {
            // Fallthrough to next basic block
            const next_block = self.basic_block_from_instructions(instructions, end_index);
            block.terminator = .{ .unconditional = next_block };
            break;
        } else if (instr.kind == .jump) {
            // Unconditional jump to other block
            const branch_next = instr.data.branch;
            const bb_index = if (branch_next != null) std.mem.indexOfScalar(*Instruction, instructions.items, branch_next.?).? + 1 else null;
            const next_block = self.basic_block_from_instructions(instructions, bb_index);
            block.terminator = .{ .unconditional = next_block };
            break;
        } else if (instr.kind == .panic) {
            // Block ends in a panic
            end_index += 1; // panics are kept as an instruction because they store the error message
            block.terminator = .panic;
            break;
        } else if (instr.kind == .branch_if_false) {
            // Branch only if condition is false else fallthrough
            const branch_next = instr.data.branch;
            const false_target_index = if (branch_next != null) std.mem.indexOfScalar(*Instruction, instructions.items, branch_next.?).? + 1 else null;
            const next_block = self.basic_block_from_instructions(instructions, end_index);
            const branch_block = self.basic_block_from_instructions(instructions, false_target_index);
            block.terminator = .{ .conditional = .{
                .condition = instr.src1.?,
                .true_target = next_block,
                .false_target = branch_block,
            } };
            break;
        }
    }

    // Assign instructions to the block
    block.instructions.appendSlice(instructions.items[start_index.? .. end_index - 1]) catch unreachable;

    return block;
}

pub fn calculate_usage(self: *Self) void {
    // FIXME: High Cyclo
    if (self.symbol.decl.?.* == .fn_decl or self.symbol.decl.?.* == .method_decl) {
        const param_symbols = self.symbol.decl.?.param_symbols();
        if (param_symbols != null) {
            for (param_symbols.?.items) |param_symbol| {
                param_symbol.uses = 0;
            }
        }
    }

    for (self.basic_blocks.items) |bb| {
        // Clear all used flags
        for (bb.instructions.items) |instr| {
            if (instr.dest != null) {
                instr.dest.?.reset_usage();
            }
        }
    }

    for (self.basic_blocks.items) |bb| {
        // Go through and see if each symbol is used
        for (bb.instructions.items) |instr| {
            if (instr.dest != null and instr.dest.?.* != .symbver) {
                instr.dest.?.increment_usage();
            }
            if (instr.src1 != null) {
                instr.src1.?.increment_usage();
            }
            if (instr.src2 != null) {
                instr.src2.?.increment_usage();
            }
            if (instr.data == .lval_list) {
                for (instr.data.lval_list.items) |lval| {
                    lval.increment_usage();
                }
            }
            if (instr.data == .call) {
                for (instr.data.call.arg_lval_list.items) |lval| {
                    lval.increment_usage();
                }
                for (instr.data.call.context_arg_lval_list.items) |lval| {
                    lval.increment_usage();
                }
            }
            if (instr.data == .invoke) {
                if (instr.data.invoke.dyn_value != null) {
                    instr.data.invoke.dyn_value.?.increment_usage();
                }
                if (instr.data.invoke.method_decl_lval != null and !instr.data.invoke.method_decl.method_decl.is_virtual) {
                    instr.data.invoke.method_decl_lval.?.increment_usage();
                }
                for (instr.data.invoke.arg_lval_list.items) |lval| {
                    lval.increment_usage();
                }
                for (instr.data.invoke.context_arg_lval_list.items) |lval| {
                    lval.increment_usage();
                }
            }
        }

        // Conditions are used
        if (bb.terminator == .conditional) {
            bb.terminator.conditional.condition.extract_symbver().uses += 1;
            bb.terminator.conditional.condition.extract_symbver().symbol.uses += 1;
        }
    }
}

pub fn calculate_definitions(self: *Self) void {
    // FIXME: High Cyclo
    for (self.basic_blocks.items) |bb| {
        // Reset all reachable symbol verison counts to 0
        for (bb.instructions.items) |instr| {
            reset_defs(instr.dest);
            reset_defs(instr.src1);
            reset_defs(instr.src2);
            if (instr.data == .lval_list) {
                for (instr.data.lval_list.items) |lval| {
                    reset_defs(lval);
                }
            }
            if (instr.data == .call) {
                for (instr.data.call.arg_lval_list.items) |lval| {
                    reset_defs(lval);
                }
                for (instr.data.call.context_arg_lval_list.items) |lval| {
                    reset_defs(lval);
                }
            }
            if (instr.data == .invoke) {
                reset_defs(instr.data.invoke.dyn_value);
                reset_defs(instr.data.invoke.method_decl_lval);
                for (instr.data.invoke.arg_lval_list.items) |lval| {
                    reset_defs(lval);
                }
                for (instr.data.invoke.context_arg_lval_list.items) |lval| {
                    reset_defs(lval);
                }
            }
        }
        self.return_symbol.defs = 0;
    }

    for (self.basic_blocks.items) |bb| {
        // Go through sum up each definition
        for (bb.instructions.items) |instr| {
            if (instr.dest != null) {
                var symbver = instr.dest.?.extract_symbver();
                symbver.set_def(instr);
            }
            if (instr.dest != null) {
                instr.dest.?.extract_symbver().symbol.roots += 1;
            }
            if (instr.kind == .mut_addr_of) {
                instr.src1.?.extract_symbver().symbol.aliases += 1;
            }
        }
    }
}

pub fn reset_defs(maybe_lval: ?*lval_.L_Value) void {
    if (maybe_lval == null) {
        return;
    }

    switch (maybe_lval.?.*) {
        .symbver => {
            maybe_lval.?.symbver.symbol.defs = 0;
        },
        .dereference => reset_defs(maybe_lval.?.dereference.expr),
        .index => {
            reset_defs(maybe_lval.?.index.length);
            reset_defs(maybe_lval.?.index.lhs);
            reset_defs(maybe_lval.?.index.rhs);
        },
        .select => {
            reset_defs(maybe_lval.?.select.tag);
            reset_defs(maybe_lval.?.select.lhs);
        },
        .raw_address => std.debug.panic("compiler error: undefined reset_defs for raw_address", .{}),
    }
}

pub fn count_bb_predecessors(self: *Self) void {
    // Reset all basic block predecessors to 0
    for (self.basic_blocks.items) |bb| {
        bb.number_predecessors = 0;
    }
    self.clear_visited_BBs();
    if (self.block_graph_head) |head| {
        // start predecessor count walk at head
        head.count_predecessors();
    }
}

pub fn remove_basic_block(self: *Self, bb: *Basic_Block) void {
    _ = self.basic_blocks.swapRemove(self.index_of_basic_block(bb));
}

fn index_of_basic_block(self: *Self, bb: *Basic_Block) usize {
    for (0..self.basic_blocks.items.len) |i| {
        if (bb == self.basic_blocks.items[i]) {
            return i;
        }
    } else {
        unreachable;
    }
}

/// Flattens all CFG's instructions to the module's list of instructions, recursively.
///
/// This also then adds the CFG to the list of CFGs in this module.
///
/// Also sets the `offset` flag of a CFG, which is the address in the instructions list that the
/// instructions for the CFG start.
///
/// Returns the index in the cfg in the cfgs list.
pub fn emplace_cfg(self: *Self, module_uid: u32, cfgs: *std.array_list.Managed(*Self), instructions_list: *std.array_list.Managed(*Instruction)) i64 {
    const len = @as(i64, @intCast(cfgs.items.len));
    if (self.offset_table.contains(module_uid)) {
        // Already visited, do nothing
        return len;
    } else if (self.block_graph_head == null) {
        // CFG doesn't have any real instructions. Insert phony BB.
        self.offset_table.put(module_uid, self.append_phony_block(instructions_list)) catch unreachable;
        cfgs.append(self) catch unreachable;
    } else {
        // Normal CFG with instructions, append BBs to instructions list, recursively append children
        self.offset_table.put(module_uid, self.append_basic_block(self.block_graph_head.?, module_uid, instructions_list)) catch unreachable;
        cfgs.append(self) catch unreachable;

        for (self.children.keys()) |child| {
            // TODO: The cfgs list should be a struct that wraps the list, and likely has entries that abstract the cfg's and bb's offset
            if (child.symbol.decl.?.num_generic_params() > 0) continue;
            if (child.symbol.decl.?.* == .method_decl and child.symbol.decl.?.method_decl.impl.?.num_generic_params() > 0) continue;
            _ = child.emplace_cfg(module_uid, cfgs, instructions_list);
        }
    }
    self.locals_size = self.calculate_offsets();
    return len;
}

/// Appends the instructions in a BasicBlock to the module's instructions.
/// Returns the offset of the basic block
fn append_basic_block(self: *Self, first_bb: *Basic_Block, module_uid: u32, instructions_list: *std.array_list.Managed(*Instruction)) Instruction.Index {
    var work_queue = std.array_list.Managed(*Basic_Block).init(self.allocator);
    defer work_queue.deinit();
    work_queue.append(first_bb) catch unreachable;

    while (work_queue.items.len > 0) {
        var bb = work_queue.orderedRemove(0); // Youch! Does this really have to be ordered?

        if (bb.offset_table.contains(module_uid)) {
            continue;
        }

        var label = Instruction.init_label("bb", Span.phony, self.allocator);
        label.uid = bb.uid;
        instructions_list.append(label) catch unreachable;

        bb.append_instructions(module_uid, instructions_list, &work_queue);
    }
    return first_bb.offset_table.get(module_uid).?;
}

/// This function inserts a label and a return instruction. It is needed for functions which do not have
/// instructions. The label is needed so that codegen_ can know there is a new function, and the return
/// instruction is for interpreting so that jumping to the function won't jump to some random function.
fn append_phony_block(self: *Self, instructions_list: *std.array_list.Managed(*Instruction)) Instruction.Index {
    const offset = @as(Instruction.Index, @intCast(instructions_list.items.len));
    // Append a label which has a back-reference to the CFG
    instructions_list.append(Instruction.init_label(
        "phony",
        Span.phony,
        self.allocator,
    )) catch unreachable;
    // Append a return instruction (a jump to null)
    instructions_list.append(Instruction.init_jump_addr(
        null,
        Span.phony,
        self.allocator,
    )) catch unreachable;
    return offset;
}

pub fn get_adjacent(self: *Self) []*Self {
    return self.children.keys();
}

/// Fills in a CFG's local offsets
///
/// ## The Orange interpreter calling convention
/// (stack grows up btw, lmao)
/// ```
///       |         |
/// sp -> |         |
///       | ------- | \
///       | local 2 |  |
///       | ------- |  |
///       | local 1 |  |
/// bp -> | ret-adr |  | - callee stack frame
///    -8 | prev-bp |  |
///   -16 | prev-sp |  |
///   -24 | &retval |  | \
///       | ------- |  |  |
///       | - pad - |  |  |
///       | ------- |  |  |
///   -28 | arg 1   |  |  |
///       | arg 2   |  |  |
///   -30 | arg 2   | /   | - caller stack frame
///       | - pad - |     |
///   -32 | arg 3   |     |
///       | - pad - |     |
///       | local 2 |     |
/// ```
///
/// - Entry (caller)
///   * (save sp, will be pushed later)
///   - push args in reverse order
///   - push address to return-value slot
///   - `call` instruction
///     * push pre-arg sp
///     * push bp
///     * push ret-addr
///     * bp := sp
///   - sp += n  (allocate space for locals)
///
/// - Exit (callee)
///   - sp := bp   (deallocate locals)
///   - `ret` instruction
///     * ip := pop  (set ip to ret-addr)
///     * bp := pop  (set bp to prev-bp)
///     * sp := pop  (deallocate all params)
///
/// There is padding before, in between, and after parameters, locals, and tuple fields so that each
/// location is aligned to a multiple of it's size in bytes.
pub fn calculate_offsets(self: *Self) i64 //< Number of bytes used for locals by the function.
{
    self.return_symbol.offset = null; // return value is set using an out-parameter

    // Calculate parameters offsets, descending from retval address offset
    var phony_sp: i64 = 0;
    if (self.symbol.decl.?.* == .fn_decl or self.symbol.decl.?.* == .method_decl) {
        const param_symbols = self.symbol.decl.?.param_symbols().?.items;
        // Go through params, as if we were pushing them
        var i: i64 = @as(i64, @intCast(param_symbols.len)) - 1;
        while (i >= 0) : (i -= 1) {
            var param: *Symbol = param_symbols[@as(usize, @intCast(i))];
            const size = param.expanded_type().sizeof();
            const alignof = param.expanded_type().alignof();
            phony_sp = alignment_.next_alignment(phony_sp, alignof);
            param.offset = phony_sp;
            phony_sp += size;
        }

        // Push a "header" word of alignment 8. Pretend this has the offset of the header. Adjust the offsets
        // set before accordingly
        const header = alignment_.next_alignment(phony_sp, 8);
        // Have to do this stupid round-a-bout way because we don't know how much padding to include after the
        // parameters until we get the offsets of each parameter.
        for (param_symbols) |param| {
            param.offset.? -= header - retval_offset;
        }
    }

    // Calculate locals offsets, ascending from local starting offset
    var local_offsets: i64 = locals_starting_offset;
    for (self.symbvers.items) |symbver| {
        if (symbver.symbol.offset == null and !std.mem.eql(u8, symbver.symbol.name, "_retval")) {
            local_offsets = alignment_.next_alignment(local_offsets, symbver.symbol.expanded_type().alignof());
            local_offsets += symbver.symbol.set_offset(local_offsets);
        }
    }
    local_offsets = alignment_.next_alignment(local_offsets, 8);

    // The total number of bytes used for locals
    return local_offsets - locals_starting_offset;
}

pub fn print_instructions(self: *Self) void {
    // Print out the basic blocks
    std.debug.print("CFG {s}:\n", .{self.symbol.name});
    for (self.basic_blocks.items) |bb| {
        bb.pprint();
    }
}
