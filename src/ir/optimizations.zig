const std = @import("std");
const Basic_Block = @import("basic-block.zig");
const CFG = @import("../ir/cfg.zig");
const errs_ = @import("../util/errors.zig");
const Instruction = @import("../ir/instruction.zig");
const Symbol_Version = @import("symbol_version.zig");

var debug = false;

pub fn optimize(cfg: *CFG, errors: *errs_.Errors, allocator: std.mem.Allocator) error{CompileError}!void {
    // debug = std.mem.eql(u8, cfg.symbol.name, "main");
    if (debug) {
        std.debug.print("[  CFG  ]: {s}\n", .{cfg.symbol.name});
        cfg.block_graph_head.?.pprint();
        log("\n\n");
    }

    while (try propagate(cfg, errors, allocator) or
        remove_unused_defs(cfg) or
        bb_optimizations(cfg, allocator) or
        remove_unused_defs(cfg))
    {}
    cfg.clear_visited_BBs();

    log_optimization_pass("final", cfg);
}

fn propagate(cfg: *CFG, errors: *errs_.Errors, allocator: std.mem.Allocator) error{CompileError}!bool {
    var retval = false;

    cfg.calculate_definitions();
    cfg.calculate_usage();

    for (cfg.basic_blocks.items) |bb| {
        // For each BB, keep a map of symbol versions and their definitions
        var def_map = std.AutoArrayHashMap(*Symbol_Version, ?*Instruction).init(allocator);
        defer def_map.deinit();

        for (bb.instructions.items) |instr| {
            // Walk through Instruction in BB, update map of src1 and src2 defs
            const src1_def: ?*Instruction = if (instr.src1 != null and instr.src1.?.* == .symbver) def_map.get(instr.src1.?.symbver) orelse null else null;
            const src2_def: ?*Instruction = if (instr.src2 != null and instr.src2.?.* == .symbver) def_map.get(instr.src2.?.symbver) orelse null else null;
            retval = try propagate_instruction(instr, src1_def, src2_def, errors) or retval;
            log_optimization_pass("^", cfg);

            if (instr.dest != null and instr.dest.?.* != .symbver) {
                def_map.put(instr.dest.?.extract_symbver(), null) catch unreachable;
            } else if (instr.kind == .mut_addr_of) {
                def_map.put(instr.src1.?.extract_symbver(), null) catch unreachable;
            }
            if (instr.dest != null and instr.dest.?.* == .symbver) {
                def_map.put(instr.dest.?.symbver, instr) catch unreachable;
            }
        }
        bb.remove_marked_instrs();
        if (bb.terminator == .conditional and bb.terminator.conditional.condition.* == .symbver) {
            const cond_def: ?*Instruction = def_map.get(bb.terminator.conditional.condition.symbver) orelse null;
            bb.terminator.conditional.condition.symbver.set_def(cond_def);
            if (cond_def != null and cond_def.?.kind == .copy and cond_def.?.src1 != null) {
                bb.terminator.conditional.condition = cond_def.?.src1.?;
                retval = true;
            }
        }
    }

    return retval;
}

/// May throw `error.CompileError` for integer arithmetic overflow, or `error.CompileError` when it can be
/// proven that code would divide by zero.
fn propagate_instruction(instr: *Instruction, src1_def: ?*Instruction, src2_def: ?*Instruction, errors: *errs_.Errors) error{CompileError}!bool {
    var retval = false;

    switch (instr.kind) {
        .copy => {
            if (instr.src1 == null or (instr.src1.?.expanded_type_sizeof() == 0)) {
                logfln("unit-copy elimination {?f} {f}", .{ instr.src1, instr.src1.?.get_expanded_type() });
                instr.in_block.?.mark_for_removal(instr); // Mark for deletion in a later pass
            } else if (instr.dest.?.* == .symbver and
                instr.src1.?.* == .symbver and
                instr.dest.?.symbver.symbol == instr.src1.?.symbver.symbol and
                src1_def != null)
            {
                log("self-copy elimination");
                instr.in_block.?.mark_for_removal(instr); // Mark for deletion in a later pass
                retval = true;
            } else {
                logfln("copy-propagation {f}", .{instr});
                retval = try instr.copy_prop(src1_def, .load_int, errors) or // TODO: Check if data fits int type bounds!
                    try instr.copy_prop(src1_def, .load_float, errors) or
                    try instr.copy_prop(src1_def, .load_string, errors) or
                    (instr.src1.?.* == .symbver and instr.src1.?.symbver.uses == 1 and try instr.copy_prop(src1_def, .load_struct, errors)) or
                    try instr.copy_prop(src1_def, .load_union, errors) or
                    try instr.copy_prop(src1_def, .load_symbol, errors) or
                    (try instr.copy_prop(src1_def, .copy, errors) and instr.src1 != src1_def.?.src1.?) or
                    try instr.copy_prop(src1_def, .addr_of, errors) or
                    try instr.copy_prop(src1_def, .mut_addr_of, errors) or
                    try instr.copy_prop(src1_def, .add_int, errors) or try instr.copy_prop(src1_def, .add_float, errors) or
                    try instr.copy_prop(src1_def, .sub_int, errors) or try instr.copy_prop(src1_def, .sub_float, errors) or
                    try instr.copy_prop(src1_def, .mult_int, errors) or try instr.copy_prop(src1_def, .mult_float, errors) or
                    try instr.copy_prop(src1_def, .div_int, errors) or try instr.copy_prop(src1_def, .div_float, errors) or
                    try instr.copy_prop(src1_def, .mod, errors);
                // DO NOT IMPLEMENT .call => propagation
            }
        },

        .not => {
            if (src1_def != null and src1_def.?.kind == .load_int) {
                log("not; known int value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.int == 0) 1 else 0 }, errors);
                retval = true;
            }
        },

        .negate_int => {
            if (src1_def != null and src1_def.?.kind == .load_int) {
                log("negate_int; known value");
                try instr.convert_to_load(.load_int, .{ .int = -src1_def.?.data.int }, errors);
                retval = true;
            }
        },

        .negate_float => {
            if (src1_def != null and src1_def.?.kind == .load_float) {
                log("negate_float; known value");
                try instr.convert_to_load(.load_float, .{ .float = -src1_def.?.data.float }, errors);
                retval = true;
            }
        },

        .equal => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("equal; known int,int value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.int == src2_def.?.data.int) 1 else 0 }, errors);
                retval = true;
            } else if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float) {
                log("equal; known float,float value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.float == src2_def.?.data.float) 1 else 0 }, errors);
                retval = true;
            } else if (instr.self_eq() and !instr.src1.?.symbver.symbol.expanded_type().can_expanded_represent_float()) {
                // `x == x => true (where x is NOT floating-point type)`
                // NOTE: Cannot do `x == x  ==> true` optimization for floats, `NaN == NaN` is false!
                log("equal; self inequality");
                try instr.convert_to_load(.load_int, .{ .int = 1 }, errors);
                retval = true;
            } else if (instr.src1.?.get_expanded_type().is_c_void_type() and instr.src2.?.get_expanded_type().is_c_void_type()) {
                log("equal; known unit,unit value");
                try instr.convert_to_load(.load_int, .{ .int = 1 }, errors);
                retval = true;
            }
        },

        .not_equal => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("not_equal; known int,int value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.int != src2_def.?.data.int) 1 else 0 }, errors);
                retval = true;
            } else if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float) {
                log("not_equal; known float,float value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.float != src2_def.?.data.float) 1 else 0 }, errors);
                retval = true;
            } else if (instr.self_eq() and !instr.src1.?.symbver.symbol.expanded_type().can_expanded_represent_float()) {
                // `x != x` => `false`
                // NOTE: Cannot do `x != x  ==> true` optimization for floats, `NaN == NaN` is true!
                log("not_equal; self inequality");
                try instr.convert_to_load(.load_int, .{ .int = 0 }, errors);
                retval = true;
            } else if (instr.src1.?.get_expanded_type().is_c_void_type() and instr.src2.?.get_expanded_type().is_c_void_type()) {
                log("equal; known unit,unit value");
                try instr.convert_to_load(.load_int, .{ .int = 0 }, errors);
                retval = true;
            }
        },

        .greater_int => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("greater_int; known int,int value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.int > src2_def.?.data.int) 1 else 0 }, errors);
                retval = true;
            } else if (instr.self_eq()) {
                // `x > x` => `false`
                log("greater_int; self compare");
                try instr.convert_to_load(.load_int, .{ .int = 0 }, errors);
                retval = true;
            }
        },

        .greater_float => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float) {
                log("greater_float; known float,float value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.float > src2_def.?.data.float) 1 else 0 }, errors);
                retval = true;
            } else if (instr.self_eq()) {
                // `x > x` => `false`
                log("greater_float; self compare");
                try instr.convert_to_load(.load_int, .{ .int = 0 }, errors);
                retval = true;
            }
        },

        .lesser_int => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("lesser_int; known int,int value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.int < src2_def.?.data.int) 1 else 0 }, errors);
                retval = true;
            } else if (instr.self_eq()) {
                // `x < x` => `false`
                log("lesser_int; self compare");
                try instr.convert_to_load(.load_int, .{ .int = 0 }, errors);
                retval = true;
            }
        },

        .lesser_float => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float) {
                log("lesser_float; known int,int value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.float < src2_def.?.data.float) 1 else 0 }, errors);
                retval = true;
            } else if (instr.self_eq()) {
                // `x < x` => `false`
                log("lesser_float; self compare");
                try instr.convert_to_load(.load_int, .{ .int = 0 }, errors);
                retval = true;
            }
        },

        .greater_equal_int => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("greater_equal_int; known int,int value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.int >= src2_def.?.data.int) 1 else 0 }, errors);
                retval = true;
            } else if (instr.self_eq()) {
                // `x >= x` => `true`
                log("greater_equal_int; self compare");
                try instr.convert_to_load(.load_int, .{ .int = 1 }, errors);
                retval = true;
            }
        },

        .greater_equal_float => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float) {
                log("greater_equal_float; known float,float value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.float >= src2_def.?.data.float) 1 else 0 }, errors);
                retval = true;
            }
            // NOTE: Cannot do `x >= x  ==> true` optimization for floats, `NaN >= NaN` is false!
        },

        .lesser_equal_int => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("lesser_equal_int; known int,int value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.int <= src2_def.?.data.int) 1 else 0 }, errors);
                retval = true;
            } else if (instr.self_eq()) {
                // `x <= x` => `true`
                log("lesser_equal_int; self compare");
                try instr.convert_to_load(.load_int, .{ .int = 1 }, errors);
                retval = true;
            }
        },

        .lesser_equal_float => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float) {
                log("lesser_equal_float; known float,float value");
                try instr.convert_to_load(.load_int, .{ .int = if (src1_def.?.data.float <= src2_def.?.data.float) 1 else 0 }, errors);
                retval = true;
            }
            // NOTE: Cannot do `x <= x  ==> true` optimization, `NaN <= NaN` is false!
        },

        .add_int => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("add_int; known int,int value");
                try instr.convert_to_load(.load_int, try src1_def.?.data.add_int_overflow(src2_def.?.data), errors);
                retval = true;
            } else if (src1_def != null and src1_def.?.kind == .load_int and src1_def.?.data.int == 0) {
                log("add_int; add 0 lhs");
                instr.convert_to_unop(instr.src2.?, .copy);
                retval = true;
            } else if (src2_def != null and src2_def.?.kind == .load_int and src2_def.?.data.int == 0) {
                log("add_int; add 0 rhs");
                instr.convert_to_unop(instr.src1.?, .copy);
                retval = true;
            }
        },

        .add_float => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float) {
                log("add_float; known float,float value");
                try instr.convert_to_load(.load_float, .{ .float = src1_def.?.data.float + src2_def.?.data.float }, errors);
                retval = true;
            } else if (src1_def != null and src1_def.?.kind == .load_float and src1_def.?.data.float == 0.0) {
                log("add_float; add 0.0 lhs");
                instr.convert_to_unop(instr.src2.?, .copy);
                retval = true;
            } else if (src2_def != null and src2_def.?.kind == .load_float and src2_def.?.data.float == 0.0) {
                log("add_float; add 0.0 rhs");
                instr.convert_to_unop(instr.src1.?, .copy);
                retval = true;
            }
        },

        .sub_int => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("sub; known int,int value");
                try instr.convert_to_load(.load_int, try src1_def.?.data.sub_int_overflow(src2_def.?.data), errors);
                retval = true;
            } else if (src1_def != null and src1_def.?.kind == .load_int and src1_def.?.data.int == 0) {
                log("sub; sub 0 lhs");
                instr.convert_to_unop(instr.src2.?, .negate_int);
                retval = true;
            } else if (src2_def != null and src2_def.?.kind == .load_int and src2_def.?.data.int == 0) {
                log("sub; sub 0 rhs");
                instr.convert_to_unop(instr.src1.?, .copy);
                retval = true;
            }
        },

        .sub_float => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float) {
                log("sub; known float,float value");
                try instr.convert_to_load(.load_float, .{ .float = src1_def.?.data.float - src2_def.?.data.float }, errors);
                retval = true;
            } else if (src1_def != null and src1_def.?.kind == .load_float and src1_def.?.data.float == 0.0) {
                log("sub; sub 0.0 lhs");
                instr.convert_to_unop(instr.src2.?, .negate_float);
                retval = true;
            } else if (src2_def != null and src2_def.?.kind == .load_float and src2_def.?.data.float == 0.0) {
                log("sub; sub 0.0 rhs");
                instr.convert_to_unop(instr.src1.?, .copy);
                retval = true;
            }
        },

        .mult_int => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("mult_int; known int,int value");
                try instr.convert_to_load(.load_int, try src1_def.?.data.mult_int_overflow(src2_def.?.data), errors);
                retval = true;
            } else if (src1_def != null and src1_def.?.kind == .load_int and src1_def.?.data.int == 1) {
                log("mult; mult 1 lhs");
                instr.convert_to_unop(instr.src2.?, .copy);
                retval = true;
            } else if (src2_def != null and src2_def.?.kind == .load_int and src2_def.?.data.int == 1) {
                log("mult; mult 1 rhs");
                instr.convert_to_unop(instr.src1.?, .copy);
                retval = true;
            } else if ((src1_def != null and src1_def.?.kind == .load_int and src1_def.?.data.int == 0) or
                (src2_def != null and src2_def.?.kind == .load_int and src2_def.?.data.int == 0))
            {
                log("mult; mult 0 lhs int");
                try instr.convert_to_load(.load_int, .{ .int = 0 }, errors);
                retval = true;
            }
        },

        .mult_float => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float) {
                log("mult_float; known float,float value");
                try instr.convert_to_load(.load_float, .{ .float = src1_def.?.data.float * src2_def.?.data.float }, errors);
                retval = true;
            } else if (src1_def != null and src1_def.?.kind == .load_float and src1_def.?.data.float == 1.0) {
                log("mult; mult 1.0 lhs");
                instr.convert_to_unop(instr.src2.?, .copy);
                retval = true;
            } else if (src2_def != null and src2_def.?.kind == .load_float and src2_def.?.data.float == 1.0) {
                log("mult; mult 1.0 rhs");
                instr.convert_to_unop(instr.src1.?, .copy);
                retval = true;
            } else if ((src1_def != null and src1_def.?.kind == .load_float and src1_def.?.data.float == 0.0) or
                (src2_def != null and src2_def.?.kind == .load_float and src2_def.?.data.float == 0.0))
            {
                log("mult; mult 0.0 lhs float");
                try instr.convert_to_load(.load_float, .{ .float = 0.0 }, errors);
                retval = true;
            }
        },

        .div_int => {
            try divide_by_zero_check(src2_def, errors); // Static check; divide by zero
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("div_int; known int value");
                try instr.convert_to_load(.load_int, .{ .int = @divTrunc(src1_def.?.data.int, src2_def.?.data.int) }, errors);
                retval = true;
            } else if (src2_def != null and src2_def.?.kind == .load_int and src2_def.?.data.int == 1) {
                log("div; div 1 rhs");
                instr.convert_to_unop(instr.src1.?, .copy);
                retval = true;
            }
        },

        .div_float => {
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_float and src2_def.?.kind == .load_float and src2_def.?.data.float != 0.0) {
                log("div_float; known float value");
                try instr.convert_to_load(.load_float, .{ .float = src1_def.?.data.float / src2_def.?.data.float }, errors);
                retval = true;
            } else if (src2_def != null and src2_def.?.kind == .load_float and src2_def.?.data.float == 1.0) {
                log("div; div 1.0 rhs");
                instr.convert_to_unop(instr.src1.?, .copy);
                retval = true;
            }
        },

        .mod => {
            try divide_by_zero_check(src2_def, errors); // Static check; divide by zero
            if (src1_def != null and src2_def != null and src1_def.?.kind == .load_int and src2_def.?.kind == .load_int) {
                log("mod; known int value");
                try instr.convert_to_load(.load_int, .{ .int = @rem(src1_def.?.data.int, src2_def.?.data.int) }, errors);
                retval = true;
            } else if ((src1_def != null and src1_def.?.kind == .load_int and src1_def.?.data.int == 0) or
                (src2_def != null and src2_def.?.kind == .load_int and src2_def.?.data.int == 1))
            {
                log("mod; mod 0 lhs");
                try instr.convert_to_load(.load_int, .{ .int = 0 }, errors);
                retval = true;
            }
        },

        .get_tag => {
            if (src1_def != null and src1_def.?.kind == .load_union) {
                log("get_tag; known tag");
                try instr.convert_to_load(.load_int, .{ .int = src1_def.?.data.int }, errors); // Copy the src's tag (in data.int)
                retval = true;
            }
        },

        else => {},
    }

    retval = instr.copy_of_prop(&instr.src1, src1_def) or retval;
    retval = instr.copy_of_prop(&instr.src2, src2_def) or retval;

    return retval;
}

/// Checks if a div_int or mod Instruction divides by zero
fn divide_by_zero_check(instr: ?*Instruction, errors: *errs_.Errors) error{CompileError}!void {
    if (instr != null) {
        if (instr.?.kind == .load_int and instr.?.data.int == 0) {
            errors.add_error(errs_.Error{ .basic = .{
                .span = instr.?.span,
                .msg = "divide by 0",
            } });
            return error.CompileError;
        }
    }
}

/// Removes unused definitions from the CFG, skipping calls and invokes (to avoid removing instructions with side
/// effects). Returns true if any instructions were removed.
fn remove_unused_defs(cfg: *CFG) bool {
    var retval = false;

    cfg.calculate_usage();
    cfg.calculate_definitions();

    for (cfg.basic_blocks.items) |bb| {
        for (bb.instructions.items) |instr| {
            if (instr.dest != null and
                instr.dest.?.* == .symbver and
                instr.dest.?.symbver.uses == 0 and
                instr.dest.?.symbver.symbol.uses == 0 and
                instr.dest.?.symbver.symbol != cfg.return_symbol)
            {
                logfln("removing: {f}", .{instr});
                if (instr.kind != .call and instr.kind != .invoke) { // Calls and invokes may have side-effects, do not remove them!
                    instr.in_block.?.mark_for_removal(instr);
                    retval = true;
                }
            }
        }
        bb.remove_marked_instrs();
    }

    return retval;
}

/// Performs various Basic Block optimizations in a CFG. Returns true if any optimizations are made.
/// TODO: Split up optimization checks into their own functions
fn bb_optimizations(cfg: *CFG, allocator: std.mem.Allocator) bool {
    // FIXME: High Cyclo
    var retval: bool = false;

    cfg.count_bb_predecessors();
    cfg.calculate_definitions();

    for (cfg.basic_blocks.items) |bb| {
        if (bb.number_predecessors == 0) {
            defer log_optimization_pass("remove unused block", cfg);
            cfg.remove_basic_block(bb);
            bb.mark_all_instructions_as_removed();
            return true; // Perhaps mark these and remove them in a sweep pass?
        }
    }

    for (cfg.basic_blocks.items) |bb| {
        // TODO: Too long
        // Adopt basic blocks with only one incoming block
        if (bb.terminator == .unconditional and !bb.empty() and bb.terminator.unconditional != null and bb.terminator.unconditional.?.number_predecessors == 1) {
            var log_msg = std.array_list.Managed(u8).init(allocator);
            defer log_msg.deinit();
            log_msg.print("adopt BB{} into BB{}", .{ bb.terminator.unconditional.?.uid, bb.uid }) catch unreachable;
            defer log_optimization_pass(log_msg.items, cfg);
            cfg.remove_basic_block(bb.terminator.unconditional.?);
            bb.merge_with(bb.terminator.unconditional.?);
            retval = true;
            break;
        }

        if (bb.terminator == .conditional and bb.terminator.conditional.condition.* == .symbver) {
            const latest_condition = bb.terminator.conditional.condition.symbver.def; // This is set by `propagate()`
            // Convert constant true/false branches to jumps
            if (latest_condition != null and latest_condition.?.kind == .load_int) {
                defer log_optimization_pass("convert constant true/false to jumps", cfg);
                if (bb.terminator.conditional.condition.symbver.def.?.data.int == 0) {
                    const new_terminator: Basic_Block.Terminator = .{ .unconditional = bb.terminator.conditional.false_target };
                    bb.terminator = new_terminator;
                } else {
                    const new_terminator: Basic_Block.Terminator = .{ .unconditional = bb.terminator.conditional.true_target };
                    bb.terminator = new_terminator;
                }
                retval = true;
            }
            // Flip labels if branch condition is negation, plunge negation
            else if (latest_condition != null and latest_condition.?.kind == .not and latest_condition.?.src1.?.* == .symbver and
                latest_condition.?.src1.?.symbver.def != null // prevents a loop if a function parameter (which are without a def) is neg'd
            ) {
                defer log_optimization_pass("flip not condition", cfg);
                const old_branch = bb.terminator.conditional.false_target;
                bb.terminator.conditional.false_target = bb.terminator.conditional.true_target;
                bb.terminator.conditional.true_target = old_branch;
                bb.terminator.conditional.condition = latest_condition.?.src1.?;
                retval = true;
            }
        }

        // Remove jump chains.
        // Replace:
        //   BB1():
        //       ...
        //       jump BB2()
        //   BB2():
        //       jump BB3()
        // with:
        //   BB1():
        //       jump BB3()
        if (bb.terminator == .unconditional and bb.terminator.unconditional != null) {
            const next_bb = bb.terminator.unconditional.?;
            if (next_bb.empty() and
                next_bb.terminator == .unconditional and
                next_bb != bb and
                next_bb.terminator.unconditional != bb and
                (next_bb.terminator.unconditional == null or (next_bb.terminator.unconditional.?.terminator == .unconditional and next_bb.terminator.unconditional.?.terminator.unconditional != next_bb)))
            {
                var s = std.array_list.Managed(u8).init(allocator);
                s.print("remove jump chain BB{}", .{next_bb.uid}) catch unreachable;
                defer s.deinit();
                defer log_optimization_pass(s.items, cfg);
                bb.terminator.unconditional = next_bb.terminator.unconditional;
                retval = true;
            }
        } // TODO: Generalize and do for unconditional jumps too

        if (bb.terminator == .conditional and bb.terminator.conditional.false_target != null) {
            const branch = bb.terminator.conditional.false_target.?;
            if (branch.empty() and branch.terminator == .unconditional) {
                bb.terminator.conditional.false_target = branch.terminator.unconditional;
                retval = true;
            }
        }

        // If a conditional branch has the same target for both true and false, convert to unconditional
        if (bb.terminator == .conditional and bb.terminator.conditional.true_target == bb.terminator.conditional.false_target) {
            bb.terminator = .{ .unconditional = bb.terminator.conditional.true_target };
            retval = true;
        }

        // If next is a branch that depends on a known arugment
        if (bb.terminator == .unconditional and bb.terminator.unconditional != null) {
            const next = bb.terminator.unconditional.?;
            if (next.terminator == .conditional and next.empty() and next.terminator.conditional.condition.* == .symbver) {
                defer log_optimization_pass("next depends on known argument", cfg);
                const def = bb.get_latest_def(next.terminator.conditional.condition, null);
                if (def != null and def.?.kind == .load_int) {
                    if (def.?.data.int == 0) {
                        bb.terminator.unconditional = next.terminator.conditional.false_target;
                    } else {
                        bb.terminator.unconditional = next.terminator.conditional.true_target;
                    }
                }
            }
        }

        // If branch is a branch that depends on a known arugment
        if (bb.terminator == .conditional and
            bb.terminator.conditional.false_target != null and
            bb.terminator.conditional.false_target.?.terminator == .conditional and
            bb.terminator.conditional.false_target.?.empty() and
            bb.terminator.conditional.false_target.?.terminator.conditional.condition.* == .symbver)
        {
            defer log_optimization_pass("branch depends on known argument", cfg);
            const def = bb.get_latest_def(bb.terminator.conditional.false_target.?.terminator.conditional.condition, null);
            if (def != null and def.?.kind == .load_int) {
                if (def.?.data.int == 0) {
                    bb.terminator.conditional.false_target = bb.terminator.conditional.false_target.?.terminator.conditional.false_target;
                } else {
                    bb.terminator.conditional.false_target = bb.terminator.conditional.false_target.?.terminator.conditional.true_target;
                }
            }
        }
    }

    // Rebase block graph if jump chain
    if (cfg.block_graph_head) |head| {
        // TODO: For this optimization to truly be effective, we'll need to see that `head` is not apart of its precedessor set
        // It's not a very important optimization though, totally cosmetic
        if (head.empty() and head.terminator == .unconditional and head.terminator.unconditional != head and head.number_predecessors == 0) {
            defer log_optimization_pass("rebase block", cfg);
            cfg.block_graph_head = head.terminator.unconditional;
            retval = true;
        }
    }

    return retval;
}

// TODO: Move this out, and into a proper logging system that's configurable from the cmd line
fn log_optimization_pass(msg: []const u8, cfg: *CFG) void {
    if (debug) {
        std.debug.print("[OPTIMIZATION] {s}\n", .{msg});
        if (cfg.block_graph_head) |block_head| {
            cfg.clear_visited_BBs();
            block_head.pprint();
            cfg.clear_visited_BBs();
            log("\n\n");
        } else {
            std.debug.print("[WARNING] block head for CFG: {s} is null\n", .{cfg.symbol.name});
        }

        var in_buffer: [256]u8 = undefined;
        _ = std.fs.File.stdin().read(&in_buffer) catch unreachable;
    }
}

fn log(msg: []const u8) void {
    if (debug) {
        std.debug.print("{s}\n", .{msg});
    }
}

fn logfln(comptime fmt: []const u8, args: anytype) void {
    if (debug) {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
    }
}
