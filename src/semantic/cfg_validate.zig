//! This file validates a CFG of Instruction. It checks for things like unused symbols, symbols marked `mut` that are
//! never mutated, etc.

const std = @import("std");
const CFG = @import("../ir/cfg.zig");
const errs_ = @import("../util/errors.zig");
const Instruction = @import("../ir/instruction.zig");
const lval_ = @import("../ir/lval.zig");
const Span = @import("../util/span.zig");
const Symbol = @import("../symbol/symbol.zig");

pub fn validate_cfg(cfg: *CFG, errors: *errs_.Errors) error{CompileError}!void {
    cfg.calculate_usage();
    cfg.calculate_definitions();

    if (cfg.symbol.decl.?.* == .fn_decl or cfg.symbol.decl.?.* == .method_decl) {
        const param_symbols = cfg.symbol.decl.?.param_symbols();
        if (param_symbols != null) {
            for (param_symbols.?.items) |param_symbol| {
                try param_symbol.err_if_unused(errors);
                try param_symbol.err_if_var_not_mutated(errors);
            }
        }
    }

    for (cfg.basic_blocks.items) |bb| {
        for (bb.instructions.items) |instr| {
            if (instr.dest != null and // has a dest symbol to test
                instr.dest.?.* == .symbver and // dest is symbver
                !instr.dest.?.symbver.symbol.is_temp and // dest symbver's symbol isn't temporary
                instr.dest.?.symbver.symbol != cfg.return_symbol // dest symbver's symbol isn't the function's return value
            ) {
                try instr.dest.?.symbver.symbol.err_if_unused(errors);
                try instr.dest.?.symbver.symbol.err_if_var_not_mutated(errors);
            }
            try err_if_chain_undefd(instr.src1, errors, cfg.return_symbol, instr.span);
            try err_if_chain_undefd(instr.src2, errors, cfg.return_symbol, instr.span);
            if (instr.data == .lval_list) {
                for (instr.data.lval_list.items) |lval| {
                    try err_if_chain_undefd(lval, errors, cfg.return_symbol, instr.span);
                }
            }
            if (instr.data == .call) {
                for (instr.data.call.arg_lval_list.items) |lval| {
                    try err_if_chain_undefd(lval, errors, cfg.return_symbol, instr.span);
                }
                for (instr.data.call.ability_arg_lval_list.items) |lval| {
                    try err_if_chain_undefd(lval, errors, cfg.return_symbol, instr.span);
                }
            }
            if (instr.data == .invoke) {
                try err_if_chain_undefd(instr.data.invoke.dyn_value, errors, cfg.return_symbol, instr.span);
                for (instr.data.invoke.arg_lval_list.items) |lval| {
                    try err_if_chain_undefd(lval, errors, cfg.return_symbol, instr.span);
                }
                for (instr.data.invoke.ability_arg_lval_list.items) |lval| {
                    try err_if_chain_undefd(lval, errors, cfg.return_symbol, instr.span);
                }
            }
        }
    }
}

fn err_if_chain_undefd(maybe_lval: ?*lval_.L_Value, errors: *errs_.Errors, return_symbol: *Symbol, use: Span) error{CompileError}!void {
    if (maybe_lval == null) {
        return;
    }

    switch (maybe_lval.?.*) {
        .symbver => if (maybe_lval.?.symbver.symbol.lvalue_is_symbol(return_symbol)) {
            try maybe_lval.?.symbver.symbol.err_if_undefd(errors, use);
        },
        .dereference => try err_if_chain_undefd(maybe_lval.?.dereference.expr, errors, return_symbol, use),
        .index => {
            try err_if_chain_undefd(maybe_lval.?.index.length, errors, return_symbol, use);
            try err_if_chain_undefd(maybe_lval.?.index.lhs, errors, return_symbol, use);
            try err_if_chain_undefd(maybe_lval.?.index.rhs, errors, return_symbol, use);
        },
        .select => {
            try err_if_chain_undefd(maybe_lval.?.select.tag, errors, return_symbol, use);
            try err_if_chain_undefd(maybe_lval.?.select.lhs, errors, return_symbol, use);
        },
        .raw_address => std.debug.panic("compiler error: undefined err_if_chain_undefd for raw_address", .{}),
    }
}
