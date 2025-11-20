// Lowers an AST into a list of Instruction.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const CFG = @import("../ir/cfg.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const Interned_String_Set = @import("../ir/interned_string_set.zig");
const Instruction = @import("../ir/instruction.zig");
const lval_ = @import("lval.zig");
const prelude_ = @import("../hierarchy/prelude.zig");
const Span = @import("../util/span.zig");
const Symbol = @import("../symbol/symbol.zig");
const Symbol_Version = @import("symbol_version.zig");
const Token = @import("../lexer/token.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Type_AST = @import("../types/type.zig").Type_AST;

pub const Lower_Errors = error{CompileError};

const Self = @This();

ctx: *Compiler_Context,
instructions: std.array_list.Managed(*Instruction),
interned_strings: *Interned_String_Set,
cfg: *CFG,
number_temps: usize = 0,

const Labels = struct {
    return_label: ?*Instruction,
    break_label: ?*Instruction,
    continue_label: ?*Instruction,
    error_label: ?*Instruction,

    const null_labels: Labels = .{ .return_label = null, .break_label = null, .continue_label = null, .error_label = null };
};

pub fn init(ctx: *Compiler_Context, cfg: *CFG, interned_strings: *Interned_String_Set) Self {
    return Self{
        .instructions = std.array_list.Managed(*Instruction).init(ctx.allocator()),
        .interned_strings = interned_strings,
        .ctx = ctx,
        .cfg = cfg,
    };
}

pub fn lower_AST_into_cfg(self: *Self) Lower_Errors!void {
    const eval: ?*lval_.L_Value = try self.lower_AST(self.cfg.symbol.init_value().?, Labels.null_labels);
    if (self.cfg.symbol.decl.?.* == .fn_decl or self.cfg.symbol.decl.?.* == .method_decl) {
        // `_comptime` symbols don't have parameters anyway
        const param_symbols = self.cfg.symbol.decl.?.param_symbols();
        if (param_symbols != null) {
            for (param_symbols.?.items) |param| {
                self.cfg.parameters.append(Symbol_Version.create_unversioned(param, self.ctx.allocator())) catch unreachable;
            }
        }
    }
    const return_version = lval_.L_Value.create_unversioned_symbver(self.cfg.return_symbol, self.ctx.allocator());
    if (eval != null) {
        self.instructions.append(Instruction.init_simple_copy(return_version, eval.?, self.cfg.symbol.span(), self.ctx.allocator())) catch unreachable;
    }
    self.instructions.append(Instruction.init_jump(null, self.cfg.symbol.span(), self.ctx.allocator())) catch unreachable;

    if (false) {
        // Print symbol Instruction after lowering, before breaking up into basic blocks
        std.debug.print("CFG {s}: {f}\n", .{ self.cfg.symbol.name, self.cfg.symbol.type() });
        for (self.instructions.items) |instr| {
            std.debug.print("{f}", .{instr});
        }
        std.debug.print("\n", .{});
    }

    self.cfg.block_graph_head = self.cfg.basic_block_from_instructions(self.instructions, 0);

    if (false) {
        // Print out the basic blocks
        std.debug.print("CFG {s}:\n", .{self.cfg.symbol.name});
        for (self.cfg.basic_blocks.items) |bb| {
            bb.pprint();
        }
    }
}

fn lower_AST(self: *Self, ast: *ast_.AST, labels: Labels) Lower_Errors!?*lval_.L_Value {
    const retval = self.lower_AST_inner(ast, labels);
    if (false) {
        std.debug.print("{}\n", .{ast});
        // Print symbol Instruction after lowering, before breaking up into basic blocks
        std.debug.print("CFG {s}:\n", .{self.cfg.symbol.name});
        for (self.instructions.items) |instr| {
            std.debug.print("{}", .{instr});
        }
        std.debug.print("\n", .{});
    }
    return retval;
}

fn lower_AST_inner(
    self: *Self,
    ast: *ast_.AST,
    labels: Labels,
) Lower_Errors!?*lval_.L_Value {
    switch (ast.*) {
        // Unit-values
        .enum_decl, .struct_decl, .unit_value, .trait, .impl, .type_alias => return self.lval_from_unit_value(ast),
        // Literals
        .int => return self.lval_from_int(ast.int.data, self.ctx.typecheck.typeof(ast), ast.token().span),
        .float => {
            const temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
            self.instructions.append(Instruction.init_float(temp, ast.float.data, ast.token().span, self.ctx.allocator())) catch unreachable;
            return temp;
        },
        .string => {
            const id: Interned_String_Set.String_Idx = self.interned_strings.add_string(ast.string.data, self.cfg.symbol.scope.module.?.uid);
            const temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
            self.instructions.append(Instruction.init_string(temp, id, ast.token().span, self.ctx.allocator())) catch unreachable;
            return temp;
        },
        .access, .identifier, .generic_apply => {
            const symbol = ast.symbol().?;
            if (symbol.init_validation_state == .validating) {
                self.ctx.errors.add_error(errs_.Error{ .recursive_definition = .{
                    .span = symbol.span(),
                    .symbol_name = symbol.name,
                } });
                return error.CompileError;
            }
            if (symbol.kind == .@"fn") {
                return try self.lval_from_symbol_cfg(symbol, ast.token().span);
            } else if (symbol.kind == .@"const" and symbol.storage != .@"extern") {
                return try self.lower_AST(symbol.init_value().?, labels);
            } else {
                const src = lval_.L_Value.create_unversioned_symbver(symbol, self.ctx.allocator());
                return src;
            }
        },
        .pattern_symbol => return try self.lval_from_symbol_cfg(ast.symbol().?, ast.token().span),
        .true => return self.lval_from_int(1, self.ctx.typecheck.typeof(ast), ast.token().span),
        .false => return self.lval_from_int(0, self.ctx.typecheck.typeof(ast), ast.token().span),
        // Unary operators
        .not, .negate, .addr_of, .bit_not => return try self.unop(ast, labels),
        .as => {
            const expr = try self.lower_AST(ast.expr(), labels) orelse return null;
            const expanded_type = self.ctx.typecheck.typeof(ast).expand_identifier();
            const temp = self.create_temp_lvalue(expanded_type);
            self.instructions.append(Instruction.init(.cast, temp, expr, null, ast.token().span, self.ctx.allocator())) catch unreachable;
            return temp;
        },
        .dereference => {
            const expr = try self.lower_AST(ast.expr(), labels) orelse return null;
            const expanded_type = self.ctx.typecheck.typeof(ast).expand_identifier();
            const temp = expr.create_dereference_lval(expanded_type, self.ctx.allocator());
            return temp;
        },
        .@"try" => {
            var expr = (try self.lower_AST(ast.expr(), labels)) orelse return null;

            const end_label = Instruction.init_label("try.end", ast.token().span, self.ctx.allocator());
            const ok_label = Instruction.init_label("try.ok", ast.token().span, self.ctx.allocator());

            var expanded_expr_type = expr.get_expanded_type();

            // Trying error sum, runtime check if ok, branch to ok path
            const condition = self.create_temp_lvalue(prelude_.word64_type);
            self.instructions.append(Instruction.init_get_tag(condition, expr, ast.token().span, self.ctx.allocator())) catch unreachable; // `ok` is zero, `err`s are nonzero
            self.instructions.append(Instruction.init_branch(condition, ok_label, ast.token().span, self.ctx.allocator())) catch unreachable;

            // Unwrap the `.err` value
            const err_type = expanded_expr_type.get_err_type().expand_identifier();
            const err_lval = expr.create_select_lval(1, 0, err_type, null, self.ctx.allocator());
            const full_err_lval = self.create_temp_lvalue(self.cfg.return_symbol.type());
            self.instructions.append(Instruction.init_union(full_err_lval, err_lval, 1, ast.token().span, self.ctx.allocator())) catch unreachable;
            const retval_lval = lval_.L_Value.create_unversioned_symbver(self.cfg.return_symbol, self.ctx.allocator());
            self.instructions.append(Instruction.init_simple_copy(retval_lval, full_err_lval, ast.token().span, self.ctx.allocator())) catch unreachable;
            self.instructions.append(Instruction.init_jump(labels.error_label, ast.token().span, self.ctx.allocator())) catch unreachable;

            // Else, unwrap the `.ok` value
            self.instructions.append(ok_label) catch unreachable;

            const ok_type = expanded_expr_type.get_ok_type().expand_identifier();
            const ok_lval = expr.create_select_lval(0, 0, ok_type, null, self.ctx.allocator());
            self.instructions.append(Instruction.init_jump(end_label, ast.token().span, self.ctx.allocator())) catch unreachable;
            self.instructions.append(end_label) catch unreachable;

            return ok_lval;
        },

        // Binary operators
        .assign => {
            const rhs = try self.lower_AST(ast.rhs(), labels) orelse return null;
            // If tuple, recursively generate a series of assignments
            // Else, create a *single* copy Instruction with an L_Value tree
            try self.generate_assign(ast.lhs(), rhs, labels);
            return self.lval_from_unit_value(ast);
        },

        .@"or", .@"and" => return try self.or_and_op(ast, labels),

        .@"comptime" => return try self.lower_AST(ast.expr(), labels),

        .add,
        .sub,
        .mult,
        .div,
        .mod,
        .left_shift,
        .right_shift,
        .greater,
        .lesser,
        .greater_equal,
        .lesser_equal,
        => return try self.binop(ast, labels),

        .equal, .not_equal => return try self.tuple_equality_check(ast, labels),

        .@"catch", .@"orelse" => return try self.coalesce_op(ast, labels),

        .bit_and, .bit_or, .bit_xor => {
            var temp: *lval_.L_Value = undefined;
            var src1 = (try self.lower_AST(ast.children().items[0], labels)) orelse return null;

            for (1..ast.children().items.len) |i| {
                temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
                const src2 = (try self.lower_AST(ast.children().items[i], labels)) orelse continue;
                self.instructions.append(Instruction.init(bit_ir_kind_from_ast_kind(ast), temp, src1, src2, ast.token().span, self.ctx.allocator())) catch unreachable;
                src1 = temp;
            }
            return temp;
        },
        .call => {
            const lhs = (try self.lower_AST(ast.lhs(), labels)) orelse return null;
            var temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
            temp.extract_symbver().symbol.set_span(ast.token().span);

            var instr = Instruction.init_call(temp, lhs, ast.token().span, self.ctx.allocator());
            for (ast.children().items) |term| {
                instr.data.call.arg_lval_list.append((try self.lower_AST(term, labels)) orelse continue) catch unreachable;
            }
            for (ast.call.context_args.items) |context_arg| {
                instr.data.call.arg_lval_list.append(lval_.L_Value.create_unversioned_symbver(context_arg, self.ctx.allocator())) catch unreachable;
            }
            self.instructions.append(Instruction.init_stack_push(ast.token().span, self.ctx.allocator())) catch unreachable;
            self.instructions.append(instr) catch unreachable;
            self.instructions.append(Instruction.init_stack_pop(ast.token().span, self.ctx.allocator())) catch unreachable;
            return temp;
        },
        .invoke => {
            var temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
            temp.extract_symbver().symbol.set_span(ast.token().span);

            // dyn_value is the method receiver that will be passed in
            var dyn_value: ?*lval_.L_Value = null;
            var lval_list = std.array_list.Managed(*lval_.L_Value).init(self.ctx.allocator());
            if (ast.children().items.len == 0) {
                // dyn_value should be the receiver
                if (self.ctx.typecheck.typeof(ast.lhs()).expand_identifier().* == .dyn_type) {
                    dyn_value = (try self.lower_AST(ast.lhs(), labels)) orelse return null;
                }
            } else {
                for (ast.children().items, 0..) |term, i| {
                    // Lower each child
                    const term_lval = (try self.lower_AST(term, labels)) orelse continue;
                    lval_list.append(term_lval) catch unreachable;

                    // Try to find the first child, it's possibly the receiver
                    if (self.ctx.typecheck.typeof(ast.lhs()).expand_identifier().* == .dyn_type and i == 0) {
                        if (term == ast.lhs()) {
                            // UFCS-like, receiver was prepended to the children list
                            dyn_value = term_lval;
                        } else {
                            // Other wise, lower the lhs
                            dyn_value = (try self.lower_AST(ast.lhs(), labels)) orelse continue;
                        }
                    }
                }
            }
            const instr = Instruction.init_invoke(temp, ast.invoke.method_decl.?, lval_list, dyn_value, ast.token().span, self.ctx.allocator());
            for (ast.invoke.context_args.items) |context_arg| {
                instr.data.invoke.arg_lval_list.append(lval_.L_Value.create_unversioned_symbver(context_arg, self.ctx.allocator())) catch unreachable;
            }
            if (ast.invoke.method_decl.?.method_decl.init != null) {
                // Fine if symbol is null, for invokes on trait objects.
                instr.data.invoke.method_decl_lval = try self.lval_from_symbol_cfg(ast.invoke.method_decl.?.symbol().?, ast.token().span);
            }
            self.instructions.append(Instruction.init_stack_push(ast.token().span, self.ctx.allocator())) catch unreachable;
            self.instructions.append(instr) catch unreachable;
            self.instructions.append(Instruction.init_stack_pop(ast.token().span, self.ctx.allocator())) catch unreachable;
            return temp;
        },
        .dyn_value => {
            const expr = try self.lower_AST(ast.expr(), labels) orelse return null;
            const temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
            self.instructions.append(Instruction.init_dyn(temp, expr, ast.dyn_value.mut, ast.dyn_value.impl.?, ast.token().span, self.ctx.allocator())) catch unreachable;
            return temp;
        },
        .index => {
            const lhs = (try self.lower_AST(ast.lhs(), labels)) orelse return null;
            const rhs = (try self.lower_AST(ast.children().items[0], labels)) orelse return null;
            var new_lhs = lhs;

            // Get the type of the index ast. This will determine if this is an array index or a slice index
            const ast_expanded_type = self.ctx.typecheck.typeof(ast.lhs()).expand_identifier();
            const _type = self.ctx.typecheck.typeof(ast);
            if (ast_expanded_type.* == .addr_of) {
                // Indexing a multi-ptr, don't change lhs
                std.debug.assert(ast_expanded_type.addr_of.multiptr);
            } else if (ast_expanded_type.* == .struct_type and ast_expanded_type.struct_type.was_slice) {
                // Indexing a slice; index_val := lhs._0^[rhs]
                new_lhs = new_lhs.create_select_lval(0, 0, _type.expand_identifier(), null, self.ctx.allocator());
                new_lhs = new_lhs.create_dereference_lval(_type.expand_identifier(), self.ctx.allocator());
            }

            // Surround with L_Value node
            const length: ?*lval_.L_Value = self.generate_indexable_length(lhs, ast_expanded_type, ast.token().span);
            return new_lhs.create_index_lval(rhs, length, _type.expand_identifier(), self.ctx.allocator());
        },
        .select => {
            // Recursively get select's ast L_Value node
            const ast_lval = try self.lower_AST(ast.lhs(), labels); // cannot be unreachable, since unreachable isn't selectable

            // Get the offset into the struct that this select does
            var lhs_expanded_type = self.ctx.typecheck.typeof(ast.lhs()).expand_identifier();
            const offset = if (lhs_expanded_type.* == .struct_type)
                lhs_expanded_type.struct_type.get_offset(ast.pos().?)
            else if (lhs_expanded_type.* == .tuple_type)
                lhs_expanded_type.tuple_type.get_offset(ast.pos().?)
            else
                0;

            var tag: ?*lval_.L_Value = null;
            if (lhs_expanded_type.* == .enum_type) {
                // Check that the sum value has the proper tag before a selection
                tag = self.create_temp_lvalue(prelude_.word64_type);
                self.instructions.append(Instruction.init_get_tag(tag.?, ast_lval.?, ast.token().span, self.ctx.allocator())) catch unreachable;
            }

            // Surround with L_Value node
            const field = ast.pos().?;
            const expanded_type = self.ctx.typecheck.typeof(ast).expand_identifier();
            return ast_lval.?.create_select_lval(field, offset, expanded_type, tag, self.ctx.allocator());
        },
        .struct_value, .tuple_value, .array_value, .context_value => {
            _ = self.ctx.typecheck.typeof(ast).expand_identifier();
            const temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
            var instr = Instruction.init_load_struct(temp, ast.token().span, self.ctx.allocator());
            for (ast.children().items) |term| {
                instr.data.lval_list.append((try self.lower_AST(term, labels)) orelse return null) catch unreachable;
            }
            self.instructions.append(instr) catch unreachable;
            return temp;
        },
        // Fancy Operators
        .sub_slice => {
            const arr = (try self.lower_AST(ast.sub_slice.super, labels)) orelse return null;
            const lower = (try self.lower_AST(ast.sub_slice.lower.?, labels)) orelse return null;
            const upper = (try self.lower_AST(ast.sub_slice.upper.?, labels)) orelse return null;

            self.generate_subslice_check(lower, upper, ast.token().span);

            const new_size = self.create_temp_lvalue(prelude_.int_type);
            self.instructions.append(Instruction.init(.sub_int, new_size, upper, lower, ast.token().span, self.ctx.allocator())) catch unreachable;

            const data_type = self.ctx.typecheck.typeof(ast).children().items[0];
            const data_ptr = arr.create_select_lval(0, 0, data_type.expand_identifier(), null, self.ctx.allocator());

            const new_data_ptr = self.create_temp_lvalue(data_type);
            self.instructions.append(Instruction.init(.add_int, new_data_ptr, data_ptr, lower, ast.token().span, self.ctx.allocator())) catch unreachable;

            const temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
            var load_struct = Instruction.init_load_struct(temp, ast.token().span, self.ctx.allocator());
            load_struct.data.lval_list.append(new_data_ptr) catch unreachable;
            load_struct.data.lval_list.append(new_size) catch unreachable;
            self.instructions.append(load_struct) catch unreachable;
            return temp;
        },
        .enum_value => {
            var _init: ?*lval_.L_Value = null;
            const pos: usize = ast.pos().?;
            const ast_type = self.ctx.typecheck.typeof(ast).expand_identifier();
            const proper_term: *Type_AST = if (ast_type.* == .enum_type or ast_type.* == .untagged_sum_type)
                ast_type.children().items[pos]
            else if (ast_type.* == .annotation)
                ast_type
            else
                std.debug.panic("expected either sum type or annot got {f}", .{ast_type});
            if (ast.enum_value.init != null) {
                const sum_init = try self.lower_AST(ast.enum_value.init.?, labels);
                if (proper_term.child().* != .unit_type) { // still output the Instruction, but do not refer to it unless the type isn't unit
                    _init = sum_init;
                }
            }
            const temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
            self.instructions.append(Instruction.init_union(temp, _init, ast.pos().?, ast.token().span, self.ctx.allocator())) catch unreachable;
            return temp;
        },
        // Control-flow expressions
        .@"if" => {
            // Create the result symbol and labels used
            const symbol = self.create_temp_symbol(self.ctx.typecheck.typeof(ast));
            const else_label = Instruction.init_label("if.else", ast.token().span, self.ctx.allocator());
            const end_label = Instruction.init_label("if.end", ast.token().span, self.ctx.allocator());

            // If there's a let, then do it, dumby!
            if (ast.@"if".let) |let| {
                _ = try self.lower_AST(let, labels);
            }

            // Test condition
            try self.flow(ast.@"if".condition, else_label, false, ast.token().span, labels);

            // lhs was true
            try self.generate_control_flow_block(ast.body_block(), symbol, ast.else_block() != null, labels);
            // TODO: De-duplicate 1
            self.instructions.append(Instruction.init_jump(end_label, ast.token().span, self.ctx.allocator())) catch unreachable;

            // lhs was false
            self.instructions.append(else_label) catch unreachable;
            try self.generate_control_flow_else(ast.else_block(), symbol, ast.token().span, labels);

            self.instructions.append(end_label) catch unreachable;
            return lval_.L_Value.create_unversioned_symbver(symbol, self.ctx.allocator());
        },
        .match => {
            // Create the result symbol and labels used
            const symbol = self.create_temp_symbol(self.ctx.typecheck.typeof(ast));
            const end_label = Instruction.init_label("match.end", ast.token().span, self.ctx.allocator()); // Exit label of match
            const fail_label = Instruction.init_label("match.fail", ast.token().span, self.ctx.allocator()); // jumped to if all tests fail and no `else` mapping

            if (ast.match.let) |let| { // If there's a let, then do it, dumby!
                _ = try self.lower_AST(let, labels);
            }

            const expr = (try self.lower_AST(ast.expr(), labels)) orelse return null;
            const rhs_label_list = try self.generate_match_pattern_checks(expr, ast.children().*, fail_label, labels);

            // Couldn't match pattern, panic!
            self.instructions.append(fail_label) catch unreachable;
            self.instructions.append(Instruction.init_stack_push(ast.token().span, self.ctx.allocator())) catch unreachable;
            self.instructions.append(Instruction.init_panic("could not match pattern", ast.token().span, self.ctx.allocator())) catch unreachable;

            const span = ast.token().span;
            try self.generate_match_bodies(expr, ast.children().*, rhs_label_list, symbol, end_label, span, labels);

            self.instructions.append(end_label) catch unreachable;
            return lval_.L_Value.create_unversioned_symbver(symbol, self.ctx.allocator());
        },
        .@"while" => {
            // Create the result symbol and labels used
            const symbol = self.create_temp_symbol(self.ctx.typecheck.typeof(ast));
            const cond_label = Instruction.init_label("while.cond", ast.token().span, self.ctx.allocator());
            const current_continue_label = Instruction.init_label("while.continue", ast.token().span, self.ctx.allocator());
            const else_label = Instruction.init_label("while.else", ast.token().span, self.ctx.allocator());
            const end_label = Instruction.init_label("while.end", ast.token().span, self.ctx.allocator());

            if (ast.@"while".let) |let| { // do `let` if there's a let
                _ = try self.lower_AST(let, labels);
            }

            const loop_labels = Labels{
                .return_label = labels.return_label,
                .break_label = end_label,
                .continue_label = current_continue_label,
                .error_label = labels.error_label,
            };
            const post_labels = Labels{
                .return_label = labels.return_label,
                .break_label = end_label,
                .continue_label = labels.continue_label,
                .error_label = labels.error_label,
            };

            // Test condition, branch to either body or else block
            self.instructions.append(cond_label) catch unreachable;
            try self.flow(ast.@"while".condition, else_label, false, ast.token().span, loop_labels);

            // Body block
            try self.generate_control_flow_block(ast.body_block(), symbol, ast.else_block() != null, loop_labels);

            self.instructions.append(current_continue_label) catch unreachable;
            if (ast.@"while".post) |post| { // do `post` if there's a post, jump to end_label to break
                _ = try self.lower_AST(post, post_labels);
            }
            self.instructions.append(Instruction.init_jump(cond_label, ast.token().span, self.ctx.allocator())) catch unreachable;

            self.instructions.append(else_label) catch unreachable;
            try self.generate_control_flow_else(ast.else_block(), symbol, ast.token().span, labels);

            self.instructions.append(end_label) catch unreachable;
            return lval_.L_Value.create_unversioned_symbver(symbol, self.ctx.allocator());
        },
        .with => {
            for (ast.with.contexts.items) |ctx| {
                _ = try self.lower_AST(ctx, labels);
            }
            return self.lower_AST(ast.with._body_block, labels);
        },
        .block => { // TODO: TOO LONG
            if (ast.children().items.len == 0 and ast.block.final == null) {
                return self.lval_from_unit_value(ast);
            }

            // List of labels for each kind of jump
            var continue_labels = std.array_list.Managed(*Instruction).init(self.ctx.allocator());
            defer continue_labels.deinit();
            var break_labels = std.array_list.Managed(*Instruction).init(self.ctx.allocator());
            defer break_labels.deinit();
            var return_labels = std.array_list.Managed(*Instruction).init(self.ctx.allocator());
            defer return_labels.deinit();
            var error_labels = std.array_list.Managed(*Instruction).init(self.ctx.allocator());
            defer error_labels.deinit();
            // populate the lists a label for each jump kind for each defer
            for (ast.block.defers.items) |_| {
                continue_labels.append(Instruction.init_label("block.defer.continue", ast.token().span, self.ctx.allocator())) catch unreachable;
                break_labels.append(Instruction.init_label("block.defer.break", ast.token().span, self.ctx.allocator())) catch unreachable;
                return_labels.append(Instruction.init_label("block.defer.return", ast.token().span, self.ctx.allocator())) catch unreachable;
                error_labels.append(Instruction.init_label("block.errdefer.error.ok", ast.token().span, self.ctx.allocator())) catch unreachable;
            }
            const end_label = Instruction.init_label("block.end", ast.token().span, self.ctx.allocator());

            // These are the labels to go to on each final statement.
            // These are updated to point to different places in the defer chain at the end of this block.
            var current_labels: Labels = labels;

            var defer_label_index: usize = 0;
            var errdefer_label_index: usize = 0;

            var temp: ?*lval_.L_Value = null;
            for (ast.children().items) |child| {
                temp = try self.lower_AST(child, current_labels);
                if (child.* == .@"defer") {
                    current_labels.continue_label = continue_labels.items[defer_label_index];
                    current_labels.break_label = break_labels.items[defer_label_index];
                    current_labels.return_label = return_labels.items[defer_label_index];
                    current_labels.error_label = error_labels.items[defer_label_index];
                    defer_label_index += 1;
                } else if (child.* == .@"errdefer") {
                    current_labels.error_label = error_labels.items[errdefer_label_index];
                    errdefer_label_index += 1;
                }
            }
            if (ast.block.final) |final| {
                _ = try self.lower_AST(final, current_labels);
            } else if (temp) |_temp| {
                self.wrap_error_return(_temp, ast.token().span, current_labels);
            }

            try self.generate_defers(&ast.block.defers, &continue_labels);
            self.instructions.append(Instruction.init_jump(end_label, ast.token().span, self.ctx.allocator())) catch unreachable;

            try self.generate_defers(&ast.block.defers, &break_labels);
            self.instructions.append(Instruction.init_jump(labels.break_label, ast.token().span, self.ctx.allocator())) catch unreachable;

            try self.generate_defers(&ast.block.defers, &return_labels);
            self.instructions.append(Instruction.init_jump(labels.return_label, ast.token().span, self.ctx.allocator())) catch unreachable;

            try self.generate_defers(&ast.block.defers, &error_labels);
            self.instructions.append(Instruction.init_jump(labels.error_label, ast.token().span, self.ctx.allocator())) catch unreachable;

            self.instructions.append(end_label) catch unreachable;

            return temp;
        },

        // Control-flow statements
        .binding => {
            if (ast.binding.init) |_init| {
                const def: ?*lval_.L_Value = try self.lower_AST(_init, labels);
                if (def == null) {
                    return null;
                }
                try self.generate_pattern(ast.binding.pattern, ast.binding.type.expand_identifier(), def.?);
            }
            return self.lval_from_unit_value(ast);
        },
        .context_value_decl => {
            if (ast.context_value_decl.init) |_init| {
                const def: ?*lval_.L_Value = try self.lower_AST(_init, labels);
                if (def == null) {
                    return null;
                }

                const symbver = lval_.L_Value.create_unversioned_symbver(ast.symbol().?, self.ctx.allocator());
                self.instructions.append(Instruction.init_simple_copy(symbver, def.?, ast.token().span, self.ctx.allocator())) catch unreachable;
            }
            return self.lval_from_unit_value(ast);
        },
        .decl => return self.lval_from_unit_value(ast),
        .fn_decl => return try self.lval_from_symbol_cfg(ast.symbol().?, ast.token().span),
        .@"errdefer", .@"defer" => return self.lval_from_unit_value(ast),
        .@"continue" => {
            self.instructions.append(Instruction.init_jump(labels.continue_label, ast.token().span, self.ctx.allocator())) catch unreachable;
            return null;
        },
        .@"unreachable" => {
            self.instructions.append(Instruction.init_stack_push(ast.token().span, self.ctx.allocator())) catch unreachable;
            self.instructions.append(Instruction.init_panic("reached unreachable code", ast.token().span, self.ctx.allocator())) catch unreachable;
            return null;
        },
        .@"break" => {
            self.instructions.append(Instruction.init_jump(labels.break_label, ast.token().span, self.ctx.allocator())) catch unreachable;
            return null;
        },
        .@"return" => {
            if (ast.@"return"._ret_expr) |expr| {
                // Copy expr to retval
                const retval = (try self.lower_AST(expr, labels)) orelse return null;
                const retval_lval = lval_.L_Value.create_unversioned_symbver(self.cfg.return_symbol, self.ctx.allocator());
                self.instructions.append(Instruction.init_simple_copy(retval_lval, retval, ast.token().span, self.ctx.allocator())) catch unreachable;
                self.wrap_error_return(retval, ast.token().span, labels);
            }
            self.instructions.append(Instruction.init_jump(labels.return_label, ast.token().span, self.ctx.allocator())) catch unreachable;
            return null;
        },

        else => std.debug.panic("compiler error: unimplemented lower_AST() for: AST.{s}", .{@tagName(ast.*)}),
    }
}

fn lval_from_unit_value(
    self: *Self,
    ast: *ast_.AST, // TODO: Just accept span
) *lval_.L_Value {
    const lval = self.create_temp_lvalue(prelude_.unit_type);
    self.instructions.append(Instruction.init(.load_unit, lval, null, null, ast.token().span, self.ctx.allocator())) catch unreachable;
    return lval;
}

fn lval_from_int(
    self: *Self,
    int: i128,
    _type: *Type_AST,
    span: Span,
) *lval_.L_Value {
    const temp = self.create_temp_lvalue(_type);
    self.instructions.append(Instruction.init_int(temp, int, span, self.ctx.allocator())) catch unreachable;
    return temp;
}

fn lval_from_symbol_cfg(
    self: *Self,
    symbol: *Symbol,
    span: Span,
) Lower_Errors!*lval_.L_Value {
    const callee = try self.ctx.cfg_store.get_cfg(symbol, self.interned_strings);
    self.cfg.children.put(callee, {}) catch unreachable;
    const lval = self.create_temp_lvalue(symbol.type());
    const instr = Instruction.init_symbol(lval, symbol, span, self.ctx.allocator());
    self.instructions.append(instr) catch unreachable;
    return lval;
}

fn unop(
    self: *Self,
    ast: *ast_.AST, // TODO: Create some sort of context for these parameters?
    labels: Labels,
) Lower_Errors!?*lval_.L_Value {
    const expr = try self.lower_AST(ast.expr(), labels) orelse return null;
    const temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
    self.instructions.append(Instruction.init_int_float_kind(int_kind(ast), float_kind(ast), temp, expr, null, ast.token().span, self.ctx.allocator())) catch unreachable;
    return temp;
}

fn binop(
    self: *Self,
    ast: *ast_.AST, // TODO: Create some sort of context for these parameters?
    labels: Labels,
) Lower_Errors!?*lval_.L_Value {
    const lhs_lval = try self.lower_AST(ast.lhs(), labels);
    const rhs_lval = try self.lower_AST(ast.rhs(), labels);
    if (lhs_lval == null or rhs_lval == null) {
        return null;
    }
    const temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
    self.instructions.append(Instruction.init_int_float_kind(int_kind(ast), float_kind(ast), temp, lhs_lval, rhs_lval, ast.token().span, self.ctx.allocator())) catch unreachable;
    return temp;
}

fn int_kind(ast: *ast_.AST) Instruction.Kind {
    return switch (ast.*) {
        .not => .not,
        .negate => .negate_int,
        .bit_not => .bit_not,
        .addr_of => if (ast.addr_of.mut) .mut_addr_of else .addr_of,
        .add => .add_int,
        .sub => .sub_int,
        .mult => .mult_int,
        .div => .div_int,
        .mod => .mod,
        .left_shift => .left_shift,
        .right_shift => .right_shift,
        .greater => .greater_int,
        .lesser => .lesser_int,
        .greater_equal => .greater_equal_int,
        .lesser_equal => .lesser_equal_int,
        else => unreachable,
    };
}

fn float_kind(ast: *ast_.AST) Instruction.Kind {
    return switch (ast.*) {
        .not => .not,
        .negate => .negate_float,
        .bit_not => .bit_not,
        .addr_of => if (ast.addr_of.mut) .mut_addr_of else .addr_of,
        .add => .add_float,
        .sub => .sub_float,
        .mult => .mult_float,
        .div => .div_float,
        .mod => .mod,
        .greater => .greater_float,
        .lesser => .lesser_float,
        .greater_equal => .greater_equal_float,
        .lesser_equal => .lesser_equal_float,
        .left_shift => .left_shift,
        .right_shift => .right_shift,
        else => unreachable,
    };
}

fn or_and_op(
    self: *Self,
    ast: *ast_.AST,
    labels: Labels,
) Lower_Errors!?*lval_.L_Value {
    // Create the result symbol and labels used
    const or_and_symbol = self.create_temp_symbol(self.ctx.typecheck.typeof(ast));
    const jump_label = Instruction.init_label("or/and.jump", ast.token().span, self.ctx.allocator());
    const end_label = Instruction.init_label("or/and.end", ast.token().span, self.ctx.allocator());

    const should_jump = ast.* == .@"or";

    // Test condition
    try self.flow(ast.lhs(), jump_label, should_jump, ast.token().span, labels);
    try self.flow(ast.rhs(), jump_label, should_jump, ast.token().span, labels);

    // never jumped, store whether that's good or not in temp
    const load_false_lval = lval_.L_Value.create_unversioned_symbver(or_and_symbol, self.ctx.allocator());
    self.instructions.append(Instruction.init_int(load_false_lval, if (should_jump) 0 else 1, ast.token().span, self.ctx.allocator())) catch unreachable;
    self.instructions.append(Instruction.init_jump(end_label, ast.token().span, self.ctx.allocator())) catch unreachable;

    // jumped, store whether that's good or not in temp
    self.instructions.append(jump_label) catch unreachable;
    const load_true_lval = lval_.L_Value.create_unversioned_symbver(or_and_symbol, self.ctx.allocator());
    self.instructions.append(Instruction.init_int(load_true_lval, if (should_jump) 1 else 0, ast.token().span, self.ctx.allocator())) catch unreachable;

    self.instructions.append(end_label) catch unreachable;
    return lval_.L_Value.create_unversioned_symbver(or_and_symbol, self.ctx.allocator());
}

fn tuple_equality_check(
    self: *Self,
    ast: *ast_.AST,
    labels: Labels,
) Lower_Errors!?*lval_.L_Value {
    std.debug.assert(ast.* == .equal or ast.* == .not_equal);
    const lhs = try self.lower_AST(ast.lhs(), labels);
    const rhs = try self.lower_AST(ast.rhs(), labels);
    if (lhs == null or rhs == null) {
        return null;
    }

    // simple case, simple equality
    const temp = self.create_temp_lvalue(self.ctx.typecheck.typeof(ast));
    const lhs_type = lhs.?.get_expanded_type();
    if (lhs_type.* != .tuple_type and lhs_type.* != .enum_type) {
        self.instructions.append(Instruction.init(if (ast.* == .equal) .equal else .not_equal, temp, lhs, rhs, lhs.?.extract_symbver().symbol.span(), self.ctx.allocator())) catch unreachable;
        return temp;
    }

    // Labels used
    const fail_label = Instruction.init_label("tuple-eq.fail", ast.token().span, self.ctx.allocator());
    const end_label = Instruction.init_label("tuple-eq.end", ast.token().span, self.ctx.allocator());

    self.tuple_equality_flow(lhs.?, rhs.?, fail_label);

    self.instructions.append(Instruction.init_int(temp, if (ast.* == .equal) 1 else 0, ast.token().span, self.ctx.allocator())) catch unreachable;
    self.instructions.append(Instruction.init_jump(end_label, ast.token().span, self.ctx.allocator())) catch unreachable;

    self.instructions.append(fail_label) catch unreachable;
    self.instructions.append(Instruction.init_int(temp, if (ast.* == .equal) 0 else 1, ast.token().span, self.ctx.allocator())) catch unreachable;

    self.instructions.append(end_label) catch unreachable;
    return temp;
}

fn tuple_equality_flow(
    self: *Self,
    lhs: *lval_.L_Value,
    rhs: *lval_.L_Value,
    fail_label: *Instruction,
) void {
    const new_lhs = lhs;
    const new_rhs = rhs;
    const temp = self.create_temp_lvalue(prelude_.bool_type);
    var lhs_type = lhs.get_expanded_type();
    if (lhs_type.* == .tuple_type) {
        for (0..lhs_type.children().items.len) |field| {
            const expanded_type = lhs_type.children().items[field].expand_identifier();
            const offset = lhs_type.tuple_type.get_offset(field);
            const lhs_select = new_lhs.create_select_lval(field, offset, expanded_type, null, self.ctx.allocator());
            const rhs_select = new_rhs.create_select_lval(field, offset, expanded_type, null, self.ctx.allocator());
            self.tuple_equality_flow(lhs_select, rhs_select, fail_label);
        }
    } else if (lhs_type.* == .enum_type) {
        const lhs_tag = self.create_temp_lvalue(prelude_.word64_type);
        const rhs_tag = self.create_temp_lvalue(prelude_.word64_type);
        self.instructions.append(Instruction.init_get_tag(lhs_tag, new_lhs, lhs.extract_symbver().symbol.span(), self.ctx.allocator())) catch unreachable;
        self.instructions.append(Instruction.init_get_tag(rhs_tag, new_rhs, lhs.extract_symbver().symbol.span(), self.ctx.allocator())) catch unreachable;
        self.instructions.append(Instruction.init(.equal, temp, lhs_tag, rhs_tag, lhs.extract_symbver().symbol.span(), self.ctx.allocator())) catch unreachable;
        self.instructions.append(Instruction.init_branch(temp, fail_label, lhs.extract_symbver().symbol.span(), self.ctx.allocator())) catch unreachable;
    } else {
        self.instructions.append(Instruction.init(.equal, temp, new_lhs, rhs, lhs.extract_symbver().symbol.span(), self.ctx.allocator())) catch unreachable;
        self.instructions.append(Instruction.init_branch(temp, fail_label, lhs.extract_symbver().symbol.span(), self.ctx.allocator())) catch unreachable;
    }
}

fn coalesce_op(
    self: *Self,
    ast: *ast_.AST, // The coalesce operator to lower
    labels: Labels, // The current labels to use
) Lower_Errors!?*lval_.L_Value {
    // Create the result symbol and labels
    const coalesce_symbol = self.create_temp_symbol(self.ctx.typecheck.typeof(ast));
    const zero_label = Instruction.init_label("coalesce.zero", ast.token().span, self.ctx.allocator());
    const end_label = Instruction.init_label("coalesce.end", ast.token().span, self.ctx.allocator());

    // Test if lhs tag is 0 (`ok` or `some`)
    const lhs = (try self.lower_AST(ast.lhs(), labels)) orelse return null;

    const condition = self.create_temp_lvalue(prelude_.word64_type);
    self.instructions.append(Instruction.init_get_tag(condition, lhs, ast.token().span, self.ctx.allocator())) catch unreachable;
    self.instructions.append(Instruction.init_branch(condition, zero_label, ast.token().span, self.ctx.allocator())) catch unreachable;

    // tag was an error/none, store rhs in temp
    const maybe_rhs = (try self.lower_AST(ast.rhs(), labels));
    if (maybe_rhs) |rhs| {
        const rhs_lval = lval_.L_Value.create_unversioned_symbver(coalesce_symbol, self.ctx.allocator());
        self.instructions.append(Instruction.init_simple_copy(rhs_lval, rhs, ast.token().span, self.ctx.allocator())) catch unreachable;
        self.instructions.append(Instruction.init_jump(end_label, ast.token().span, self.ctx.allocator())) catch unreachable;
    }

    // tag was `.ok` or `.some`, store lhs in temp
    self.instructions.append(zero_label) catch unreachable;
    const expanded_type = self.ctx.typecheck.typeof(ast).expand_identifier();
    const val = lhs.create_select_lval(0, 0, expanded_type, null, self.ctx.allocator());
    const ok_lval = lval_.L_Value.create_unversioned_symbver(coalesce_symbol, self.ctx.allocator());
    self.instructions.append(Instruction.init_simple_copy(ok_lval, val, ast.token().span, self.ctx.allocator())) catch unreachable;

    self.instructions.append(end_label) catch unreachable;
    return lval_.L_Value.create_unversioned_symbver(coalesce_symbol, self.ctx.allocator());
}

fn bit_ir_kind_from_ast_kind(ast: *ast_.AST) Instruction.Kind {
    return switch (ast.*) {
        .bit_and => Instruction.Kind.bit_and,
        .bit_or => Instruction.Kind.bit_or,
        .bit_xor => Instruction.Kind.bit_xor,
        else => std.debug.panic("compiler error: not a bit ast: {s}\n", .{@tagName(ast.*)}),
    };
}

fn generate_assign(
    self: *Self,
    lhs: *ast_.AST, // AST node for the LHS of the `=`
    rhs: *lval_.L_Value, // L_Value which holds the value to assign
    labels: Labels,
) Lower_Errors!void {
    if (lhs.* == .tuple_value) {
        // Recursively call `generate_assign` for each term in the tuple.
        // tuple-assigns may be nested, for example:
        //     ((x, y), (a, b)) = get_tuple()
        // So it's important that this is recursive
        var lhs_expanded_type = self.ctx.typecheck.typeof(lhs).expand_identifier();
        for (lhs.children().items, 0..) |term, field| {
            const product_lhs = try self.lower_AST(term, labels);
            if (product_lhs == null) {
                continue;
            }
            const expanded_type = rhs.get_expanded_type().children().items[field].expand_identifier();
            const offset = lhs_expanded_type.tuple_type.get_offset(field);
            const select = rhs.create_select_lval(field, offset, expanded_type, null, self.ctx.allocator());
            try self.generate_assign(term, select, labels);
        }
    } else if (lhs.* == .array_value) {
        // Recursively call `generate_assign` for each term in the array.
        // array-assigns may be nested, for example:
        //     [[x, y], [a, b]] = get_array()
        // So it's important that this is recursive
        const lhs_expanded_type = self.ctx.typecheck.typeof(lhs).expand_identifier();
        for (lhs.children().items, 0..) |term, field| {
            const array_lhs = try self.lower_AST(term, labels);
            if (array_lhs == null) {
                continue;
            }
            const expanded_type = rhs.get_expanded_type().child().expand_identifier();
            const length: ?*lval_.L_Value = self.generate_indexable_length(rhs, lhs_expanded_type, lhs.token().span);
            const idx = self.lval_from_int(field, prelude_.int_type, lhs.token().span);
            const index = rhs.create_index_lval(idx, length, expanded_type, self.ctx.allocator());
            try self.generate_assign(term, index, labels);
        }
    } else if (lhs.* == .select and self.ctx.typecheck.typeof(lhs.lhs()).expand_identifier().* == .enum_type) {
        // Select on a sum-type, convert to a copy of an enum value
        const enum_value = self.create_temp_lvalue(self.ctx.typecheck.typeof(lhs.lhs()).expand_identifier());
        self.instructions.append(Instruction.init_union(enum_value, rhs, lhs.pos().?, lhs.token().span, self.ctx.allocator())) catch unreachable;
        try self.generate_assign(lhs.lhs(), enum_value, labels);
    } else {
        // Get L_Value tree, create a `copy` Instruction of L_Value tree and rhs
        const lval = try self.lower_AST(lhs, labels);
        const instr = Instruction.init(.copy, lval, rhs, null, lhs.token().span, self.ctx.allocator());
        self.instructions.append(instr) catch unreachable;
    }
}

/// Generates the Instruction to get the length of either an array or a slice. Used by indices to get the length for bounds checks
fn generate_indexable_length(
    self: *Self,
    lhs: *lval_.L_Value,
    _type: *Type_AST,
    span: Span,
) ?*lval_.L_Value {
    if (_type.* == .addr_of) {
        // Implies multiptr, unfortunately there is no way to get the length
        std.debug.assert(_type.addr_of.multiptr);
        return null;
    } else if (_type.* == .struct_type and _type.struct_type.was_slice) {
        const offset = _type.struct_type.get_offset(1);
        return lhs.create_select_lval(1, offset, prelude_.int64_type, null, self.ctx.allocator());
    } else if (_type.* == .array_of) {
        const retval = self.create_temp_lvalue(prelude_.int_type);
        self.instructions.append(Instruction.init_int(retval, _type.array_of.len.int.data, span, self.ctx.allocator())) catch unreachable;
        return retval;
    } else if (_type.* == .tuple_type) {
        const retval = self.create_temp_lvalue(prelude_.int_type);
        self.instructions.append(Instruction.init_int(retval, _type.children().items.len, span, self.ctx.allocator())) catch unreachable;
        return retval;
    } else {
        std.debug.panic("compiler error: cannot generate Instruction to get length of type AST.{s}", .{@tagName(_type.*)});
    }
}

/// Generates code to verify at runtime that a subslice's lower bound is less-than-or-equal-to the subslice's upper bound.
fn generate_subslice_check(
    self: *Self,
    lower: *lval_.L_Value,
    upper: *lval_.L_Value,
    span: Span,
) void {
    const end_label = Instruction.init_label("subslice-bounds-check.end", span, self.ctx.allocator());
    const compare = self.create_temp_lvalue(prelude_.bool_type);
    self.instructions.append(Instruction.init(.greater_int, compare, lower, upper, span, self.ctx.allocator())) catch unreachable;
    self.instructions.append(Instruction.init_branch(compare, end_label, span, self.ctx.allocator())) catch unreachable;
    self.instructions.append(Instruction.init_stack_push(span, self.ctx.allocator())) catch unreachable;
    self.instructions.append(Instruction.init_panic("subslice lower bound is greater than upper bound", span, self.ctx.allocator())) catch unreachable;
    self.instructions.append(end_label) catch unreachable;
}

/// Generates Instruction code to jump to `label` iff `condition` evaluates to `sense`.
fn flow(
    self: *Self,
    condition: *ast_.AST,
    label: *Instruction,
    sense: bool,
    span: Span,
    labels: Labels,
) Lower_Errors!void {
    // FIXME: High Cyclo
    switch (condition.*) {
        .true => if (sense) {
            self.instructions.append(Instruction.init_jump(label, span, self.ctx.allocator())) catch unreachable;
        },
        .false => if (!sense) {
            self.instructions.append(Instruction.init_jump(label, span, self.ctx.allocator())) catch unreachable;
        },
        .not => try self.flow(condition.expr(), label, !sense, span, labels),
        .@"or" => if (sense) {
            try self.flow(condition.lhs(), label, true, span, labels);
            try self.flow(condition.rhs(), label, true, span, labels);
        } else {
            const skip_label = Instruction.init_label("or-flow.skip", span, self.ctx.allocator());
            try self.flow(condition.lhs(), skip_label, true, span, labels);
            try self.flow(condition.rhs(), label, false, span, labels);
            self.instructions.append(skip_label) catch unreachable;
        },
        .@"and" => if (sense) {
            const skip_label = Instruction.init_label("and-flow.skip", span, self.ctx.allocator());
            try self.flow(condition.lhs(), skip_label, false, span, labels);
            try self.flow(condition.rhs(), label, true, span, labels);
            self.instructions.append(skip_label) catch unreachable;
        } else {
            try self.flow(condition.lhs(), label, false, span, labels);
            try self.flow(condition.rhs(), label, false, span, labels);
        },
        else => if (sense) {
            const condition_lval = try self.lower_AST(condition, labels) orelse return;
            const not_condition_lval = self.create_temp_lvalue(prelude_.bool_type);
            self.instructions.append(Instruction.init(.not, not_condition_lval, condition_lval, null, span, self.ctx.allocator())) catch unreachable; // Will be optimized out!
            self.instructions.append(Instruction.init_branch(not_condition_lval, label, span, self.ctx.allocator())) catch unreachable;
        } else {
            const condition_lval = try self.lower_AST(condition, labels) orelse return;
            self.instructions.append(Instruction.init_branch(condition_lval, label, span, self.ctx.allocator())) catch unreachable;
        },
    }
}

/// Wraps stores to an lval in either a union or as a simple-copy
fn generate_control_flow_block(
    self: *Self,
    ast: *ast_.AST,
    symbol: *Symbol,
    has_else: bool,
    labels: Labels,
) Lower_Errors!void {
    if (try self.lower_AST(ast, labels)) |rhs_lval| {
        const rhs_copy_lval = lval_.L_Value.create_unversioned_symbver(symbol, self.ctx.allocator());
        if (has_else) {
            self.instructions.append(Instruction.init_simple_copy(rhs_copy_lval, rhs_lval, ast.token().span, self.ctx.allocator())) catch unreachable;
        } else {
            self.instructions.append(Instruction.init_union(rhs_copy_lval, rhs_lval, 0, ast.token().span, self.ctx.allocator())) catch unreachable; // `some` is 0
        }
    }
}

fn generate_control_flow_else(
    self: *Self,
    ast: ?*ast_.AST,
    symbol: *Symbol,
    span: Span,
    labels: Labels,
) Lower_Errors!void {
    if (ast) |else_block| {
        if (try self.lower_AST(else_block, labels)) |else_lval| {
            const else_copy_lval = lval_.L_Value.create_unversioned_symbver(symbol, self.ctx.allocator());
            self.instructions.append(Instruction.init_simple_copy(else_copy_lval, else_lval, span, self.ctx.allocator())) catch unreachable;
        }
    } else {
        // no else block => store null
        const else_copy_lval = lval_.L_Value.create_unversioned_symbver(symbol, self.ctx.allocator());
        self.instructions.append(Instruction.init_union(else_copy_lval, null, 1, span, self.ctx.allocator())) catch unreachable; // `none` is 1
    }
}

fn generate_match_pattern_checks(
    self: *Self,
    expr: *lval_.L_Value,
    mappings: std.array_list.Managed(*ast_.AST),
    none_label: *Instruction,
    labels: Labels,
) Lower_Errors!std.array_list.Managed(*Instruction) {
    var lhs_label_list = std.array_list.Managed(*Instruction).init(self.ctx.allocator()); // labels to branch on an unsuccessful test ("next pattern")
    defer lhs_label_list.deinit();
    var rhs_label_list = std.array_list.Managed(*Instruction).init(self.ctx.allocator()); // labels to branch on a successful test ("code for the mapping")
    errdefer rhs_label_list.deinit();
    for (mappings.items) |mapping| {
        lhs_label_list.append(Instruction.init_label("match.lhs", mapping.token().span, self.ctx.allocator())) catch unreachable;
        rhs_label_list.append(Instruction.init_label("match-rhs", mapping.token().span, self.ctx.allocator())) catch unreachable;
    }
    for (mappings.items, 0..) |mapping, i| {
        self.instructions.append(lhs_label_list.items[i]) catch unreachable;
        const next_label = if (i < lhs_label_list.items.len - 1)
            lhs_label_list.items[i + 1]
        else
            none_label;
        try self.generate_match_pattern_check(mapping.lhs(), expr, next_label, labels);
        self.instructions.append(Instruction.init_jump(rhs_label_list.items[i], mapping.token().span, self.ctx.allocator())) catch unreachable;
    }
    return rhs_label_list;
}

/// Generates the code which checks if the pattern matches the match expression
fn generate_match_pattern_check(
    self: *Self,
    pattern: ?*ast_.AST,
    expr: *lval_.L_Value,
    next_pattern: *Instruction,
    labels: Labels,
) Lower_Errors!void {
    switch (pattern.?.*) {
        .pattern_symbol => {
            // Infallible check, do not branch to next pattern
        },
        .int,
        .float,
        .true,
        .false,
        .string,
        .block,
        => {
            const value = (try self.lower_AST(pattern.?, labels)) orelse return;
            const condition = self.create_temp_lvalue(prelude_.bool_type);
            self.instructions.append(Instruction.init(.equal, condition, expr, value, pattern.?.token().span, self.ctx.allocator())) catch unreachable;
            self.instructions.append(Instruction.init_branch(condition, next_pattern, pattern.?.token().span, self.ctx.allocator())) catch unreachable;
        },
        .tuple_value => {
            for (pattern.?.children().items, 0..) |term, field| {
                const expanded_type = expr.get_expanded_type().children().items[field].expand_identifier();
                const pattern_type = self.ctx.typecheck.typeof(pattern.?).expand_identifier();
                const offset = pattern_type.tuple_type.get_offset(field);
                const lval = expr.create_select_lval(field, offset, expanded_type, null, self.ctx.allocator());
                try self.generate_match_pattern_check(term, lval, next_pattern, labels);
            }
        },
        .select, .enum_value => {
            // Get tag of pattern
            const sel = self.create_temp_lvalue(prelude_.word64_type);
            self.instructions.append(Instruction.init_int(sel, pattern.?.pos().?, pattern.?.token().span, self.ctx.allocator())) catch unreachable;

            // Get tag of expr
            const tag = self.create_temp_lvalue(prelude_.word64_type);
            self.instructions.append(Instruction.init_get_tag(tag, expr, pattern.?.token().span, self.ctx.allocator())) catch unreachable;

            // Compare them, jump to next pattern if they are not equal
            const neql = self.create_temp_lvalue(prelude_.bool_type);
            self.instructions.append(Instruction.init(.equal, neql, tag, sel, pattern.?.token().span, self.ctx.allocator())) catch unreachable;
            self.instructions.append(Instruction.init_branch(neql, next_pattern, pattern.?.token().span, self.ctx.allocator())) catch unreachable;
        },
        else => std.debug.panic("compiler error: unimplemented generate_match_pattern_check() for {s}", .{@tagName(pattern.?.*)}),
    }
}

fn generate_match_bodies(
    self: *Self,
    expr: *lval_.L_Value,
    mappings: std.array_list.Managed(*ast_.AST),
    rhs_label_list: std.array_list.Managed(*Instruction),
    symbol: *Symbol,
    end_label: *Instruction,
    span: Span,
    labels: Labels,
) Lower_Errors!void {
    for (mappings.items, 0..) |mapping, i| {
        self.instructions.append(rhs_label_list.items[i]) catch unreachable;
        // Generate initialization for patterns before the rhs
        try self.generate_pattern(mapping.lhs(), expr.get_expanded_type(), expr);

        // Generate the rhs, copy result to symbol
        if (try self.lower_AST(mapping.rhs(), labels)) |rhs_lval| {
            const rhs_copy_lval = lval_.L_Value.create_unversioned_symbver(symbol, self.ctx.allocator());
            self.instructions.append(Instruction.init_simple_copy(rhs_copy_lval, rhs_lval, mapping.rhs().token().span, self.ctx.allocator())) catch unreachable;
        }
        self.instructions.append(Instruction.init_jump(end_label, span, self.ctx.allocator())) catch unreachable;
    }
}

fn wrap_error_return(
    self: *Self,
    expr: *lval_.L_Value,
    span: Span,
    labels: Labels,
) void {
    const expanded_temp_type = expr.get_expanded_type();
    if (labels.error_label != null and expanded_temp_type.* == .enum_type and expanded_temp_type.enum_type.from == .@"error") {
        // Returning error sum, runtime check if error, branch to error path
        const condition = self.create_temp_lvalue(prelude_.word64_type);
        self.instructions.append(Instruction.init_get_tag(condition, expr, span, self.ctx.allocator())) catch unreachable; // `ok` is 0 `err`s nonzero
        const not_condition = self.create_temp_lvalue(prelude_.bool_type);
        self.instructions.append(Instruction.init(.not, not_condition, condition, null, span, self.ctx.allocator())) catch unreachable;
        self.instructions.append(Instruction.init_branch(not_condition, labels.error_label, span, self.ctx.allocator())) catch unreachable;
    }
}

fn create_temp_lvalue(self: *Self, _type: *Type_AST) *lval_.L_Value {
    const temp_symbol = self.create_temp_symbol(_type);
    const retval = lval_.L_Value.create_unversioned_symbver(temp_symbol, self.ctx.allocator());
    return retval;
}

fn create_temp_symbol(self: *Self, _type: *Type_AST) *Symbol {
    var buf = std.array_list.Managed(u8).init(self.ctx.allocator());
    buf.print("t{}", .{self.number_temps}) catch unreachable;
    self.number_temps += 1;
    const name = buf.toOwnedSlice() catch unreachable;
    const token = Token.init_simple(name);
    const decl = ast_.AST.create_decl(
        token,
        ast_.AST.create_pattern_symbol(token, .mut, .local, name, self.ctx.allocator()),
        _type,
        null,
        self.ctx.allocator(),
    );
    var temp_symbol = Symbol.init(
        self.cfg.symbol.scope,
        name,
        decl,
        .mut,
        .local,
        self.ctx.allocator(),
    );
    _ = _type.expand_identifier();
    temp_symbol.is_temp = true;
    return temp_symbol;
}

/// Appends the label and body for each defer
fn generate_defers(
    self: *Self,
    defers: *std.array_list.Managed(*ast_.AST),
    defer_labels: *std.array_list.Managed(*Instruction),
) Lower_Errors!void {
    var i: usize = defers.items.len;
    while (i > 0) : (i -= 1) {
        self.instructions.append(defer_labels.items[i - 1]) catch unreachable;
        _ = try self.lower_AST(defers.items[i - 1], Labels.null_labels);
    }
}

/// Generates all symbol definitions in a pattern
fn generate_pattern(
    self: *Self,
    pattern: *ast_.AST,
    _type: *Type_AST,
    def: *lval_.L_Value,
) Lower_Errors!void {
    if (pattern.* == .pattern_symbol) {
        if (!std.mem.eql(u8, pattern.pattern_symbol.name, "_")) {
            const symbver = lval_.L_Value.create_unversioned_symbver(pattern.symbol().?, self.ctx.allocator());
            self.instructions.append(Instruction.init_simple_copy(symbver, def, pattern.token().span, self.ctx.allocator())) catch unreachable;
        }
    } else if (pattern.* == .tuple_value) {
        for (pattern.children().items, 0..) |term, field| {
            const expanded_type = _type.children().items[field].expand_identifier();
            const offset = _type.tuple_type.get_offset(field);
            const lval = def.create_select_lval(field, offset, expanded_type, null, self.ctx.allocator());
            try self.generate_pattern(term, expanded_type, lval);
        }
    } else if (pattern.* == .array_value) {
        const lhs_expanded_type = self.ctx.typecheck.typeof(pattern).expand_identifier();
        for (pattern.children().items, 0..) |term, field| {
            const expanded_type = def.get_expanded_type().child().expand_identifier();
            const length: ?*lval_.L_Value = self.generate_indexable_length(def, lhs_expanded_type, pattern.token().span);
            const idx = self.lval_from_int(field, prelude_.int_type, pattern.token().span);
            const index = def.create_index_lval(idx, length, expanded_type, self.ctx.allocator());
            try self.generate_pattern(term, expanded_type, index);
        }
    } else if (pattern.* == .enum_value) {
        const expanded_type = pattern.enum_value.domain.?.child().expand_identifier();
        const field = pattern.pos().?;
        const lval = def.create_select_lval(field, 0, expanded_type, null, self.ctx.allocator());
        try self.generate_pattern(pattern.enum_value.init.?, expanded_type, lval);
    }
}
