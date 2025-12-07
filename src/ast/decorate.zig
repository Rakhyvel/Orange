//! This file represents a struct for an AST walk, and is used to decorate identifier ASTs with symbols they refer to.

const std = @import("std");
const ast_ = @import("../ast/ast.zig");
const core_ = @import("../hierarchy/core.zig");
const Compiler_Context = @import("../hierarchy/compiler.zig");
const errs_ = @import("../util/errors.zig");
const Scope = @import("../symbol/scope.zig");
const Symbol = @import("../symbol/symbol.zig");
const token_ = @import("../lexer/token.zig");
const Tree_Writer = @import("../ast/tree_writer.zig");
const Type_AST = @import("../types/type.zig").Type_AST;
const walk_ = @import("../ast/walker.zig");

scope: *Scope,
ctx: *Compiler_Context,

const Self = @This();

pub fn new(scope: *Scope, ctx: *Compiler_Context) Self {
    return Self{
        .scope = scope,
        .ctx = ctx,
    };
}

pub fn prefix(self: Self, ast: *ast_.AST) walk_.Error!?Self {
    return self.decorate_prefix(ast);
}

pub fn postfix(self: Self, ast: *ast_.AST) walk_.Error!void {
    return self.decorate_postfix(ast);
}

pub fn prefix_type(self: Self, _type: *Type_AST) walk_.Error!?Self {
    return self.decorate_prefix_type(_type);
}

pub fn postfix_type(self: Self, _type: *Type_AST) walk_.Error!void {
    return self.decorate_postfix_type(_type);
}

fn decorate_prefix(self: Self, ast: *ast_.AST) walk_.Error!?Self {
    switch (ast.*) {
        else => return self,

        .identifier => {
            if (ast.symbol() != null) {
                return self;
            }

            const res = self.scope.lookup(ast.token().data, .{ .allow_modules = false });
            switch (res) {
                // Found the symbol, decorate the identifier AST with it
                .found => ast.set_symbol(res.found),

                // Couldn't find the symbol
                .not_found => {
                    self.ctx.errors.add_error(errs_.Error{ .undeclared_identifier = .{ .identifier = ast.token(), .expected = null } });
                    return error.CompileError;
                },

                // Found the symbol, but must cross a comptime-boundary to access it, and it is not const
                .found_but_rt => {
                    self.ctx.errors.add_error(errs_.Error{ .comptime_access_runtime = .{ .identifier = ast.token() } });
                    return error.CompileError;
                },

                // Found the symbol, but must cross an inner-function boundary to access it, and it is not const
                .found_but_fn => {
                    self.ctx.errors.add_error(errs_.Error{ .inner_fn_access_runtime = .{ .identifier = ast.token() } });
                    return error.CompileError;
                },
            }

            if ((ast.symbol().?.kind == .let or ast.symbol().?.kind == .mut) // local variable
            and ast.symbol().?.decl.?.decl_init() != null // not a parameter
            and !ast.symbol().?.defined // not defined
            ) {
                self.ctx.errors.add_error(errs_.Error{ .use_before_def = .{ .identifier = ast.token() } });
                return error.CompileError;
            }

            return self;
        },

        .@"if", .match, .mapping, .@"while", .@"for", .block, .impl, .trait, .struct_decl, .enum_decl, .type_alias, .fn_decl => {
            var new_context = self;
            new_context.scope = ast.scope().?;
            return new_context;
        },

        .method_decl => {
            var new_context = self;
            if (ast.symbol() != null) {
                new_context.scope = ast.symbol().?.scope;
            }
            return new_context;
        },

        .access => {
            // Capture scope, so that `Trait::member` accesses are began at the access point
            ast.set_scope(self.scope);
            return self;
        },
    }
}

fn decorate_prefix_type(self: Self, _type: *Type_AST) walk_.Error!?Self {
    switch (_type.*) {
        else => return self,

        .identifier => {
            if (_type.symbol()) |_| {
                // Do not re-decorate.
                // Symbols are injected into ident types for lexical generic scoping
                // Keep those symbols the way they are, even if they're not "visible" from this scope!
                return self;
            }
            const res = self.scope.lookup(_type.token().data, .{});
            switch (res) {
                // Found the symbol, decorate the identifier AST with it
                .found => _type.set_symbol(res.found),

                // Couldn't find the symbol
                .not_found => {
                    self.ctx.errors.add_error(errs_.Error{ .undeclared_identifier = .{ .identifier = _type.token(), .expected = null } });
                    return error.CompileError;
                },

                // Found the symbol, but must cross a comptime-boundary to access it, and it is not const
                .found_but_rt => {
                    self.ctx.errors.add_error(errs_.Error{ .comptime_access_runtime = .{ .identifier = _type.token() } });
                    return error.CompileError;
                },

                // Found the symbol, but must cross an inner-function boundary to access it, and it is not const
                .found_but_fn => {
                    self.ctx.errors.add_error(errs_.Error{ .inner_fn_access_runtime = .{ .identifier = _type.token() } });
                    return error.CompileError;
                },
            }

            return self;
        },
    }
}

fn decorate_postfix(self: Self, ast: *ast_.AST) walk_.Error!void {
    switch (ast.*) {
        else => {},

        .access => ast.set_symbol(try self.resolve_access_ast(ast)),

        .call => {
            if (ast.lhs().* == .enum_value) {
                // Enum value
                ast.lhs().enum_value.init = ast.children().items[0];
                ast.* = ast.lhs().*;
            } else if (ast.lhs().refers_to_type()) {
                // Struct value construction
                const struct_type = Type_AST.from_ast(ast.lhs(), self.ctx.allocator());
                const struct_value = ast_.AST.create_struct_value(
                    ast.lhs().token(),
                    struct_type,
                    ast.children().*,
                    self.ctx.allocator(),
                );
                ast.* = struct_value.*;
            }
        },

        .index => {
            var child = ast.lhs();
            if (child.* != .identifier and child.* != .access) return;

            const sym = child.symbol() orelse return;
            const decl = sym.decl orelse return;

            // child points to a generic function
            if (decl.num_generic_params() > 0) {
                var types = std.array_list.Managed(*Type_AST).init(self.ctx.allocator());
                for (ast.children().items) |arg| {
                    try types.append(Type_AST.from_ast(arg, self.ctx.allocator()));
                }
                ast.* = ast_.AST.create_generic_apply(ast.token(), child, types, self.ctx.allocator()).*;
                try self.monomorphize_generic_apply(ast);
            }
        },

        .select => {
            var child = ast.lhs();
            if (child.* == .identifier and child.symbol() != null and child.symbol().?.is_type()) {
                const enum_value = ast_.AST.create_enum_value(ast.rhs().token(), self.ctx.allocator());
                enum_value.enum_value.base = Type_AST.create_type_identifier(child.token(), self.ctx.allocator());
                enum_value.enum_value.base.?.set_symbol(child.symbol());
                ast.* = enum_value.*;
                return;
            }

            // child points to a generic function
            if (child.* != .identifier and child.* != .access) return;

            const sym = child.symbol() orelse return;
            const decl = sym.decl orelse return;

            if (decl.* == .context_decl) {
                const fn_ctx = decl.decl_typedef().?;
                const context_val_symbol = self.scope.parent.?.context_lookup(fn_ctx, self.ctx) orelse {
                    self.ctx.errors.add_error(errs_.Error{ .missing_context = .{
                        .span = ast.token().span,
                        .context = Type_AST.create_type_identifier(decl.token(), self.ctx.allocator()),
                    } });
                    return error.CompileError;
                };
                var token = ast.token();
                token.data = context_val_symbol.name;
                const context_val_ident = ast_.AST.create_identifier(token, self.ctx.allocator());
                context_val_ident.set_symbol(context_val_symbol);
                ast.set_lhs(context_val_ident);
            }
        },

        .write => {
            const format_all_call = try self.create_format_all_call(ast, ast.write.writer);
            ast.* = format_all_call.*;
        },

        .print => {
            const context_val_symbol = self.scope.parent.?.context_lookup(core_.io_context, self.ctx) orelse {
                self.ctx.errors.add_error(errs_.Error{ .missing_context = .{
                    .span = ast.token().span,
                    .context = Type_AST.create_type_identifier(core_.io_context.token(), self.ctx.allocator()),
                } });
                return error.CompileError;
            };

            // Append writer to the call args
            var writer_token = ast.token();
            writer_token.data = context_val_symbol.name;
            const io_context = ast_.AST.create_identifier(writer_token, self.ctx.allocator());
            io_context.set_symbol(context_val_symbol);
            var writer_field_token = ast.token();
            writer_field_token.data = "writer";
            const writer_field = ast_.AST.create_field(writer_field_token, self.ctx.allocator());
            const writer = ast_.AST.create_select(writer_token, io_context, writer_field, self.ctx.allocator());

            const format_all_call = try self.create_format_all_call(ast, writer);

            ast.* = format_all_call.*;
        },

        .format_args => {
            const format_args_slice = try self.create_format_args_slice(ast);
            ast.* = format_args_slice.*;
        },

        .binding => {
            for (ast.binding.decls.items) |decl| {
                if (decl.* == .decl) {
                    decl.decl.name.symbol().?.defined = true;
                }
            }
        },
        .decl => {
            if (ast.decl.init != null and ast.decl.init.?.* == .access and ast.decl.init.?.symbol().?.kind == .type) {
                const init_symbol = ast.decl.init.?.symbol().?;
                ast.* = ast_.AST.create_type_alias(
                    ast.token(),
                    ast.decl.name,
                    Type_AST.from_ast(ast.decl.init.?, self.ctx.allocator()),
                    std.array_list.Managed(*ast_.AST).init(self.ctx.allocator()),
                    self.ctx.allocator(),
                ).*;
                ast.type_alias.init.?.set_symbol(init_symbol);
            }
        },
        .generic_apply => return self.monomorphize_generic_apply(ast),
        .trait => try self.scope.traits.put(ast, void{}),
        .enum_decl => try self.scope.enums.append(ast),
        .@"test" => try self.scope.tests.append(ast),
    }
}

fn decorate_postfix_type(self: Self, ast: *Type_AST) walk_.Error!void {
    switch (ast.*) {
        .access => ast.set_symbol(try self.resolve_access_ast(ast.access.inner_access)),

        else => {},
    }
}

fn resolve_access_ast(self: Self, ast: *ast_.AST) walk_.Error!*Symbol {
    const stripped_lhs = if (ast.lhs().* == .addr_of) ast.lhs().expr() else ast.lhs();
    const symbol = stripped_lhs.symbol().?;

    return self.resolve_access_symbol(symbol, ast, stripped_lhs);
}

fn resolve_access_symbol(self: Self, symbol: *Symbol, ast: *ast_.AST, stripped_lhs: *ast_.AST) walk_.Error!*Symbol {
    switch (symbol.kind) {
        .module => return try self.resolve_access_module(symbol, ast.rhs()),

        .import => {
            const module_symbol = symbol.kind.import.real_symbol.?;
            return try self.resolve_access_module(module_symbol, ast.rhs());
        },

        .import_inner => {
            const new_symbol =
                if (symbol.decl.?.* == .type_alias)
                    try self.resolve_access_ast(symbol.init_typedef().?.access.inner_access)
                else
                    try self.resolve_access_ast(symbol.init_value().?);
            return self.resolve_access_symbol(new_symbol, ast, stripped_lhs);
        },

        .type => return try self.resolve_access_const(Type_AST.from_ast(stripped_lhs, self.ctx.allocator()), ast.rhs(), ast.scope().?),

        else => {
            self.ctx.errors.add_error(errs_.Error{
                .member_not_in_module = .{
                    .span = ast.rhs().token().span,
                    .identifier = ast.rhs().token().data,
                    .name = "symbol",
                    .module_name = symbol.name,
                },
            });
            return error.CompileError;
        },
    }
}

// TODO: This has a lot of similarities to monomorphizing a generic_apply type in type_validate.zig
fn monomorphize_generic_apply(self: Self, ast: *ast_.AST) walk_.Error!void {
    const sym = ast.lhs().symbol().?;
    const params = sym.decl.?.generic_params();
    if (params.items.len != ast.generic_apply._children.items.len) {
        self.ctx.errors.add_error(errs_.Error{ .mismatch_arity = .{
            .span = ast.token().span,
            .takes = params.items.len,
            .given = ast.generic_apply._children.items.len,
            .thing_name = sym.name,
            .takes_name = "type parameter",
            .given_name = "argument",
        } });
        return error.CompileError;
    }

    for (ast.generic_apply._children.items, 0..) |child, i| {
        try self.ctx.validate_type.validate_type(child);

        const param = params.items[i];
        const sat_res = child.satisfies_all_constraints(param.type_param_decl.constraints.items);
        switch (sat_res) {
            .satisfies => {},
            .not_impl => |unimpld| {
                self.ctx.errors.add_error(errs_.Error{ .type_not_impl_trait = .{
                    .span = child.token().span,
                    .trait_name = unimpld.name,
                    ._type = child,
                } });
                return error.CompileError;
            },
            .not_eq => |uneqd| {
                self.ctx.errors.add_error(errs_.Error{ .eq_constraint_failed = .{
                    .call_span = child.token().span,
                    .associated_type_name = uneqd.associated_type_name,
                    .constraint_span = uneqd.constraint_span,
                    .impl_span = uneqd.impl_span,
                    .expected = uneqd.expected,
                    .got = uneqd.got,
                } });
                return error.CompileError;
            },
        }
    }

    if (ast.generic_apply.state == .unmorphed) {
        ast.generic_apply.state = .morphing;
        ast.generic_apply._symbol = try sym.monomorphize(ast.generic_apply._children, self.ctx);
        ast.generic_apply.state = .morphed;
    }
}

/// Takes in an a qualified-name AST and returns the symbol that it refers to. This requires looking up modules and packages, and so the
/// compiler instance is required.
fn resolve_symbol_from_ast(self: Self, ast: *ast_.AST) walk_.Error!*Symbol {
    switch (ast.*) {
        .access => return self.resolve_symbol_from_access(ast),
        .identifier, .pattern_symbol => return self.resolve_symbol_from_identlike(ast),
        .generic_apply => return ast.symbol().?,
        else => std.debug.panic("compiler error: fell through {f}", .{ast}),
    }
}

/// Takes in an access-ast and resolves the symbol it refers to
fn resolve_symbol_from_access(self: Self, access_ast: *ast_.AST) walk_.Error!*Symbol {
    const stripped_lhs = if (access_ast.lhs().* == .addr_of) access_ast.lhs().expr() else access_ast.lhs();
    const expanded_lhs = stripped_lhs.symbol().?;
    const symbol = try self.resolve_access_symbol(expanded_lhs, access_ast.rhs(), access_ast.scope().?);
    access_ast.set_symbol(symbol);
    return symbol;
}

/// Takes in a identifier-like AST (such as an identifier or a pattern symbol) and returns the symbol it refers to
fn resolve_symbol_from_identlike(self: Self, identlike_ast: *ast_.AST) walk_.Error!*Symbol {
    switch (identlike_ast.symbol().?.kind) {
        .module => {
            // We found it as a module _because_ it wasn't imported
            self.ctx.errors.add_error(errs_.Error{ .undeclared_identifier = .{ .identifier = identlike_ast.token(), .expected = null } });
            return error.CompileError;
        },
        .import => return try self.resolve_symbol_from_import_identlike(identlike_ast),
        else => return identlike_ast.symbol().?,
    }
}

/// Takes in an identlike AST that refers to an import symbol, and returns the module symbol that it imports
fn resolve_symbol_from_import_identlike(self: Self, identlike_ast: *ast_.AST) !*Symbol {
    const this_module = identlike_ast.symbol().?.scope.module.?;
    const curr_package_path = this_module.get_package_abs_path();
    var module_path_name = std.array_list.Managed(u8).init(self.ctx.allocator());
    defer module_path_name.deinit();
    try module_path_name.print("{s}.orng", .{identlike_ast.token().data});
    const package_build_paths = [_][]const u8{ curr_package_path, module_path_name.items };
    const other_module_dir = try std.fs.path.join(self.ctx.allocator(), &package_build_paths);

    if (std.mem.eql(u8, identlike_ast.symbol().?.kind.import.real_name, "core")) {
        return core_.core_symbol.?;
    }

    // TODO: If these are both non-null, report the ambiguity
    //       If these are both null, report that the module was not found

    const local_module_lookup = self.ctx.lookup_module(other_module_dir);
    const foreign_module_lookup = self.ctx.lookup_package_root_module(curr_package_path, identlike_ast.symbol().?.kind.import.real_name);

    // TODO: This doesn't work when we do:
    //   import module::member
    return (local_module_lookup orelse foreign_module_lookup) orelse {
        std.debug.panic("couldn't find module {s} or package {s}", .{ other_module_dir, identlike_ast.symbol().?.kind.import.real_name });
    };
}

/// Resolves a symbol access from a module
fn resolve_access_module(self: Self, module_symbol: *Symbol, rhs: *ast_.AST) walk_.Error!*Symbol {
    std.debug.assert(module_symbol.kind == .module);
    const module_lookup_res = module_symbol.init_value().?.scope().?.lookup(
        rhs.token().data,
        .{},
    );
    const rhs_decl = switch (module_lookup_res) {
        .found => module_lookup_res.found.decl.?,
        else => {
            self.ctx.errors.add_error(errs_.Error{
                .member_not_in_module = .{
                    .span = rhs.token().span,
                    .identifier = rhs.token().data,
                    .name = "module",
                    .module_name = module_symbol.name,
                },
            });
            return error.CompileError;
        },
    };
    return extract_symbol_from_decl(rhs_decl);
}

/// Resolves a symbol access on a constant symbol, likely a trait lookup
fn resolve_access_const(self: Self, lhs: *Type_AST, rhs: *ast_.AST, scope: *Scope) walk_.Error!*Symbol {
    const rhs_decl = scope.lookup_impl_member(lhs, rhs.token().data, self.ctx) catch return error.CompileError;
    if (rhs_decl == null) {
        self.ctx.errors.add_error(errs_.Error{
            .type_not_impl_method = .{
                .span = rhs.token().span,
                .method_name = rhs.token().data,
                ._type = lhs,
            },
        });
        return error.CompileError;
    } else {
        return extract_symbol_from_decl(rhs_decl.?);
    }
}

/// Extracts the symbol that a decl-like AST decls
fn extract_symbol_from_decl(decl: *ast_.AST) *Symbol {
    if (decl.* == .decl) {
        return decl.decl.name.symbol().?;
    } else if (decl.* == .method_decl or decl.* == .fn_decl or decl.* == .trait or decl.* == .struct_decl or decl.* == .enum_decl or decl.* == .type_alias or decl.* == .context_decl or decl.* == .type_param_decl) {
        return decl.symbol().?;
    } else if (decl.* == .binding) {
        return decl.binding.pattern.symbol().?;
    } else {
        std.debug.panic("compiler error: unsupported access symbol resolution for decl-like AST: {s}", .{@tagName(decl.*)});
    }
}

fn create_format_all_call(self: *const Self, ast: *ast_.AST, writer: *ast_.AST) !*ast_.AST {
    var format_all = ast_.AST.create_identifier(ast.token(), self.ctx.allocator());
    format_all.set_symbol(self.ctx.get_core_symbol("format_all"));
    var args = std.array_list.Managed(*ast_.AST).init(self.ctx.allocator());
    try args.append(writer);

    const args_slice = try self.create_format_args_slice(ast);
    try args.append(args_slice);

    const format_all_call = ast_.AST.create_call(ast.token(), format_all, args, self.ctx.allocator());
    return format_all_call;
}

fn create_format_args_slice(self: *const Self, ast: *ast_.AST) !*ast_.AST {
    var array_terms = std.array_list.Managed(*ast_.AST).init(self.ctx.allocator());
    var format_ident_token = ast.token();
    format_ident_token.data = "Format";
    var format_ident = Type_AST.create_type_identifier(format_ident_token, self.ctx.allocator());
    format_ident.set_symbol(self.ctx.get_core_symbol("Format"));
    const dyn_type = Type_AST.create_dyn_type(ast.token(), format_ident, false, self.ctx.allocator());
    for (ast.children().items) |child| {
        const dyn_value = ast_.AST.create_dyn_value(
            child.token(),
            dyn_type,
            child,
            self.scope,
            false,
            self.ctx.allocator(),
        );
        try array_terms.append(dyn_value);
    }
    const args_array = ast_.AST.create_array_value(ast.token(), array_terms, self.ctx.allocator());
    return ast_.AST.create_slice_of(ast.token(), args_array, false, self.ctx.allocator());
}
