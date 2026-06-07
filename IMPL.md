# GenericArg Three-Variant Refactor

## Goal

Replace the two-variant `GenericArg` with three variants, eliminating scattered
`is_const_param_ref()` checks from all `GenericArg` contexts.

## New union

```zig
// src/ast/generic_arg.zig
pub const GenericArg = union(enum) {
    type_arg: *Type_AST,
    const_arg: *AST,          // any concrete const value (literal, comptime block, module const)
    const_param_ref: *Symbol, // reference to a const_param_decl — transient, gone by instantiation
};
```

## Invariant

`const_param_ref` is eliminated by substitution before `monomorphize_generic_apply` is
called for real. An `assert(arg != .const_param_ref)` is placed after the early-exit loop
in `monomorphize_generic_apply` to enforce this.

`array_of.len` remains a raw `*AST`; `is_const_param_ref()` stays for those call sites.

## Classification seams

### AST path — `decorate.zig : decorate_postfix : .index`

Already builds `generic_args` from scratch. Update the const-param branch:

```zig
if (i < params.len and params[i].* == .const_param_decl) {
    if (arg.is_const_param_ref()) {
        try generic_args.append(.{ .const_param_ref = arg.symbol().? });
    } else {
        try generic_args.append(.{ .const_arg = arg });
    }
} else {
    try generic_args.append(.{ .type_arg = Type_AST.from_ast(arg, self.ctx.allocator()) });
}
```

### Type_AST path — `decorate.zig : decorate_postfix_type : .generic_apply`

Reclassify in-place before calling `instantiate`. By `postfix_type`, all arg-type
identifiers already have their symbols set (from `prefix_type` scope lookup).

```zig
.generic_apply => {
    if (ast.lhs().symbol()) |sym| {
        if (sym.decl) |decl| {
            const params = decl.generic_params().items;
            for (ast.generic_apply.args.items, 0..) |*arg, i| {
                if (i >= params.len or params[i].* != .const_param_decl) continue;
                switch (arg.*) {
                    .type_arg => |ty| {
                        if (ty.* == .identifier and ty.symbol() != null and
                            ty.symbol().?.decl != null and
                            ty.symbol().?.decl.?.* == .const_param_decl)
                        {
                            arg.* = .{ .const_param_ref = ty.symbol().? };
                        }
                    },
                    else => {},
                }
            }
        }
    }
    try generic_apply_.instantiate(ast, self.ctx);
},
```

## Downstream changes

| File | Change |
|---|---|
| `walker.zig` | Add `.const_param_ref => {}` in both AST and Type_AST `generic_apply` arms |
| `symbol.zig : monomorphize` | Early exit: `.const_param_ref => return self` |
| `type.zig : is_generic` | `.const_param_ref => return true` |
| `type.zig : clone` | `.const_param_ref => |sym| subst.get_const(sym.name) → .const_arg else keep` |
| `unification.zig : unify_inner generic_apply` | Switch on `const_param_ref` vs `const_arg` directly |
| `type_map.zig : generic_args_match` | Add `assert(arg != .const_param_ref)` |
| `generic_apply.zig` | Assert `arg != .const_param_ref` after early-exit loop |

## What does NOT change

- `is_const_param_ref()` in `ast.zig` — kept for `array_of.len` call sites
- `array_of.len` type — stays `*AST`
- `const_arg` name — unchanged

## Steps

1. `generic_arg.zig` — add `const_param_ref: *Symbol`
2. `decorate.zig` — AST path reclassification (postfix .index)
3. `decorate.zig` — Type_AST path reclassification (postfix_type .generic_apply)
4. `walker.zig` — handle `const_param_ref`
5. `symbol.zig` — early exit
6. `type.zig` — `is_generic`, `clone`
7. `unification.zig` — `unify_inner` generic_apply case
8. `type_map.zig` — assert
9. `generic_apply.zig` — assert
10. Build, fix errors, run tests
