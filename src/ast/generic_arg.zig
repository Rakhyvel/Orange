const AST = @import("../ast/ast.zig").AST;
const Type_AST = @import("../types/type.zig").Type_AST;

/// A generic argument, either a type (for type params) or a comptime value (for const params)
pub const GenericArg = union(enum) {
    type_arg: *Type_AST,
    const_arg: *AST,
};
