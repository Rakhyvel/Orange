const std = @import("std");

/// Returns the type of the field with a given name in a Zig union type
fn Unwrapped(comptime Union: type, comptime field: []const u8) type {
    return inline for (std.meta.fields(Union)) |variant| {
        const Struct = variant.type;
        const s: Struct = undefined;
        if (@hasField(Struct, field)) break @TypeOf(@field(s, field));
    } else @compileError("No such field in any of the variants!");
}

/// Generically retrieve the value of a field in a Zig union type
pub fn get_struct_field(u: anytype, comptime field: []const u8) Unwrapped(@TypeOf(u), field) {
    return switch (u) {
        inline else => |v| if (@hasField(@TypeOf(v), field)) @field(v, field) else error.NoField,
    } catch {
        std.debug.panic("compiler error: `{s}` does not have field `{s}` {}", .{ @tagName(u), field, Unwrapped(@TypeOf(u), field) });
    };
}

/// Generically retrieve the value of a field in a Zig union type
pub fn has_struct_field(u: anytype, comptime field: []const u8) bool {
    _ = switch (u) {
        inline else => |v| if (@hasField(@TypeOf(v), field)) @field(v, field) else error.NoField,
    } catch {
        return false;
    };
    return true;
}

/// Generically retrieve a reference to a field in a Zig union type
pub fn get_field_ref(u: anytype, comptime field: []const u8) *Unwrapped(@TypeOf(u.*), field) {
    return switch (u.*) {
        inline else => |*v| &@field(v, field),
    };
}

pub fn get_field_const_ref(u: anytype, comptime field: []const u8) *const Unwrapped(@TypeOf(u.*), field) {
    return switch (u.*) {
        inline else => |*v| &@field(v, field),
    };
}

/// Generically set a field in a Zig union type
pub fn set_field(u: anytype, comptime field: []const u8, val: Unwrapped(@TypeOf(u.*), field)) void {
    switch (u.*) {
        inline else => |*v| if (@hasField(@TypeOf(v.*), field)) {
            @field(v, field) = val;
        } else {
            std.debug.panic("compiler error: `{s}` does not have field `{s}`", .{ @tagName(u.*), field });
        },
    }
}
