/// This format wrapper formats a type to a canonical C representation string. Canonicalizations are stable
/// across compilations, cyclic tolerant, allow for extern types.
const std = @import("std");
const prelude_ = @import("../hierarchy/prelude.zig");
const String = @import("../zig-string/zig-string.zig").String;
const Type_AST = @import("../types/type.zig").Type_AST;
const C_Type_Map = @import("../types/type_map.zig").C_Type_Map;

const Self = @This();

type: *Type_AST,
case: enum { lower, upper } = .lower,

pub fn format(self: *const Self, writer: *std.io.Writer) !void {
    const hash = try Self.hash_type(self.type);
    switch (self.case) {
        .lower => try writer.print("orange_type_{X}", .{hash}),
        .upper => try writer.print("ORANGE_TYPE_{X}", .{hash}),
    }
}

/// Returns the hash for a given type
pub fn hash_type(_type: *Type_AST) !usize {
    var seen_map = C_Type_Map(usize).init(std.heap.page_allocator);
    defer seen_map.deinit();

    var next_id: usize = 0;

    var str = String.init(std.heap.page_allocator);
    defer str.deinit();
    var writer = str.writer(&.{});
    const writer_intfc = &writer.interface;

    try hash_type_internal(_type, &seen_map, &next_id, writer_intfc);

    return std.hash.Fnv1a_64.hash(str.str());
}

/// Returns a String object representing the canonical representation of the type
pub fn canonical_rep(_type: *Type_AST) !String {
    var seen_map = C_Type_Map(usize).init(std.heap.page_allocator);
    defer seen_map.deinit();

    var next_id: usize = 0;

    var str = String.init(std.heap.page_allocator);
    var writer = str.writer(&.{});
    const writer_intfc = &writer.interface;

    try hash_type_internal(_type, &seen_map, &next_id, writer_intfc);

    return str;
}

/// Hashes a type to its C canonical representation.
///
/// ### Params
/// * `_type`: The type to hash
/// * `seen_map`: A C-equivalence linear map of types that have been seen to their ID. Used for back edges in cycles.
/// * `next_id`: Pointer to a usize to monotonically increase, for assigning IDs to types.
/// * `writer`: Writer to write the repr to.
fn hash_type_internal(
    _type: *Type_AST,
    seen_map: *C_Type_Map(usize),
    next_id: *usize,
    writer: *std.io.Writer,
) error{WriteFailed}!void {
    var real_type = _type.expand_identifier();

    // Already computed the hash, emit a back ref with the id
    if (seen_map.contains(real_type)) {
        try writer.print("ref{}", .{seen_map.get(real_type).?});
        return;
    }

    // Assign a unique ID to this type
    const id = next_id.*;
    next_id.* += 1;
    seen_map.put(real_type, id) catch unreachable;

    // Compute children hashes depending on type kind
    switch (real_type.*) {
        .identifier => {
            const info = prelude_.info_from_name(real_type.token().data);
            if (info != null) {
                try writer.print("{s}", .{info.?.canon_name});
            } else {
                try writer.print("{s}", .{real_type.token().data});
            }
        },
        .addr_of => {
            try writer.print("p", .{});
            try hash_type_internal(real_type.child(), seen_map, next_id, writer);
        },
        .anyptr_type => try writer.print("void", .{}),
        .unit_type => try writer.print("unit", .{}),
        .struct_type, .tuple_type => {
            try writer.print("tuple{}", .{non_unit_len(real_type)});
            try append_tuple_fields(real_type, seen_map, next_id, writer);
        },
        .context_type => {
            try writer.print("context{}", .{non_unit_len(real_type)});
            try append_tuple_fields(real_type, seen_map, next_id, writer);
        },
        .array_of => {
            try writer.print("array{}_", .{real_type.array_of.len.int.data});
            try hash_type_internal(real_type.child(), seen_map, next_id, writer);
        },
        .enum_type => {
            // TODO will likely need to emit the tag size, too
            try writer.print("enum{}", .{non_unit_len(real_type)});
            try append_enum_fields(real_type, seen_map, next_id, writer);
        },
        .untagged_sum_type => {
            try writer.print("union", .{});
            try append_enum_fields(real_type.child().expand_identifier(), seen_map, next_id, writer);
        },
        .dyn_type => {
            try writer.print("dyn_{s}", .{real_type.child().symbol().?.name});
        },
        .function => {
            try writer.print("function{}", .{non_unit_len(real_type)});
            try append_tuple_fields(real_type, seen_map, next_id, writer);
            try writer.print("_", .{});
            try hash_type_internal(real_type.rhs(), seen_map, next_id, writer);
        },
        else => try writer.print("({t})", .{real_type.*}),
    }
}

/// Calculate how many fields would be emitted for C. Orange unit typed fields do not get emitted.
fn non_unit_len(real_type: *Type_AST) usize {
    var ret: usize = 0;
    for (real_type.children().items) |child| {
        if (!child.is_c_void_type()) ret += 1;
    }
    return ret;
}

fn append_tuple_fields(
    _type: *Type_AST,
    seen_map: *C_Type_Map(usize),
    next_id: *usize,
    writer: *std.io.Writer,
) !void {
    for (_type.children().items) |child| {
        try writer.print("_", .{});
        try hash_type_internal(child, seen_map, next_id, writer);
    }
}

fn append_enum_fields(
    _type: *Type_AST,
    seen_map: *C_Type_Map(usize),
    next_id: *usize,
    writer: *std.io.Writer,
) !void {
    for (_type.children().items) |child| {
        if (child.is_c_void_type()) continue;
        try writer.print("_", .{});
        try hash_type_internal(child, seen_map, next_id, writer);
    }
}
