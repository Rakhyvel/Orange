///! Encapsulates information about modules and their hashes for a package.
const std = @import("std");

const Self = @This();

json_parsed: ?std.json.Parsed(std.json.Value),
json_absolute_path: []const u8,

/// Attempts to open up the hash.json file if it exists, and reads in the json data. If the file doesn't exist, simply
/// sets the json data to null to be filled in later.
///
/// Potentially allocates for json data. Call deinit to deinitialize.
pub fn init(package_absolute_path: []const u8, allocator: std.mem.Allocator) !Self {
    const json_absolute_paths = [_][]const u8{ package_absolute_path, "build", "hash.json" };
    const json_absolute_path = try std.fs.path.join(allocator, &json_absolute_paths);

    const contents = read_contents(json_absolute_path, allocator) catch |err| switch (err) {
        error.FileNotFound => return Self{ .json_parsed = null, .json_absolute_path = json_absolute_path },
        else => return error.CompileError,
    };
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, contents, .{}) catch
        return Self{ .json_parsed = null, .json_absolute_path = json_absolute_path };
    return Self{
        .json_parsed = parsed,
        .json_absolute_path = json_absolute_path,
    };
}

/// Deinitializes a module hash struct
pub fn deinit(self: *Self) void {
    if (self.json_parsed) |parsed| {
        parsed.deinit();
    }
}

fn read_contents(absolute_path: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var file = std.fs.openFileAbsolute(absolute_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        else => return error.CompileError,
    };
    defer file.close();

    const stat = file.stat() catch return error.CompileError;
    const contents = try file.readToEndAlloc(allocator, stat.size);
    return contents;
}

pub fn get_module_stored_hash(self: *const Self, module_name: []const u8) ?[]const u8 {
    if (self.json_parsed) |old_json_parsed| {
        const module_lookup_res = old_json_parsed.value.object.get(module_name) orelse return null;
        return module_lookup_res.string;
    } else {
        return null;
    }
}

pub fn set_module_hash(self: *Self, module_name: []const u8, hash_number_string: []const u8, allocator: std.mem.Allocator) void {
    if (self.json_parsed == null) {
        self.json_parsed = std.json.parseFromSlice(std.json.Value, allocator, "{}", .{}) catch unreachable;
    }
    self.json_parsed.?.value.object.put(module_name, std.json.Value{ .string = hash_number_string }) catch unreachable;
}

pub fn output_new_json(self: *Self, allocator: std.mem.Allocator) void {
    if (self.json_parsed == null) {
        self.json_parsed = std.json.parseFromSlice(std.json.Value, allocator, "{}", .{}) catch unreachable;
    }
    var json_file_handle = std.fs.createFileAbsolute(self.json_absolute_path, .{}) catch unreachable;
    defer json_file_handle.close();
    var out: std.io.Writer.Allocating = .init(allocator);
    std.json.Stringify.value(self.json_parsed.?.value, .{}, &out.writer) catch unreachable;
    json_file_handle.writeAll(out.written()) catch unreachable;
}
