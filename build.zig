const std = @import("std");

// Have to live on the edge because of ZLS >:-(
pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const use_valgrind = b.option(bool, "valgrind", "Build valgrind-compatible (baseline CPU + valgrind support)") orelse false;
    const sanitize_thread = b.option(bool, "sanitize-thread", "Enable ThreadSanitizer") orelse false;
    const sanitize_c = b.option(bool, "sanitize-c", "Enable the C/UB sanitizer (full)") orelse false;

    // valgrind needs a baseline CPU, otherwise build for the native host
    const target = if (use_valgrind)
        b.resolveTargetQuery(.{ .cpu_model = .baseline })
    else
        b.graph.host;

    const opts = San_Options{
        .valgrind = use_valgrind,
        .sanitize_thread = sanitize_thread,
        .sanitize_c = if (sanitize_c) .full else .off,
    };

    executable(b, optimize, target, opts, "orng-test", "src/test.zig", "orng-test", "Runs the integration tests");
    executable(b, optimize, target, opts, "orng", "src/main.zig", "orng", "Runs the compiler");
}

const San_Options = struct {
    valgrind: bool,
    sanitize_thread: bool,
    sanitize_c: std.zig.SanitizeC,
};

fn executable(b: *std.Build, optimize: std.builtin.OptimizeMode, target: std.Build.ResolvedTarget, san: San_Options, name: []const u8, path: []const u8, run: []const u8, run_desc: []const u8) void {
    const exe = b.addExecutable(.{
        .name = name,
        .root_module = b.addModule("string", .{
            .root_source_file = b.path(path),
            .target = target,
            .optimize = optimize,
            .valgrind = san.valgrind,
            .sanitize_thread = san.sanitize_thread,
            .sanitize_c = san.sanitize_c,
        }),
        .use_llvm = true,
    });
    exe.want_lto = false;
    const install_cmd = b.addInstallArtifact(exe, .{});
    const install_step = b.step(run, run_desc);
    install_step.dependOn(&install_cmd.step);
}
