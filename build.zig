const std = @import("std");

const qbe_default_source = &.{
    "qbe-1.2/main.c",
    "qbe-1.2/util.c",
    "qbe-1.2/parse.c",
    "qbe-1.2/abi.c",
    "qbe-1.2/cfg.c",
    "qbe-1.2/mem.c",
    "qbe-1.2/ssa.c",
    "qbe-1.2/alias.c",
    "qbe-1.2/load.c",
    "qbe-1.2/copy.c",
    "qbe-1.2/fold.c",
    "qbe-1.2/simpl.c",
    "qbe-1.2/live.c",
    "qbe-1.2/spill.c",
    "qbe-1.2/rega.c",
    "qbe-1.2/emit.c",

    "qbe-1.2/amd64/targ.c",
    "qbe-1.2/amd64/sysv.c",
    "qbe-1.2/amd64/isel.c",
    "qbe-1.2/amd64/emit.c",

    "qbe-1.2/arm64/targ.c",
    "qbe-1.2/arm64/abi.c",
    "qbe-1.2/arm64/isel.c",
    "qbe-1.2/arm64/emit.c",

    "qbe-1.2/rv64/targ.c",
    "qbe-1.2/rv64/abi.c",
    "qbe-1.2/rv64/isel.c",
    "qbe-1.2/rv64/emit.c",
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const zParsecDep = b.dependency("ZigParsec", .{});
    const zParsecModule = zParsecDep.module("ZigParsec");

    const exe = b.addExecutable(.{
        .name = "ShadingLanguage",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    exe.root_module.addImport("ZigParsec", zParsecModule);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // const configure_step = b.step("configure", "Configure config.h");
    // configure_step.dependOn(&configHeader.step);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const lib_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/root.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}
