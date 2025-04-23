const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zg_dep = b.dependency("zg", .{
        .target = target,
        .optimize = optimize,
    });

    const utils_mod = b.addModule("utils", .{
        .root_source_file = b.path("./root.zig"),
        .target = target,
        .optimize = optimize,
    });

    utils_mod.addImport("zg:GenCatData", zg_dep.module("GenCatData"));
    utils_mod.addImport("zg:PropsData", zg_dep.module("PropsData"));
    utils_mod.addImport("zg:CaseData", zg_dep.module("CaseData"));
    utils_mod.addImport("zg:CaseFold", zg_dep.module("CaseFold"));
    utils_mod.addImport("zg:DisplayWidth", zg_dep.module("DisplayWidth"));
}
