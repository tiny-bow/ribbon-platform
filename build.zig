const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const rg_dep = b.dependency("rg", .{
        .target = target,
        .optimize = optimize,
    });

    const utils_mod = b.addModule("utils", .{
        .root_source_file = b.path("./root.zig"),
        .target = target,
        .optimize = optimize,
    });

    utils_mod.addImport("rg:GenCatData", rg_dep.module("GenCatData"));
    utils_mod.addImport("rg:PropsData", rg_dep.module("PropsData"));
    utils_mod.addImport("rg:CaseData", rg_dep.module("CaseData"));
    utils_mod.addImport("rg:CaseFold", rg_dep.module("CaseFold"));
    utils_mod.addImport("rg:DisplayWidth", rg_dep.module("DisplayWidth"));
}
