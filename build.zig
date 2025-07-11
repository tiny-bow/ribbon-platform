const std = @import("std");

pub const Fingerprint = @import("./src/common/Fingerprint.zig");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const version_string = b.option([]const u8, "version", "version string to embed as a fingerprint in the platform build") orelse {
        @panic("Missing version string option in ribbon-platform package");
    };

    const name_string = b.option([]const u8, "name", "package name to embed as a fingerprint in the platform build") orelse {
        @panic("Missing name string option in ribbon-platform package");
    };

    var version = std.SemanticVersion.parse(version_string) catch {
        std.debug.panic("Cannot parse `{s}` as a semantic version string", .{version_string});
    };

    if (version.build != null) {
        std.debug.panic("Version string `{s}` is not valid; build metadata is not allowed in the version field, this is generated by the build system", .{version_string});
    }

    const fingerprint = Fingerprint.init(name_string);

    const build_info_opts = b.addOptions();
    // build_info_opts.addOptionPath() -- cool!
    if (optimize != .Debug) {
        version.build = std.fmt.allocPrint(b.allocator, "{}", .{fingerprint}) catch @panic("OOM");
        build_info_opts.addOption(u256, "raw_fingerprint", fingerprint.value);
    } else {
        version.build = "debug";
        build_info_opts.addOption(u256, "raw_fingerprint", 0);
    }
    build_info_opts.addOption(std.SemanticVersion, "version", version);

    const rg_dep = b.dependency("rg", .{
        .target = target,
        .optimize = optimize,
    });

    const utils_mod = b.addModule("utils", .{
        .root_source_file = b.path("./src/utils.zig"),
        .target = target,
        .optimize = optimize,
    });

    utils_mod.addImport("rg:GenCatData", rg_dep.module("GenCatData"));
    utils_mod.addImport("rg:PropsData", rg_dep.module("PropsData"));
    utils_mod.addImport("rg:CaseData", rg_dep.module("CaseData"));
    utils_mod.addImport("rg:CaseFold", rg_dep.module("CaseFold"));
    utils_mod.addImport("rg:DisplayWidth", rg_dep.module("DisplayWidth"));

    const platform_mod = b.addModule("platform", .{
        .root_source_file = b.path("src/platform.zig"),
        .target = target,
        .optimize = optimize,
    });

    const common_mod = b.addModule("common", .{
        .root_source_file = b.path("src/common.zig"),
        .target = target,
        .optimize = optimize,
    });

    const Buffer_mod = b.addModule("Buffer", .{
        .root_source_file = b.path("src/common/Buffer.zig"),
        .target = target,
        .optimize = optimize,
    });

    const Fingerprint_mod = b.addModule("Fingerprint", .{
        .root_source_file = b.path("src/common/Fingerprint.zig"),
        .target = target,
        .optimize = optimize,
    });

    const Formatter_mod = b.addModule("Formatter", .{
        .root_source_file = b.path("src/common/Formatter.zig"),
        .target = target,
        .optimize = optimize,
    });

    const Id_mod = b.addModule("Id", .{
        .root_source_file = b.path("src/common/Id.zig"),
        .target = target,
        .optimize = optimize,
    });

    const Interner_mod = b.addModule("Interner", .{
        .root_source_file = b.path("src/common/Interner.zig"),
        .target = target,
        .optimize = optimize,
    });

    const Stack_mod = b.addModule("Stack", .{
        .root_source_file = b.path("src/common/Stack.zig"),
        .target = target,
        .optimize = optimize,
    });

    const SlotMap_mod = b.addModule("SlotMap", .{
        .root_source_file = b.path("src/common/SlotMap.zig"),
        .target = target,
        .optimize = optimize,
    });

    const AllocWriter_mod = b.addModule("AllocWriter", .{
        .root_source_file = b.path("src/common/AllocWriter.zig"),
        .target = target,
        .optimize = optimize,
    });

    const VirtualWriter_mod = b.addModule("VirtualWriter", .{
        .root_source_file = b.path("src/common/VirtualWriter.zig"),
        .target = target,
        .optimize = optimize,
    });

    Buffer_mod.addImport("platform", platform_mod);

    common_mod.addImport("Buffer", Buffer_mod);
    common_mod.addImport("Fingerprint", Fingerprint_mod);
    common_mod.addImport("Formatter", Formatter_mod);
    common_mod.addImport("Id", Id_mod);
    common_mod.addImport("Interner", Interner_mod);
    common_mod.addImport("Stack", Stack_mod);
    common_mod.addImport("AllocWriter", AllocWriter_mod);
    common_mod.addImport("VirtualWriter", VirtualWriter_mod);
    common_mod.addImport("SlotMap", SlotMap_mod);

    Formatter_mod.addImport("platform", platform_mod);

    Id_mod.addImport("platform", platform_mod);

    Interner_mod.addImport("platform", platform_mod);

    platform_mod.addImport("Fingerprint", Fingerprint_mod);
    platform_mod.addOptions("build_info", build_info_opts);

    Stack_mod.addImport("platform", platform_mod);
    SlotMap_mod.addImport("platform", platform_mod);

    AllocWriter_mod.addImport("platform", platform_mod);
    VirtualWriter_mod.addImport("platform", platform_mod);
}
