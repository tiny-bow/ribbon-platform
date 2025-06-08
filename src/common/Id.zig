//! Generalized unique identifier utilities.
const Id = @This();

const std = @import("std");

const pl = @import("platform");

test {
    std.testing.refAllDeclsRecursive(@This());
}

/// The maximum integer that can be converted to an `Id`.
pub const MAX_INT = std.math.maxInt(u16) - 1;

/// The default Id type constructor, 16 bits.
/// This is used because we can pack 16 bits into a pointer on most architectures, among other things.
pub fn of(comptime T: type) type {
    return ofSize(T, 16);
}

/// Identity values used to create indirect references.
///
/// Associated type may be accessed with the `Value` constant.
///
/// Null ids are encoded as part of the enum for layout efficiency; the methods account for this by
/// incrementing integers when creating enums, and decrementing enums when creating integers.
/// Therefore, despite the null id being zero, we can still index into arrays simply by calling `.toInt()`.
/// Additionally, "null dereference" will be caught by safe-mode checks due to overflow.
pub fn ofSize(comptime T: type, comptime bits: comptime_int) type {
    const I = std.meta.Int(.unsigned, bits);
    return enum(I) {
        const Self = @This();

        /// The value type this ID binds.
        pub const Value = T;

        null = 0,
        _,

        /// Convert between `Id` types.
        pub fn cast(self: Self, comptime U: type) Id.ofSize(U, bits) {
            return @enumFromInt(@intFromEnum(self));
        }

        /// Convert this id to an integer.
        pub fn toInt(self: Self) I {
            return @intFromEnum(self) - 1;
        }

        /// Convert an integer to an `Id`.
        pub fn fromInt(i: anytype) Self {
            return @enumFromInt(i + 1);
        }

        /// Takes a copy of an `Id`, performs a mutating increment on itself, and returns the copy.
        pub fn next(self: *Self) Self {
            const old = self.*;
            self.* = @enumFromInt(@intFromEnum(old) + 1);
            return old;
        }

        /// Convert an `Id` to an optional of itself.
        /// This is useful for `orelse` clauses on the null status of the id.
        pub fn maybe(self: Self) ?Self {
            if (self == .null) {
                return null;
            }
            return self;
        }

        pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            if (self == .null) {
                try writer.writeAll(@typeName(T) ++ ":null");
            } else {
                try writer.print(@typeName(T) ++ ":{d}", .{self.toInt()});
            }
        }
    };
}


pub fn Table(comptime T: type, comptime id_size: comptime_int) type {
    return struct {
        const Self = @This();
        pub const Id = ofSize(T, id_size);

        data: pl.MultiArrayList(T) = .empty,

        pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) !Self {
            var self = Self.empty;

            try self.data.ensureTotalCapacity(allocator, capacity);
            errdefer self.data.deinit(allocator);

            return self;
        }

        pub fn ensureCapacity(self: *Self, allocator: std.mem.Allocator, capacity: usize) !void {
            try self.data.ensureUnusedCapacity(allocator, capacity);
        }

        pub fn deinitData(self: *Self, allocator: std.mem.Allocator) void {
            if (comptime pl.hasDecl(T, .deinit)) {
                for (0..self.data.len) |i| {
                    var a = self.data.get(i);
                    a.deinit(allocator);
                }
            }

            self.data.len = 0;
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.deinitData(allocator);
            self.data.deinit(allocator);
        }

        pub fn clear(self: *Self) void {
            self.data.clearRetainingCapacity();
        }

        pub fn rowCount(self: *Self) usize {
            return self.data.len;
        }

        pub fn column(self: *Self, comptime name: std.meta.FieldEnum(T)) []std.meta.FieldType(T, name) {
            return self.data.items(name);
        }

        pub fn cell(self: *Self, id: Self.Id, comptime name: std.meta.FieldEnum(T)) *std.meta.FieldType(T, name) {
            return &self.data.items(name)[id.toInt()];
        }

        pub fn getRow(self: *Self, id: Self.Id) ?T {
            if (id == .null) return null;

            return self.data.get(id.toInt());
        }

        pub fn addRow(self: *Self, allocator: std.mem.Allocator, data: T) !Self.Id {
            std.debug.assert(self.data.len <= std.math.maxInt(std.meta.Tag(Self.Id)));

            const id = Self.Id.fromInt(self.data.len);

            try self.data.append(allocator, data);

            return id;
        }
    };
}

/// This is a slice utilizing pointer-tagging to encode the length
/// into 64 bits along with the base pointer.
///
/// This is possible because of two factors:
/// + `Id`s are 16-bit, so a buffer referring to values identified by them
/// could never be larger than `std.math.maxInt(u16)` elements.
/// + Pointer values on our supported architectures only use 48-bits of their word,
/// leaving us a super convenient space to store arbitrary data
/// (assuming we decompose before using it as a pointer, see methods).
pub fn Buffer(comptime T: type, comptime MUT: pl.Mutability) type {
    // NOTE: the original description of this type said "assuming we mask it off"
    // but actually, some platforms require you to sign-extend the 47th bit to 64 bits.
    // The code remains unchanged, though, because zig handles this for us in @ptrFromInt.
    return packed struct {
        const Self = @This();

        len: u16 = 0,
        ptr: u48 = 0,

        pub const PointerType: type = MUT.MultiPointerType(T);
        pub const SliceType: type = MUT.SliceType(T);
        pub const ValueType = T;

        pub const empty = Self{
            .len = 0,
            .ptr = 0,
        };

        /// Create a buffer from a slice.
        pub fn fromSlice(slice: SliceType) Self {
            return fromPtr(slice.ptr, slice.len);
        }

        /// Create a buffer from a pointer and length.
        pub fn fromPtr(ptr: PointerType, len: usize) Self {
            const i = @intFromPtr(ptr);
            std.debug.assert(i != 0);
            std.debug.assert(i != 0xaaaa_aaaa_aaaa_aaaa);
            return Self {
                .len = @intCast(len),
                .ptr = @intCast(i),
            };
        }

        /// Extract the 48-bit address part of this buffer.
        pub fn asPtr(self: Self) PointerType {
            std.debug.assert(self.ptr != 0);
            if (!std.mem.isAligned(self.ptr, @alignOf(T))) {
                std.debug.panic("Buffer.asPtr: pointer {x} not aligned to {}", .{ self.ptr, @alignOf(T) });
            }
            return @ptrFromInt(self.ptr);
        }

        /// Extract both parts of this buffer and construct a slice.
        pub fn asSlice(self: Self) SliceType {
            if (self.ptr == 0 or self.len == 0) return &.{};

            return self.asPtr()[0..self.len];
        }

        pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            if (comptime @typeInfo(PointerType).pointer.child == u8) {
                try writer.print("\"{s}\"", .{self.asSlice()});
            } else {
                try writer.print("{any}", .{self.asSlice()});
            }
        }
    };
}


/// `HashMap` specialized to types compatible with `IdHashCtx`.
///
/// Default initialization of this struct is deprecated; use `.empty` instead.
///
/// See `platform.HashMap` and `IdHashCtx` for detailed docs.
pub fn Map(comptime K: type, comptime V: type, comptime LOAD_PERCENTAGE: u64) type {
    return pl.HashMap(K, V, HashCtx(K), LOAD_PERCENTAGE);
}

/// `HashSet` specialized to types compatible with `IdHashCtx`.
///
/// Default initialization of this struct is deprecated; use `.empty` instead.
///
/// See `platform.HashSet` and `IdHashCtx` for detailed docs.
pub fn Set(comptime T: type, comptime LOAD_PERCENTAGE: u64) type {
    return pl.HashSet(T, HashCtx(T), LOAD_PERCENTAGE);
}

/// Creates a context for hashing and comparing values based on their ids.
///
/// The type provided must have a field `id`,
/// which must be of a type that has as a unique representation.
///
/// See `std.meta.hasUniqueRepresentation`.
pub fn HashCtx(comptime T: type) type {
    comptime {
        if(!pl.hasDerefField(T, .id)) {
            @compileError("IdHashCtx: type " ++ @typeName(T) ++ " requires a field named 'id'");
        }

        if (!std.meta.hasUniqueRepresentation(T)) {
            @compileError("IdHashCtx: field " ++ @typeName(T) ++ ".id does not have a unique representation");
        }

        return struct {
            pub fn eql(_: @This(), a: T, b: T) bool {
                return a.id == b.id;
            }

            pub fn hash(_: @This(), x: T) u64 {
                return pl.hash64(std.mem.asBytes(&x.id));
            }
        };
    }
}
