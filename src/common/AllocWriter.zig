//! Byte writer for memory that requires addressing.
//!
//! Unlike VirtualWriter, this doesn't guarantee addresses are stable throughout the write,
//! instead it provides access to *relative* addresses that can be resolved afterward.
//!
//! Implements the writer interface.
//!
//! Allows access to the memory while it is being used as a writer. Use with care.
const AllocWriter = @This();

const pl = @import("platform");
const std = @import("std");

const log = std.log.scoped(.AllocWriter);

test {
    std.testing.refAllDecls(@This());
}

/// We must retain the allocator to implement the writer interface.
allocator: std.mem.Allocator,
/// The currently allocated memory region.
memory: []align(pl.PAGE_SIZE) u8 = &.{},
/// The current write offset.
cursor: u64 = 0,

/// Represents an error that can occur during a write to an `AllocWriter`.
///
/// The only error is `OutOfMemory`,
/// which is a generalization of several different kinds of syscall errors,
/// that all have the same end result for the purposes of writing to memory.
///
/// This is provided here for use by the zig std writer api.
pub const Error = error{
    /// The system ran out of memory, or otherwise did not allow page acquisition.
    OutOfMemory,
};

/// Can be used to represent a buffer with a relative address base pointer.
pub const RelativeBuffer = struct {
    len: u64,
    base: RelativeAddress,

    pub fn fromAddress(
        len: u64,
        base: RelativeAddress,
    ) RelativeBuffer {
        return RelativeBuffer{
            .len = len,
            .base = base,
        };
    }
};

/// Represents an address inside an AllocWriter to be resolved after write completion.
pub const RelativeAddress = enum(u64) {
    _,

    /// Determine if a relative address is the base address of its memory region.
    pub fn isBase(self: RelativeAddress) bool {
        return @intFromEnum(self) == 0;
    }

    /// Determine if a relative address is the end address (base + len) of its memory region.
    pub fn isSentinel(self: RelativeAddress, memory: []align(pl.PAGE_SIZE) u8) bool {
        const p = @intFromEnum(self);
        const a = @intFromPtr(memory.ptr);
        const b = a + memory.len;
        return p == b;
    }

    /// Validate that a relative address is within the bounds of a memory region.
    pub fn isValue(self: RelativeAddress, memory: []align(pl.PAGE_SIZE) u8) bool {
        const p = @intFromEnum(self);
        const a = @intFromPtr(memory.ptr);
        const b = a + memory.len;
        return p >= a and p < b;
    }

    /// Validate that a relative address is within the bounds of a memory region, or is the sentinel address.
    pub fn isValueOrSentinel(self: RelativeAddress, memory: []align(pl.PAGE_SIZE) u8) bool {
        const p = @intFromEnum(self);
        const a = @intFromPtr(memory.ptr);
        const b = a + memory.len;
        return (p >= a and p < b) or p == b;
    }

    /// Resolve a relative address to an absolute pointer given a base memory region.
    pub fn toPtr(self: RelativeAddress, base: []align(pl.PAGE_SIZE) u8) [*]u8 {
        // Use zig safe mode bounds check instead of offseting base.ptr
        return @ptrCast(&base[@intFromEnum(self)]);
    }

    /// Resolve a relative address to an absolute pointer given a base memory region.
    /// * This cannot perform type checking, it is a low-level operation.
    /// * Runtime align check is performed in safe modes.
    pub fn toTypedPtr(self: RelativeAddress, comptime T: type, base: []align(pl.PAGE_SIZE) u8) T {
        // Use zig safe mode bounds check instead of offseting base.ptr
        return @alignCast(@ptrCast(&base[@intFromEnum(self)]));
    }

    /// Resolve a relative address to an absolute pointer given a base memory region.
    /// * Unlike `toPtr`, this allows sentinel pointer resolution.
    pub fn toPtrOrSentinel(self: RelativeAddress, base: []align(pl.PAGE_SIZE) u8) [*]u8 {
        if (self.isSentinel(base)) {
            return base.ptr + base.len;
        } else {
            return self.toPtr(base);
        }
    }

    /// Try to resolve a relative address to an absolute pointer given a base memory region.
    pub fn tryToPtr(self: RelativeAddress, base: []align(pl.PAGE_SIZE) u8) ?[*]u8 {
        if (self.isValue(base)) {
            return self.toPtr(base);
        } else {
            return null;
        }
    }

    /// Try to resolve a relative address to an absolute pointer given a base memory region.
    /// * Unlike `tryToPtr`, this allows sentinel pointer resolution.
    pub fn tryToPtrOrSentinel(self: RelativeAddress, base: []align(pl.PAGE_SIZE) u8) ?[*]u8 {
        if (self.isValueOrSentinel(base)) {
            return self.toPtrOrSentinel(base);
        } else {
            return null;
        }
    }

    /// Apply a byte offset to a relative address, yielding a new one.
    pub fn applyOffset(self: RelativeAddress, offset: i64) RelativeAddress {
        return @enumFromInt(@as(u64, @bitCast(@as(i64, @bitCast(@as(u64, @intFromEnum(self)))) + offset)));
    }
};

/// Create an allocator writer, but do not create the memory region for it.
pub fn init(allocator: std.mem.Allocator) AllocWriter {
    return AllocWriter{
        .allocator = allocator,
    };
}

/// Create an allocating writer and acquire the memory region for it.
pub fn initCapacity(allocator: std.mem.Allocator) error{OutOfMemory}!AllocWriter {
    return AllocWriter{
        .allocator = allocator,
        .memory = try allocator.alignedAlloc(u8, pl.PAGE_SIZE, pl.PAGE_SIZE),
    };
}

/// Clears the writer, retaining any allocated memory it owns.
pub fn clear(self: *AllocWriter) void {
    self.cursor = 0;
}

/// De-initializes the writer, freeing any allocated memory it still owns.
pub fn deinit(self: *AllocWriter) void {
    if (self.memory.len != 0) {
        self.allocator.free(self.memory);
        self.cursor = 0;
        self.memory = &.{};
    }
}

/// Get the current length of the written region of the writer.
pub fn getWrittenSize(self: *AllocWriter) u64 {
    return self.cursor;
}

/// Returns the size of the region of allocated of memory that is unused.
pub fn getAvailableCapacity(self: *AllocWriter) u64 {
    return self.memory.len - self.cursor;
}

/// Get the current written region of the writer.
pub fn getWrittenRegion(self: *AllocWriter) []align(pl.PAGE_SIZE) u8 {
    return self.memory[0..self.cursor];
}

/// Returns the region of memory that is allocated but has not been written to.
pub fn getAvailableRegion(self: *AllocWriter) []u8 {
    return self.memory[self.cursor..self.memory.len];
}

/// Returns the current address of the writer's cursor.
/// * This address is not guaranteed to be stable throughout the write; use `getRelativeAddress` for stable references.
pub fn getCurrentAddress(self: *AllocWriter) [*]u8 {
    return self.memory.ptr + self.cursor;
}

/// Returns the current cursor position. See also `getCurrentAddress`.
/// * This can be used to get an address into the final buffer after write completion.
pub fn getRelativeAddress(self: *AllocWriter) RelativeAddress {
    return @enumFromInt(self.cursor);
}

/// Convert an unstable address in the writer, such as those acquired through `alloc`, into a stable relative address.
pub fn addressToRelative(self: *AllocWriter, address: anytype) AllocWriter.RelativeAddress {
    const p = @intFromPtr(address);
    const a = @intFromPtr(self.memory.ptr);
    const b = a + self.memory.len;

    std.debug.assert(p >= a and p <= b);

    return @enumFromInt(p - a);
}

/// Convert a stable relative address in the writer into an unstable absolute address.
pub fn relativeToAddress(self: *AllocWriter, relative: RelativeAddress) [*]u8 {
    return relative.toPtr(self.memory);
}

/// Convert a stable relative address in the writer into an unstable, typed pointer.
pub fn relativeToPointer(self: *AllocWriter, comptime T: type, relative: RelativeAddress) T {
    return relative.toTypedPtr(self.memory);
}

/// Finalizes the writer, returning the generated code as a byte slice,
/// and marking the memory with the provided access.
///
/// After calling this function, the writer will be in its default-initialized state.
/// In other words, it is safe but not necessary to call `deinit` on it.
/// It does not need to be re-initializd before reuse, as the allocator is retained.
pub fn finalize(self: *AllocWriter) error{ BadEncoding, OutOfMemory }![]align(pl.PAGE_SIZE) u8 {
    var out = self.memory[0..self.cursor];

    if (out.len == 0) {
        @branchHint(.cold);
        return error.BadEncoding;
    }

    // shrink the memory to the used size
    if (self.cursor != self.memory.len) {
        if (self.allocator.realloc(self.memory, self.cursor)) |new_memory| {
            out = new_memory;
        } else |err| {
            log.debug("AllocWriter: Allocator refused to reallocate ({}), trying alloc + memcpy", .{err});

            const new_memory = try self.allocator.alignedAlloc(u8, pl.PAGE_SIZE, self.cursor);

            @memcpy(new_memory[0..self.cursor], self.memory[0..self.cursor]);

            self.allocator.free(self.memory);

            out = new_memory;
        }
    }

    self.cursor = 0;
    self.memory = &.{};

    return out;
}

/// Reallocate the writer's memory as necessary to support the given capacity.
pub fn ensureCapacity(self: *AllocWriter, cap: u64) Error!void {
    var new_cap = if (self.memory.len != 0) self.memory.len else pl.PAGE_SIZE;

    while (new_cap < cap) new_cap *= 2;

    if (new_cap != self.memory.len) {
        const new_memory = try self.allocator.alignedAlloc(u8, pl.PAGE_SIZE, new_cap);

        @memcpy(new_memory[0..self.memory.len], self.memory);

        self.allocator.free(self.memory);

        self.memory = new_memory;
    }
}

/// Reallocate the writer's memory as necessary to support the given additional capacity.
pub fn ensureAdditionalCapacity(self: *AllocWriter, additional: u64) Error!void {
    const required = self.cursor + additional;

    if (required > self.memory.len) {
        try self.ensureCapacity(required);
    }
}

/// Allocates an aligned byte buffer from the address space of the writer.
pub fn alignedAlloc(self: *AllocWriter, alignment: pl.Alignment, len: usize) Error![]u8 {
    const padding = pl.alignDelta(self.getCurrentAddress(), alignment);
    const total_size = len + padding;

    try self.ensureAdditionalCapacity(total_size);

    const ptr = self.getCurrentAddress() + padding;

    self.cursor += total_size;

    return ptr[0..len];
}

/// Same as `std.mem.Allocator.alloc`, but allocates from the address space of the writer.
pub fn alloc(self: *AllocWriter, comptime T: type, len: usize) Error![]T {
    return std.mem.bytesAsSlice(T, self.alignedAlloc(@alignOf(T), len * @sizeOf(T)));
}

/// Same as `alloc`, but returns a RelativeAddress instead of a pointer.
pub fn allocRel(self: *AllocWriter, comptime T: type, len: usize) Error!AllocWriter.RelativeAddress {
    const ptr = try self.alloc(T, len);
    return self.addressToRelative(ptr);
}

/// Same as `std.mem.Allocator.dupe`, but copies a slice into the address space of the writer.
pub fn dupe(self: *AllocWriter, comptime T: type, slice: []const T) Error![]T {
    const dest = try self.alloc(T, slice.len);
    @memcpy(dest, slice);
    return dest;
}

/// Same as `dupe`, but returns a RelativeAddress instead of a pointer.
pub fn dupeRel(self: *AllocWriter, comptime T: type, slice: []const T) Error!AllocWriter.RelativeAddress {
    const ptr = try self.dupe(T, slice);
    return self.addressToRelative(ptr);
}

/// Same as `std.mem.Allocator.create`, but allocates from the address space of the writer.
pub fn create(self: *AllocWriter, comptime T: type) Error!*T {
    return &(try self.alloc(T, 1))[0];
}

/// Same as `create`, but returns a RelativeAddress instead of a pointer.
pub fn createRel(self: *AllocWriter, comptime T: type) Error!AllocWriter.RelativeAddress {
    const ptr = try self.create(T);
    return self.addressToRelative(ptr);
}

/// Same as `create`, but takes an initializer.
pub fn clone(self: *AllocWriter, value: anytype) Error!*@TypeOf(value) {
    const ptr = try self.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

/// Same as `create`, but returns a RelativeAddress instead of a pointer.
pub fn cloneRel(self: *AllocWriter, value: anytype) Error!AllocWriter.RelativeAddress {
    const ptr = try self.clone(value);
    return self.addressToRelative(ptr);
}

/// Writes as much of a slice of bytes to the writer as will fit without an allocation.
/// Returns the number of bytes written.
pub fn write(self: *AllocWriter, noalias bytes: []const u8) Error!usize {
    const avail = self.getAvailableRegion();

    const len = @min(bytes.len, avail.len);

    @memcpy(avail[0..len], bytes[0..len]);

    return len;
}

/// Writes all bytes from a slice to the writer.
pub fn writeAll(self: *AllocWriter, bytes: []const u8) Error!void {
    try self.ensureAdditionalCapacity(bytes.len);
    const written = try self.write(bytes);
    std.debug.assert(written == bytes.len);
}

/// Writes a single byte to the writer.
pub fn writeByte(self: *AllocWriter, byte: u8) Error!void {
    try self.writeAll(&.{byte});
}

/// Writes a byte to the writer `n` times.
pub fn writeByteNTimes(self: *AllocWriter, byte: u8, n: usize) Error!void {
    for (0..n) |_| try self.writeByte(byte);
}

/// Writes a slice of bytes to the writer `n` times.
pub fn writeBytesNTimes(self: *AllocWriter, bytes: []const u8, n: usize) Error!void {
    for (0..n) |_| try self.writeAll(bytes);
}

/// Writes an integer to the writer.
pub fn writeInt(
    self: *AllocWriter,
    comptime T: type,
    value: T,
    comptime _: enum { little }, // allows backward compat with writer api; but only in provably compatible use-cases
) Error!void {
    // We do not encode abi padding bytes here; only get the bytes that are actually used.
    const bytes = std.mem.asBytes(&value)[0..pl.bytesFromBits(@bitSizeOf(T))];
    try self.writeAll(bytes);
}
