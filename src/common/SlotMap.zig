const SlotMap = @This();

const std = @import("std");
const log = std.log.scoped(.slot_map);


pub fn MultiArray(comptime T: type, comptime index_bits: comptime_int, comptime generation_bits: comptime_int) type {
    if (@typeInfo(T) != .@"struct") {
        @compileError("SlotMap.MultiArray requires a struct type; each field will be stored in a separate array."
            ++ "If you want to store a single " ++ @typeName(T) ++ ", use SlotMap.new instead.");
    }

    return struct {
        const I: type = std.meta.Int(.unsigned, index_bits);
        const S: type = I;
        const V: type = I;
        const Self = @This();

        pub const Ref = packed struct {
            index: V,
            generation: Generation,

            pub const invalid = @This() {
                .index = 0,
                .generation = .invalid,
            };
        };

        pub const Generation = enum(std.meta.Int(.unsigned, generation_bits)) {
            invalid = 0,
            _,
        };

        const Data = data: {
            const basics = struct {
                __slot_to_value: V,
                __value_to_slot: S,
                __freelist_next: ?S,
                __generation: Generation,
            };

            const basic_fields = std.meta.fieldNames(basics);
            const T_fields = std.meta.fieldNames(T);
            var new_fields = [1]std.builtin.Type.StructField {undefined} ** (T_fields.len + basic_fields.len);
            var i = 0;

            for (basic_fields) |basic_field_name| {
                const F = @FieldType(basics, basic_field_name);
                new_fields[i] = std.builtin.Type.StructField {
                    .name = basic_field_name,
                    .type = F,
                    .alignment = @alignOf(F),
                    .default_value_ptr = null,
                    .is_comptime = false,
                };
                i += 1;
            }

            for (T_fields) |field_name| {
                const F = @FieldType(T, field_name);
                new_fields[i] = std.builtin.Type.StructField {
                    .name = field_name,
                    .type = F,
                    .alignment = @alignOf(F),
                    .default_value_ptr = null,
                    .is_comptime = false,
                };
                i += 1;
            }

            break :data @Type(.{
                .@"struct" = std.builtin.Type.Struct {
                    .layout = .auto,
                    .fields = &new_fields,
                    .decls = &.{},
                    .backing_integer = null,
                    .is_tuple = false,
                },
            });
        };


        freelist_head: ?S,
        data: std.MultiArrayList(Data),

        pub const empty = Self {
            .freelist_head = null,
            .data = std.MultiArrayList(Data).empty,
        };

        pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) !Self {
            var self = Self.empty;

            try self.data.ensureTotalCapacity(allocator, capacity);

            return self;
        }

        pub fn ensureCapacity(self: *Self, allocator: std.mem.Allocator, capacity: usize) !void {
            log.debug(@typeName(Self) ++ " ensureCapacity: {} {} {?}", .{capacity, self.data.len, self.freelist_head});

            const x = self.freelist_head;
            try self.data.ensureUnusedCapacity(allocator, capacity);

            std.debug.assert((self.freelist_head == null) == (x == null));
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.data.deinit(allocator);
        }

        pub fn clear(self: *Self) void {
            self.freelist_head = null;
            self.data.clearRetainingCapacity();
        }

        pub fn __slotToValue(self: *const Self, slot_index: S) *V {
            return &self.data.items(.__slot_to_value)[slot_index];
        }

        pub fn __valueToSlot(self: *const Self, value_index: V) *S {
            return &self.data.items(.__value_to_slot)[value_index];
        }

        pub fn __freelistNext(self: *const Self, slot_index: S) *?S {
            return &self.data.items(.__freelist_next)[slot_index];
        }

        pub fn __generation(self: *const Self, value_index: V) *Generation {
            return &self.data.items(.__generation)[value_index];
        }

        fn popFreelist(self: *Self) ?S {
            if (self.freelist_head) |slot_index| {
                log.debug("wtf?, {}", .{slot_index});
                self.freelist_head = self.__freelistNext(slot_index).*;
                return slot_index;
            } else {
                return null;
            }
        }

        fn pushFreelist(self: *Self, slot_index: S) void {
            log.debug(@typeName(Self) ++ " pushFreelist: {} {?}", .{slot_index, self.freelist_head});
            self.__freelistNext(slot_index).* = self.freelist_head;
            self.freelist_head = slot_index;
        }

        fn incrementGeneration(self: *Self, value_index: V) void {
            const g = self.__generation(value_index);
            g.* = @enumFromInt(@intFromEnum(g.*) + 1);
        }

        fn addOne(self: *Self, allocator: std.mem.Allocator) !S {
            const slot_index: S = @intCast(try self.data.addOne(allocator));

            self.__slotToValue(slot_index).* = slot_index;
            self.__valueToSlot(slot_index).* = slot_index;
            self.__freelistNext(slot_index).* = null;
            self.__generation(slot_index).* = @enumFromInt(1);

            return slot_index;
        }

        pub fn count(self: *Self) usize {
            return self.data.len;
        }

        pub fn field(self: *Self, index: usize, comptime field_name: std.meta.FieldEnum(T)) *std.meta.FieldType(T, field_name) {
            return &self.fields(field_name)[index];
        }

        pub fn fields(self: *Self, comptime field_name: std.meta.FieldEnum(T)) []std.meta.FieldType(T, field_name) {
            return self.data.items(comptime @field(std.meta.FieldEnum(Data), @tagName(field_name)));
        }

        pub fn create(self: *Self, allocator: std.mem.Allocator) !struct { Ref, V } {
            const slot_index =
                if (self.popFreelist()) |free_slot| free_slot
                else try self.addOne(allocator);

            return .{
                Ref { .index = slot_index, .generation = @enumFromInt(1) },
                self.__slotToValue(slot_index).*,
            };
        }

        pub fn destroy(self: *Self, ref: Self.Ref) void {
            log.debug(@typeName(Self) ++ " destroy: {} {}", .{ref.index, ref.generation});

            const destroyed_slot_index = ref.index;
            const destroyed_value_index = self.__slotToValue(destroyed_slot_index).*;
            const generation_ptr = self.__generation(destroyed_value_index);

            if (generation_ptr.* != ref.generation) {
                log.warn("double free of slot index {} in SlotMap of type {s}", .{destroyed_slot_index, @typeName(T)});
                return;
            }

            self.incrementGeneration(destroyed_value_index);

            const last_value_slot_index = self.__valueToSlot(destroyed_value_index).*;

            if (last_value_slot_index != destroyed_slot_index) {
                const last_value_index = self.__slotToValue(last_value_slot_index).*;

                self.setIndex(destroyed_value_index, self.getIndex(last_value_index).?);

                self.__valueToSlot(last_value_index).* = destroyed_value_index;
                self.__slotToValue(destroyed_slot_index).* = last_value_index;

                self.__valueToSlot(destroyed_value_index).* = last_value_slot_index;
                self.__slotToValue(last_value_slot_index).* = destroyed_slot_index;

                self.pushFreelist(last_value_slot_index);
            } else {
                self.pushFreelist(destroyed_slot_index);
            }
        }

        pub fn get(self: *Self, ref: Ref) ?*T {
            const slot_index = ref.index;
            const value_index_ptr = self.__slotToValue(slot_index);
            const generation_ptr = self.__generation(value_index_ptr.*);

            if (generation_ptr.* != ref.generation) {
                log.warn("invalid access of slot index {} in SlotMap of type {s}", .{slot_index, @typeName(T)});
                return null;
            }

            return self.value(value_index_ptr.*);
        }

        pub fn getIndex(self: *Self, index: V) ?T {
            var out: T = undefined;

            inline for (comptime std.meta.fieldNames(T)) |field_name| {
                @field(out, field_name) = self.field(index, @field(std.meta.FieldEnum(T), field_name)).*;
            }

            return out;
        }

        pub fn setIndex(self: *Self, index: V, value: T) void {
            inline for (comptime std.meta.fieldNames(T)) |field_name| {
                self.field(index, @field(std.meta.FieldEnum(T), field_name)).* = @field(value, field_name);
            }
        }

        pub fn resolveIndex(self: *Self, ref: Ref) ?V {
            const slot_index = ref.index;
            const value_index_ptr = self.__slotToValue(slot_index);
            const generation_ptr = self.__generation(value_index_ptr.*);

            if (generation_ptr.* != ref.generation) {
                log.warn("invalid access of slot index {} in SlotMap of type {s}", .{slot_index, @typeName(T)});
                return null;
            }

            return value_index_ptr.*;
        }
    };
}
