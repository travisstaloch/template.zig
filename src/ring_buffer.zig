const std = @import("std");

pub fn RingBuffer(comptime T: type) type {
    return struct {
        buf: []T,
        head: usize,
        tail: usize,
        len: usize,

        pub fn init(buf: []T) @This() {
            return .{ .buf = buf, .head = 0, .tail = 0, .len = 0 };
        }

        /// write to tail. if full, overwrite
        pub fn write(self: *@This(), e: T) void {
            // std.log.debug("write {} e {}", .{ self, e });
            self.buf[self.tail] = e;
            self.tail = (self.tail + 1) % self.buf.len;
            if (self.len == self.buf.len)
                self.head = (self.head + 1) % self.buf.len;
            self.len = std.math.min(self.len + 1, self.buf.len);
        }

        /// write to tail. if full, error.TooManyItems
        pub fn tryWrite(self: *@This(), e: T) !void {
            if (self.len == self.buf.len) return error.TooManyItems;
            self.write(e);
        }

        /// read from head. advance head, decrement len
        pub fn read(self: *@This()) ?T {
            // std.log.debug("read {}", .{self});
            if (self.len == 0) return null;
            const result = self.buf[self.head];
            self.len -= 1;
            self.head = (self.head + 1) % self.buf.len;
            return result;
        }
    };
}

test "basic" {
    const t = std.testing;
    std.testing.log_level = .debug;
    // follows example given here: https://en.wikipedia.org/wiki/Circular_buffer
    var buf = [_]u8{0} ** 7;
    var ring = RingBuffer(u8).init(&buf);
    ring.head = 2;
    ring.tail = 2;
    ring.write(1);
    ring.write(2);
    ring.write(3);
    t.expectEqual(ring.read(), 1);
    t.expectEqual(ring.read(), 2);
    t.expectEqual(ring.len, 1);
    ring.write(4);
    ring.write(5);
    ring.write(6);
    ring.write(7);
    ring.write(8);
    ring.write(9);
    t.expectEqual(ring.head, 4);
    t.expectEqual(ring.tail, 4);
    t.expectEqual(ring.len, 7);
    ring.write(10);
    ring.write(11);
    t.expectEqualStrings("\x06\x07\x08\x09\x0A\x0B\x05", ring.buf);
    t.expectEqual(ring.read(), 5);
    t.expectEqual(ring.read(), 6);
    t.expectEqual(ring.len, 5);
}
