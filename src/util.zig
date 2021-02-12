const std = @import("std");

pub const string = []const u8;
pub const rune = u21;
pub const int = usize;
pub fn Cx(comptime I: type) type {
    return extern struct { re: I, im: I };
}
//std.math.complex(f64);
pub fn cx(r: f64, i: f64) Cx(f64) {
    return Cx(f64){ .re = r, .im = i };
}

pub const RuneAndLen = struct {
    rune: u21,
    len: u3,
    pub fn init(r: u21, len: u3) @This() {
        return .{
            .rune = r,
            .len = len,
        };
    }
};

// DecodeRuneInString is like DecodeRune but its input is a string. if_ s is
// empty it returns (RuneError, 0). Otherwise, if the encoding is invalid, it
// returns (RuneError, 1). Both are impossible results for correct, non-empty
// UTF-8.
//
// An encoding is invalid if it is incorrect UTF-8, encodes a rune that is
// out of range, or is not the shortest possible UTF-8 encoding for the
// value. No other validation is performed.
pub fn utf8DecodeRuneInString(s: []const u8) RuneAndLen {
    const n = s.len;
    // std.log.debug("utf8DecodeRuneInString n {}", .{n});
    if (n < 1) {
        return RuneAndLen.init(utf8.RuneError, 0);
    }
    const s0 = s[0];
    const x = utf8.first[s0];
    if (x >= utf8.as) {
        // The following code simulates an additional check for x == xx and
        // handling the ASCII and invalid cases accordingly. This mask-and-or
        // approach prevents an additional branch.
        const mask = @truncate(rune, @as(u32, x) << 31 >> 31); // Create 0x0000 or 0xFFFF.
        return RuneAndLen.init(@as(rune, s[0]) & ~mask | (utf8.RuneError & mask), 1);
    }
    // std.debug.print("utf8DecodeRuneInString s0 {c} x {c}", .{ s0, x });
    const sz = @as(int, x & 7);
    const accept = utf8.acceptRanges[x >> 4];
    if (n < sz) {
        return RuneAndLen.init(utf8.RuneError, 1);
    }
    const s1 = s[1];
    if (s1 < accept.lo or accept.hi < s1) {
        return RuneAndLen.init(utf8.RuneError, 1);
    }
    if (sz <= 2) { // <= instead of == to help the compiler eliminate some bounds checks
        return RuneAndLen.init(@as(rune, s0 & utf8.mask2) << 6 | @as(rune, s1 & utf8.maskx), 2);
    }
    const s2 = s[2];
    if (s2 < utf8.locb or utf8.hicb < s2) {
        return RuneAndLen.init(utf8.RuneError, 1);
    }
    if (sz <= 3) {
        return RuneAndLen.init(@as(rune, s0 & utf8.mask3) << 12 | @as(rune, s1 & utf8.maskx) << 6 | @as(rune, s2 & utf8.maskx), 3);
    }
    const s3 = s[3];
    if (s3 < utf8.locb or utf8.hicb < s3) {
        return RuneAndLen.init(utf8.RuneError, 1);
    }
    return RuneAndLen.init(@as(rune, s0 & utf8.mask4) << 18 | @as(rune, s1 & utf8.maskx) << 12 | @as(rune, s2 & utf8.maskx) << 6 | @as(rune, s3 & utf8.maskx), 4);
}

// test "utf8DecodeRuneInString" {
//     const s: []const u8 = "}}";
//     const n = s.len;
//     // std.log.debug("utf8DecodeRuneInString n {}", .{n});
//     if (n < 1) {
//         return RuneAndLen.init(utf8.RuneError, 0);
//     }
//     const s0 = s[0];
//     const x = utf8.first[s0];
//     if (x >= utf8.as) {
//         // The following code simulates an additional check for x == xx and
//         // handling the ASCII and invalid cases accordingly. This mask-and-or
//         // approach prevents an additional branch.
//         const mask = @truncate(rune, @as(u32, x) << 31 >> 31); // Create 0x0000 or 0xFFFF.
//         const y = @as(rune, s[0]) & ~mask | (utf8.RuneError & mask);
//         std.debug.print("mask {x} y {x} {d} @as(rune, s[0]) & ~mask {d}\n", .{ mask, y, y, (@as(rune, s[0]) & ~mask) });
//         const x2: u8 = 1;
//         const yy: u8 = 1 << 2;
//         std.debug.print("{b:0>8}\n", .{x2 & ~yy});
//     }
// }

const UnqResult = struct {
    value: rune,
    multibyte: bool,
    tail: string,
    err: ?anyerror,
    pub fn init(value: rune, multibyte: bool, tail: string, err: ?anyerror) @This() {
        return .{
            .value = value,
            .multibyte = multibyte,
            .tail = tail,
            .err = err,
        };
    }
};

const utf8 = struct {
    pub const RuneError = '\u{FFFD}'; // the "error" Rune or "Unicode replacement character"
    pub const RuneSelf = 0x80; // characters below RuneSelf are represented as themselves in a single byte.
    pub const MaxRune = '\u{0010FFFF}'; // Maximum valid Unicode code point.
    pub const UTFMax = 4; // maximum number of bytes of a UTF-8 encoded Unicode character.
    pub const xx = 0xF1; // invalid: size 1
    pub const as = 0xF0; // ASCII: size 1
    pub const s1 = 0x02; // accept 0, size 2
    pub const s2 = 0x13; // accept 1, size 3
    pub const s3 = 0x03; // accept 0, size 3
    pub const s4 = 0x23; // accept 2, size 3
    pub const s5 = 0x34; // accept 3, size 4
    pub const s6 = 0x04; // accept 0, size 4
    pub const s7 = 0x44; // accept 4, size 4

    pub const first = [256]u8{
        //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
        as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, // 0x00-0x0F
        as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, // 0x10-0x1F
        as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, // 0x20-0x2F
        as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, // 0x30-0x3F
        as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, // 0x40-0x4F
        as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, // 0x50-0x5F
        as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, // 0x60-0x6F
        as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, as, // 0x70-0x7F
        //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
        xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, // 0x80-0x8F
        xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, // 0x90-0x9F
        xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, // 0xA0-0xAF
        xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, // 0xB0-0xBF
        xx, xx, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, // 0xC0-0xCF
        s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, s1, // 0xD0-0xDF
        s2, s3, s3, s3, s3, s3, s3, s3, s3, s3, s3, s3, s3, s4, s3, s3, // 0xE0-0xEF
        s5, s6, s6, s6, s7, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, // 0xF0-0xFF
    };

    pub const acceptRange = struct {
        lo: u8, // lowest value for second byte.
        hi: u8, // highest value for second byte.
    };

    // acceptRanges has size 16 to avoid bounds checks in the code that uses it.
    const acceptRanges = blk: {
        var result = [1]acceptRange{.{ .lo = 0, .hi = 0 }} ** 16;
        result[0] = .{ .lo = locb, .hi = hicb };
        result[1] = .{ .lo = 0xA0, .hi = hicb };
        result[2] = .{ .lo = locb, .hi = 0x9F };
        result[3] = .{ .lo = 0x90, .hi = hicb };
        result[4] = .{ .lo = locb, .hi = 0x8F };
        break :blk result;
    };

    pub const maskx = 0b00111111;
    pub const mask2 = 0b00011111;
    pub const mask3 = 0b00001111;
    pub const mask4 = 0b00000111;

    pub const rune1Max = 1 << 7 - 1;
    pub const rune2Max = 1 << 11 - 1;
    pub const rune3Max = 1 << 16 - 1;

    // The default lowest and highest continuation byte.
    pub const locb = 0b10000000;
    pub const hicb = 0b10111111;
};
pub fn UnquoteChar(s_: string, quote: u8) !UnqResult {
    // easy cases
    var s = s_;
    var result: UnqResult = undefined;

    if (s.len == 0) {
        result.err = error.Syntax;
        return result;
    }
    var c = s[0];

    if (c == quote and (quote == '\'' or quote == '"')) {
        result.err = error.Syntax;
        return result;
    } else if (c >= utf8.RuneSelf) {
        const rune_and_len = utf8DecodeRuneInString(s);
        return UnqResult.init(rune_and_len.rune, true, s, null);
    } else if (c != '\\') {
        s = s[1..];
        return UnqResult.init(s[0], false, s, null);
    }

    // hard case: c is backslash
    if (s.len <= 1) {
        result.err = error.Syntax;
        return result;
    }
    c = s[1];
    s = s[2..];

    switch (c) {
        'a' => result.value = 7, // '\a',
        'b' => result.value = 8, // '\b',
        'f' => result.value = 12, // '\f',
        'n' => result.value = 10, // '\n',
        'r' => result.value = 13, // '\r',
        't' => result.value = 9, // '\t',
        'v' => result.value = 11, // '\v',
        'x', 'u', 'U' => {
            const n: u8 = switch (c) {
                'x' => 2,
                'u' => 4,
                'U' => 8,
                else => unreachable,
            };
            var v: rune = 0;
            if (s.len < n) {
                result.err = error.Syntax;
                return result;
            }
            var j: u8 = 0;
            while (j < n) : (j += 1) {
                // x, ok := unhex(s[j])
                const x = std.fmt.charToDigit(s[j], 16) catch
                    {
                    result.err = error.Syntax;
                    return result;
                };

                v = v << 4 | x;
            }
            s = s[n..];
            if (c == 'x') {
                // single-byte string, possibly not UTF-8
                result.value = v;
                result.tail = s;
                return result;
            }
            if (v > utf8.MaxRune) {
                result.err = error.Syntax;
                return result;
            }
            result.value = v;
            result.multibyte = true;
        },
        '0', '1', '2', '3', '4', '5', '6', '7' => {
            var v = c - '0';
            if (s.len < 2) {
                result.err = error.Syntax;
                return result;
            }

            var j: u8 = 0;
            while (j < 2) : (j += 1) { // one digit already; two more
                const x = s[j] - '0';
                if (x < 0 or x > 7) {
                    result.err = error.Syntax;
                    return result;
                }

                v = (v << 3) | x;
            }
            s = s[2..];
            if (v > 255) {
                result.err = error.Syntax;
                return result;
            }

            result.value = v;
        },
        '\\' => result.value = '\\',
        '\'', '"' => {
            if (c != quote) {
                result.err = error.Syntax;
                return result;
            }

            result.value = c;
        },
        else => {
            result.err = error.Syntax;
            return result;
        },
    }
    result.tail = s;
    return result;
}

pub fn scanCx(comptime I: type, text: []const u8) !Cx(f64) {
    // TODO: a real parser
    const trimmed = std.mem.trim(u8, text, &std.ascii.spaces);
    if (trimmed.len == 0) return error.Empty;
    var result: Cx(I) = .{ .re = 0, .im = 0 };
    if (trimmed[trimmed.len - 1] == 'i') {
        // var it = std.mem.tokenize(trimmed, "+-")
        var i = trimmed.len - 1;
        while (true) : (i -= 1) {
            if (std.mem.indexOfScalar(u8, "+-", trimmed[i]) != null)
                result.re = try std.fmt.parseFloat(I, trimmed[0..i]);
            result.im = try std.fmt.parseFloat(I, trimmed[i..]);
            if (i == 0) break;
        }
    } else {
        result.re = try std.fmt.parseFloat(I, trimmed);
    }
    return result;
}
