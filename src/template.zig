const std = @import("std");
const mem = std.mem;

pub const FragType = enum { literal, variable, for_range, for_each };

pub const Frag = union(FragType) {
    literal: []const u8,
    variable: []const u8,
    for_range: ForRange,
    for_each: ForEach,
    pub const ForRange = struct {
        start: isize,
        end: isize,
        capture_name: []const u8,
        body: []const Frag,
    };
    pub const ForEach = struct {
        slice_name: []const u8,
        capture_name: []const u8,
        capture_index_name: ?[]const u8,
        body: []const Frag,
    };

    const FormatError = error{ DiskQuota, FileTooBig, InputOutput, NoSpaceLeft, AccessDenied, BrokenPipe, SystemResources, OperationAborted, NotOpenForWriting, WouldBlock, Unexpected };
    pub fn format(value: Frag, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) FormatError!void {
        switch (value) {
            .literal => try writer.print("'{s}'", .{value.literal}),
            .variable => try writer.print("{{{{{s}}}}}", .{value.variable}),
            .for_range => {
                try writer.print("for_range({}..{}) |{s}|", .{ value.for_range.start, value.for_range.end, value.for_range.capture_name });
                // for (value.for_range.body) |child| try format(child, fmt, options, writer);
                try writer.print("\n  {}", .{value.for_range.body});
            },
            .for_each => {
                try writer.print("for_each({}) |{s},{s}|", .{ value.for_each.slice_name, value.for_each.capture_name, value.for_each.capture_index_name });
                // for (value.for_each.body) |child| try format(child, fmt, options, writer);
                try writer.print("\n  {}", .{value.for_each.body});
            },
        }
    }
};

pub const Parser = struct {
    buf: []const u8,
    pos: usize = 0,
    marked_pos: usize = 0,

    // Returns a substring of the input starting from the current position
    // and ending where `ch` is found or until the end if not found
    pub fn until(self: *@This(), comptime ch: u8) []const u8 {
        const start = self.pos;

        if (start >= self.buf.len)
            return &[_]u8{};

        while (self.pos < self.buf.len) : (self.pos += 1) {
            if (self.buf[self.pos] == ch) break;
        }
        return self.buf[start..self.pos];
    }

    pub fn untilOneOf(self: *@This(), comptime cs: []const u8) []const u8 {
        const start = self.pos;

        if (start >= self.buf.len)
            return &[_]u8{};

        while (self.pos < self.buf.len) : (self.pos += 1) {
            if (mem.indexOfScalar(u8, cs, self.buf[self.pos])) |_| break;
        }
        return self.buf[start..self.pos];
    }

    pub fn untilStr(self: *@This(), comptime str: []const u8) []const u8 {
        const start = self.pos;

        if (start + str.len >= self.buf.len)
            return &[_]u8{};

        while (self.pos < self.buf.len) : (self.pos += 1) {
            if (self.pos + str.len <= self.buf.len and
                mem.eql(u8, self.buf[self.pos .. self.pos + str.len], str))
                break;
        }
        return self.buf[start..self.pos];
    }

    // Returns the n-th next character or null if that's past the end
    pub fn peek(self: *@This(), comptime n: usize) ?u8 {
        return if (self.pos + n < self.buf.len) self.buf[self.pos + n] else null;
    }

    pub fn eos(self: @This()) bool {
        return self.pos >= self.buf.len;
    }

    pub fn mark(self: *@This(), comptime n: usize) void {
        self.marked_pos = self.pos + n;
    }
    pub fn fromMark(self: @This()) []const u8 {
        return self.buf[self.marked_pos..self.pos];
    }
};

fn showError(comptime msg: []const u8, args: anytype) noreturn {
    @compileError(std.fmt.comptimePrint(msg, args));
}

inline fn escape(comptime input: []const u8) []u8 {
    var output = [1]u8{0} ** input.len;
    const n1 = mem.replace(u8, input, "\\{", "{", &output);
    const n2 = mem.replace(u8, &output, "\\}", "}", &output);
    return output[0 .. input.len - (n1 + n2)];
}

fn parseForRange(comptime input: []const u8) !Frag.ForRange {
    std.debug.assert(mem.startsWith(u8, input, "for("));
    var result: Frag.ForRange = undefined;
    var parser = Parser{ .buf = input[4..] };
    result.start = try std.fmt.parseInt(isize, trim(parser.untilStr("..")), 10);
    parser.pos += 2;
    result.end = try std.fmt.parseInt(isize, trim(parser.until(')')), 10);
    _ = parser.until('|');
    parser.pos += 1;
    result.capture_name = trim(parser.until('|'));
    return result;
}

fn parseForEach(comptime input: []const u8) !Frag.ForEach {
    std.debug.assert(mem.startsWith(u8, input, "for("));
    var parser = Parser{ .buf = input[4..] };
    var result: Frag.ForEach = undefined;
    result.capture_index_name = null;
    result.slice_name = trim(parser.until(')'));
    _ = parser.until('|');
    parser.pos += 1;
    result.capture_name = trim(parser.untilOneOf(",|"));
    if (parser.peek(0)) |c| {
        if (c == ',') {
            parser.pos += 1;
            result.capture_index_name = trim(parser.until('|'));
        }
    }
    return result;
}

/// trim trailing and leading whitespace
fn trim(s_: []const u8) []const u8 {
    var s = s_;
    while (mem.indexOfScalar(u8, &std.ascii.spaces, s[0])) |_|
        s = s[1..];
    while (mem.indexOfScalar(u8, &std.ascii.spaces, s[s.len - 1])) |_|
        s.len -= 1;
    return s;
}

inline fn tokenizeFragments(comptime fmt: []const u8) []const Frag {
    var fragments: []const Frag = &[0]Frag{};
    var parser = Parser{ .buf = fmt };

    while (!parser.eos()) : (parser.pos += 1) {
        const c = parser.peek(0) orelse break;
        switch (c) {
            '{' => {
                const next = parser.peek(1) orelse break;
                if (next != '{') continue;
                const lit = parser.fromMark();

                if (lit.len != 0)
                    fragments = fragments ++ [1]Frag{@unionInit(Frag, "literal", escape(lit))};
                parser.mark(2);
                parser.pos += 1;
            },
            '}' => {
                const next = parser.peek(1) orelse break;
                if (next != c) continue;
                var state: FragType = .variable;
                var variable = parser.fromMark();
                if (variable.len != 0) {
                    variable = trim(variable);
                    // change state if this is a for_range or for_each
                    if (mem.startsWith(u8, variable, "for")) {
                        state = blk: {
                            const dotsidx = mem.indexOf(u8, variable, "..") orelse break :blk .for_each;
                            const rparidx = mem.indexOf(u8, variable, ")") orelse return error.MissingRightParen;
                            break :blk if (dotsidx < rparidx) .for_range else .for_each;
                        };
                        fragments = fragments ++ if (state == .for_range)
                            [1]Frag{@unionInit(Frag, @tagName(state), try parseForRange(escape(variable)))}
                        else
                            [1]Frag{@unionInit(Frag, @tagName(state), try parseForEach(escape(variable)))};
                    } else
                        fragments = fragments ++ [1]Frag{@unionInit(Frag, @tagName(state), escape(variable))};
                }
                parser.mark(2);
                parser.pos += 1;
            },
            else => {},
        }
    }
    if (parser.marked_pos < parser.buf.len) {
        const lit = parser.buf[parser.marked_pos..];
        fragments = fragments ++ [1]Frag{@unionInit(Frag, "literal", escape(lit))};
    }
    return fragments;
}

/// recursively appends children of for_range and for_each loops to their bodies
/// until end variable reached
inline fn parseFragments(comptime flat_frags: []const Frag) []const Frag {
    comptime var result: []const Frag = &[0]Frag{};
    var i: comptime_int = 0;
    while (i < flat_frags.len) : (i += 1) {
        var frag = flat_frags[i];
        // append children until end
        if (frag == .for_range or frag == .for_each) {
            var frag_tag = &@field(frag, @tagName(frag));
            i += 1;
            frag_tag.body = parseFragments(flat_frags[i..]);
            i += frag_tag.body.len;
        }
        if (frag == .variable and mem.eql(u8, frag.variable, "end")) break;
        result = result ++ &[1]Frag{frag};
    }
    return result;
}

const Options = struct { eval_branch_quota: usize = 1000 };
pub fn Template(comptime fmt: []const u8, comptime options: Options) type {
    @setEvalBranchQuota(options.eval_branch_quota);
    var tokens = tokenizeFragments(fmt);

    return struct {
        // Currently fragments is a flat list of fragments
        // need to populate for_range.body and for_each.body with
        // nested fragments.
        // TODO: This only works for one level of depth.
        pub const fragments = parseFragments(tokens);

        pub fn bufPrint(buf: []u8, args: anytype) ![]u8 {
            var fbs = std.io.fixedBufferStream(buf);

            try bufPrintImpl(.{}, fragments, fbs.writer(), args);
            return fbs.getWritten();
        }

        pub const BufPrintError = error{ MissingVariable, BufferTooSmall } || std.io.FixedBufferStream([]u8).WriteError;
        pub fn bufPrintImpl(comptime scopes: anytype, comptime frags: []const Frag, writer: anytype, args: anytype) BufPrintError!void {
            inline for (frags) |frag, i| {
                switch (frag) {
                    .literal => _ = try writer.write(frag.literal),
                    .variable => {
                        if (@hasField(@TypeOf(args), frag.variable)) {
                            _ = try writer.write(@field(args, frag.variable));
                        } else inline for (scopes) |scope| { // search scopes
                            if (comptime mem.eql(u8, scope[0], frag.variable)) {
                                _ = try writer.print("{}", .{scope[1]});
                            }
                        }
                    },
                    .for_range => {
                        comptime var j = frag.for_range.start;
                        inline while (j < frag.for_range.end) : (j += 1) {
                            try bufPrintImpl(
                                scopes ++ .{.{ frag.for_range.capture_name, j }},
                                frag.for_range.body,
                                writer,
                                args,
                            );
                        }
                    },
                    .for_each => {
                        const slice = @field(args, frag.for_each.slice_name);
                        inline for (slice) |item, index| {
                            if (frag.for_each.capture_index_name) |idx_name|
                                try bufPrintImpl(
                                    scopes ++ .{ .{ frag.for_each.capture_name, item }, .{ idx_name, index } },
                                    frag.for_each.body,
                                    writer,
                                    args,
                                )
                            else
                                try bufPrintImpl(
                                    scopes ++ .{.{ frag.for_each.capture_name, item }},
                                    frag.for_each.body,
                                    writer,
                                    args,
                                );
                        }
                    },
                }
            }
        }

        pub fn allocPrint(allocator: *mem.Allocator, args: anytype) ![]u8 {
            var writer = std.io.countingWriter(std.io.null_writer);
            try bufPrintImpl(.{}, fragments, writer.writer(), args);
            const buf = try allocator.alloc(u8, writer.bytes_written);
            return try bufPrint(buf, args);
        }
    };
}

fn StrHasher(comptime min_bytes: usize) type {
    return struct {
        const byte_len = std.math.ceilPowerOfTwo(usize, min_bytes) catch |e| @compileError("invalid min_bytes: " ++ @errorName(e));
        const I = std.meta.Int(.unsigned, byte_len * 8);

        fn case(comptime src: []const u8) I {
            if (src.len > byte_len) @compileError("String too long");
            return comptime match(src) catch |e| @compileError("Error: " ++ @errorName(e));
        }
        fn match(src: []const u8) !I {
            if (src.len > byte_len) return error.StringTooLong;
            var dest = [1]u8{0} ** byte_len;
            std.mem.copy(u8, &dest, src);
            return std.mem.readIntNative(I, &dest);
        }
    };
}

test "main tests" {
    _ = @import("tests.zig");
}

test "string match" {
    const E = enum {
        a = 1,
        ab = 2,
        abc = 3,
        inv = 0,
        const Self = @This();
        pub fn fromStr(s: []const u8) !?Self {
            const strhasher = StrHasher(4);
            inline for (std.meta.fields(Self)) |f|
                if (strhasher.case(f.name) == try strhasher.match(s))
                    return @field(Self, f.name);
            return null;
        }
    };
    const Sh = StrHasher(4);
    for ([_][]const u8{ "a", "ab", "abc", "inv" }) |name| {
        const hash = try Sh.match(name);
        const e: E = switch (hash) {
            Sh.case("a") => .a,
            Sh.case("ab") => .ab,
            Sh.case("abc") => .abc,
            Sh.case("inv") => .inv,
            else => unreachable,
        };
        std.testing.expectEqual(try E.fromStr(name), e);
    }
}

test "end" {
    const text = "{{ end   }}";
    const fragments = comptime tokenizeFragments(text);
    std.testing.expectEqual(fragments.len, 1);
    std.testing.expect(fragments[0] == .variable);
    std.testing.expectEqualStrings("end", fragments[0].variable);
}
