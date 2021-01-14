const std = @import("std");
const mem = std.mem;

// TODO: wrap in node type with fields next and frag?
// think more about how pipelines work
pub const FragType = enum { literal, action, for_range, for_each, end };
pub const Frag = union(FragType) {
    literal: []const u8,
    action: []const u8,
    for_range: ForRange,
    for_each: ForEach,
    end,
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
            .action => try writer.print("'{{{{{s}}}}}'", .{value.action}),
            .for_range => {
                try writer.print("for({}..{}) |{s}|", .{ value.for_range.start, value.for_range.end, value.for_range.capture_name });
                // for (value.for_range.body) |child| try writer.print("  {}", .{child});
                try writer.print("\n  {}", .{value.for_range.body});
            },
            .for_each => {
                try writer.print("for({s}) |{s},{s}|", .{ value.for_each.slice_name, value.for_each.capture_name, value.for_each.capture_index_name });
                // for (value.for_each.body) |child| try writer.print("  {}", .{child});
                try writer.print("\n  {}", .{value.for_each.body});
            },
            .end => unreachable,
        }
    }
};

pub const Parser = struct {
    buf: []const u8,
    pos: usize = 0,
    marked_pos: usize = 0,

    /// Returns a substring of the input starting from the current position
    /// and ending where `ch` is found or until the end if not found
    pub fn until(self: *@This(), comptime ch: u8) []const u8 {
        const start = self.pos;

        if (start >= self.buf.len)
            return &[_]u8{};

        while (self.pos < self.buf.len) : (self.pos += 1) {
            if (self.buf[self.pos] == ch) break;
        }
        return self.buf[start..self.pos];
    }

    /// Returns a substring of the input starting from the current position
    /// and ending where one of `cs` is found or until the end if not found
    pub fn untilOneOf(self: *@This(), comptime cs: []const u8) []const u8 {
        const start = self.pos;

        if (start >= self.buf.len)
            return &[_]u8{};

        while (self.pos < self.buf.len) : (self.pos += 1) {
            if (mem.indexOfScalar(u8, cs, self.buf[self.pos])) |_| break;
        }
        return self.buf[start..self.pos];
    }

    /// Returns a substring of the input starting from the current position
    /// and ending where `str` is found or until the end if not found
    pub fn untilStr(self: *@This(), comptime str: []const u8) []const u8 {
        const start = self.pos;

        if (start + str.len >= self.buf.len)
            return &[_]u8{};

        while (self.pos < self.buf.len) : (self.pos += 1) {
            if (self.pos + str.len <= self.buf.len and
                memeql(self.buf[self.pos .. self.pos + str.len], str))
                break;
        }
        return self.buf[start..self.pos];
    }

    /// Returns the n-th next character or null if that's past the end
    pub fn peek(self: *@This(), comptime n: usize) ?u8 {
        return if (self.pos + n < self.buf.len) self.buf[self.pos + n] else null;
    }

    /// end of stream?
    pub fn eos(self: @This()) bool {
        return self.pos >= self.buf.len;
    }

    /// sets marked_pos to the current pos + n
    pub fn mark(self: *@This(), comptime n: usize) void {
        self.marked_pos = self.pos + n;
    }

    /// Returns a substring of the input starting from
    /// `marked_pos` until `pos`
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

fn parseForRange(comptime input: []const u8) Frag {
    std.debug.assert(mem.startsWith(u8, input, "for("));
    var result: Frag = .{ .for_range = undefined };
    var parser = Parser{ .buf = input[4..] };
    result.for_range.start = try std.fmt.parseInt(isize, trim(parser.untilStr("..")), 10);
    parser.pos += 2;
    result.for_range.end = try std.fmt.parseInt(isize, trim(parser.until(')')), 10);
    _ = parser.until('|');
    parser.pos += 1;
    result.for_range.capture_name = trim(parser.until('|'));
    return result;
}

fn parseForEach(comptime input: []const u8) Frag {
    std.debug.assert(mem.startsWith(u8, input, "for("));
    var parser = Parser{ .buf = input[4..] };
    var result: Frag = .{ .for_each = undefined };
    result.for_each.capture_index_name = null;
    result.for_each.slice_name = trim(parser.until(')'));
    _ = parser.until('|');
    parser.pos += 1;
    result.for_each.capture_name = trim(parser.untilOneOf(",|"));
    if (parser.peek(0)) |c| {
        if (c == ',') {
            parser.pos += 1;
            result.for_each.capture_index_name = trim(parser.until('|'));
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

inline fn parseFragments(comptime fmt: []const u8) []const Frag {
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
                var state: FragType = .action;
                var action = parser.fromMark();
                if (action.len != 0) {
                    action = trim(action);
                    // change state if this is a for_range or for_each
                    if (mem.startsWith(u8, action, "for")) {
                        state = blk: {
                            const dotsidx = mem.indexOf(u8, action, "..") orelse break :blk .for_each;
                            const rparidx = mem.indexOf(u8, action, ")") orelse return error.MissingRightParen;
                            break :blk if (dotsidx < rparidx) .for_range else .for_each;
                        };
                        fragments = fragments ++ if (state == .for_range)
                            [1]Frag{parseForRange(escape(action))}
                        else
                            [1]Frag{parseForEach(escape(action))};
                    } else if (memeql(action, "end"))
                        fragments = fragments ++ [1]Frag{.end}
                    else
                        fragments = fragments ++ [1]Frag{.{ .action = escape(action) }};
                }
                parser.mark(2);
                parser.pos += 1;
            },
            else => {},
        }
    }
    if (parser.marked_pos < parser.buf.len) {
        const lit = parser.buf[parser.marked_pos..];
        fragments = fragments ++ [1]Frag{.{ .literal = escape(lit) }};
    }
    return fragments;
}

/// recursively appends children of for_range and for_each loops to their bodies
/// until end action reached
const NestResult = struct { len: usize, children: []const Frag };
fn nestFragments(comptime flat_frags: []const Frag) NestResult {
    var result: []const Frag = &[0]Frag{};
    var i: usize = 0;
    while (i < flat_frags.len) : (i += 1) {
        var frag = flat_frags[i];
        // append children until end
        switch (frag) {
            .for_each,
            .for_range,
            => {
                i += 1;
                const nresult = nestFragments(flat_frags[i..]);
                var frag_tag = &@field(frag, @tagName(frag));
                frag_tag.body = nresult.children;
                if (nresult.len > 0)
                    i += nresult.len;
            },
            .end => break,
            else => {},
        }
        result = result ++ &[1]Frag{frag};
    }
    return .{ .len = i, .children = result };
}

fn visitTree(frags: []const Frag, ctx: anytype, cb: fn (Frag, @TypeOf(ctx)) void) void {
    for (frags) |frag| {
        switch (frag) {
            .for_each, .for_range => {
                cb(frag, ctx);
                visitTree(@field(frag, @tagName(frag)).body, ctx, cb);
            },
            else => cb(frag, ctx),
        }
    }
}

// pub const Duck = struct { value: anytype };
const Options = struct {
    eval_branch_quota: usize = 1050,
    name: ?[]const u8 = null,
};
pub fn Template(comptime fmt: []const u8, comptime options_: Options) type {
    @setEvalBranchQuota(options_.eval_branch_quota);
    var flat_frags = parseFragments(fmt);
    // flat_frags is a flat list of fragments here.
    // need to populate for_range.body and for_each.body with
    // nested fragments.
    const nest_result = nestFragments(flat_frags);
    var nested = nest_result.children;
    // append remaining non nested
    if (nest_result.len < flat_frags.len) {
        for (flat_frags[nest_result.len..]) |frag|
            nested = nested ++ [1]Frag{frag};
    }

    return struct {
        pub const options = options_;
        pub const fragments = nested;

        pub fn bufPrint(buf: []u8, args: anytype) ![]u8 {
            var fbs = std.io.fixedBufferStream(buf);
            comptime var scopes: []const []const ScopeEntry = &[_][]const ScopeEntry{};
            try bufPrintImpl(scopes, fragments, fbs.writer(), args);
            return fbs.getWritten();
        }

        pub const BufPrintError = error{DuplicateKey} || std.io.FixedBufferStream([]u8).WriteError;
        fn bufPrintImpl(comptime scopes: []const []const ScopeEntry, comptime frags: []const Frag, writer: anytype, args: anytype) BufPrintError!void {
            comptime var i: comptime_int = 0;
            inline while (i < frags.len) : (i += 1) {
                const frag = frags[i];
                switch (frag) {
                    .literal => _ = try writer.write(frag.literal),
                    .action => {
                        if (@hasField(@TypeOf(args), frag.action)) {
                            _ = try writer.write(@field(args, frag.action));
                        } else {
                            // search scopes
                            inline for (scopes) |scope| {
                                inline for (scope) |entry| {
                                    if (memeql(entry.key, frag.action)) {
                                        _ = try writer.print("{}", .{entry.value});
                                    }
                                }
                            }
                        }
                    },
                    .for_range => {
                        comptime var j = frag.for_range.start;
                        inline while (j < frag.for_range.end) : (j += 1) {
                            try bufPrintImpl(
                                try appendScope(scopes, frag.for_range.capture_name, j),
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
                                    try appendScope(try appendScope(scopes, frag.for_each.capture_name, item), idx_name, index),
                                    frag.for_each.body,
                                    writer,
                                    args,
                                )
                            else
                                try bufPrintImpl(
                                    try appendScope(scopes, frag.for_each.capture_name, item),
                                    frag.for_each.body,
                                    writer,
                                    args,
                                );
                        }
                    },
                    // .if_ => {
                    //     const condition = @field(args, frag.if_.condition);
                    //     if (!try empty(condition))
                    //         try bufPrintImpl(scopes, frag.if_.body, writer, args)
                    //     else if (i + 1 < frags.len and frags[i + 1] == .else_) {
                    //         try bufPrintImpl(scopes, frag.if_.body, writer, args);
                    //         i += 1;
                    //     }
                    // },
                    .end => unreachable,
                }
            }
        }

        fn empty(value: anytype) !bool {
            const V = @TypeOf(value);
            const vti = @typeInfo(V);
            return switch (vti) {
                .Pointer => switch (vti.Pointer.size) {
                    .One => empty(value.*),
                    .Slice => value.len == 0,
                    else => error.UnsupportedType,
                },
                .Optional => value == null,
                .Bool => value,
                .Array, .Vector => value.len == 0,
                else => if (@hasField(V, "len") or @hasDecl(V, "len"))
                    value.len == 0
                else
                    error.UnsupportedType,
            };
        }

        const ScopeEntry = struct { key: []const u8, value: anytype };

        pub fn allocPrint(allocator: *mem.Allocator, args: anytype) ![]u8 {
            var writer = std.io.countingWriter(std.io.null_writer);
            comptime var scopes: []const []const ScopeEntry = &[_][]const ScopeEntry{};
            try bufPrintImpl(scopes, fragments, writer.writer(), args);
            const buf = try allocator.alloc(u8, writer.bytes_written);
            return try bufPrint(buf, args);
        }

        fn appendScope(comptime scopes: []const []const ScopeEntry, comptime key: []const u8, value: anytype) ![]const []const ScopeEntry {
            for (scopes) |scope| for (scope) |entry| if (memeql(entry.key, key)) return error.DuplicateKey;
            return scopes ++ [_][]const ScopeEntry{&[1]ScopeEntry{.{ .key = key, .value = value }}};
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

inline fn memeql(a: []const u8, comptime b: []const u8) bool {
    // const Sh = StrHasher(b.len);
    // return (comptime Sh.case(b)) == Sh.match(a) catch return false;
    return mem.eql(u8, a, b);
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
    const fragments = comptime parseFragments(text);
    std.testing.expectEqual(fragments.len, 1);
    std.testing.expect(fragments[0] == .end);
}
