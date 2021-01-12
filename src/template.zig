const std = @import("std");

pub const Token = struct {
    pub const dir_start = '{';
    pub const dir_end = '}';
    pub const esc = '\\';
};

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
};

fn showError(comptime msg: []const u8, args: anytype) noreturn {
    @compileError(std.fmt.comptimePrint(msg, args));
}

inline fn escape(comptime input: []const u8, output: [:0]u8, i: usize, start_idx: usize) []u8 {
    const n1 = std.mem.replace(u8, input, "\\{", "{", output);
    const n2 = std.mem.replace(u8, output, "\\}", "}", output);
    const newlen = i - start_idx - (n1 + n2);
    return std.mem.span(output[0..newlen]);
}

fn prevTokEql(c: u8, fmt: []const u8, i: usize) bool {
    return i != 0 and fmt[i - 1] == c;
}

fn parseForRange(comptime input_: []const u8) !Frag.ForRange {
    var input = input_;
    std.debug.assert(std.mem.startsWith(u8, input, "for("));
    input = input[4..];
    var state: enum { start, dots, rparen, lbar } = .start;
    var result: Frag.ForRange = undefined;
    var start_idx: usize = 0;
    for (input) |c, i| {
        switch (state) {
            .start => if (c == '.' and i != 0 and input[i - 1] == '.') {
                result.start = try std.fmt.parseInt(isize, input[start_idx .. i - 1], 10);
                start_idx = i + 1;
                state = .dots;
            },
            .dots => if (c == ')') {
                result.end = try std.fmt.parseInt(isize, input[start_idx..i], 10);
                start_idx = i + 1;
                state = .rparen;
            },
            .rparen => if (c == '|') {
                start_idx = i + 1;
                state = .lbar;
            },
            .lbar => if (c == '|') {
                // TODO: for some reason, this ends up as the original input start "for(0"
                // if `result.capture_name = input[start_idx..i];`
                // figure out why this copy is fixes things
                const trimmed = std.mem.trim(u8, input[start_idx..i], " ");
                var buf: [trimmed.len]u8 = undefined;
                std.mem.copy(u8, &buf, trimmed);
                result.capture_name = &buf;
                break;
            },
        }
    }
    return result;
}

fn parseForEach(comptime input_: []const u8) !Frag.ForEach {
    var input = input_;
    std.debug.assert(std.mem.startsWith(u8, input, "for("));
    input = input[4..];
    var state: enum { start, rparen, lbar, comma } = .start;
    var result: Frag.ForEach = undefined;
    result.capture_index_name = null;
    var start_idx: usize = 0;
    for (input) |c, i| {
        switch (state) {
            .start => if (c == ')') {
                // TODO: for some reason, this ends up as the original input start "for(0"
                const trimmed = std.mem.trim(u8, input[start_idx..i], " ");
                var buf: [trimmed.len]u8 = undefined;
                std.mem.copy(u8, &buf, trimmed);
                result.slice_name = &buf;
                start_idx = i + 1;
                state = .rparen;
            },
            .rparen => if (c == '|') {
                start_idx = i + 1;
                state = .lbar;
            },
            .lbar => if (c == ',' or c == '|') {
                // TODO: for some reason, this ends up as the original input start "for(0"
                const trimmed = std.mem.trim(u8, input[start_idx..i], " ");
                var buf: [trimmed.len]u8 = undefined;
                std.mem.copy(u8, &buf, trimmed);
                result.capture_name = &buf;
                if (c == ',') {
                    start_idx = i + 1;
                    state = .comma;
                } else break;
            },
            .comma => if (c == '|') {
                // TODO: for some reason, this ends up as the original input start "for(0"
                const trimmed = std.mem.trim(u8, input[start_idx..i], " ");
                var buf: [trimmed.len]u8 = undefined;
                std.mem.copy(u8, &buf, trimmed);
                result.capture_index_name = &buf;
                break;
            },
        }
    }
    return result;
}

pub fn Template(comptime fmt: []const u8, comptime options: struct { eval_branch_quota: usize = 1000 }) type {
    @setEvalBranchQuota(options.eval_branch_quota);
    var build_fragments: []const Frag = &[0]Frag{};
    var state = FragType.literal;
    var newstate = state;
    var start_idx: usize = 0;
    var output: [fmt.len:0]u8 = [1:0]u8{0} ** fmt.len; // can't use undefined - leads to compiler error
    var output_idx: usize = 0;
    for (fmt) |c, i| {
        var opt_escaped: ?[]u8 = null;
        switch (c) {
            Token.dir_start => {
                if (state == .variable)
                    showError("Too many levels of '{c}' at position {}", .{ Token.dir_start, i });
                if (prevTokEql(Token.dir_start, fmt, i)) {
                    opt_escaped = escape(fmt[start_idx .. i - 1], output[output_idx..], i - 1, start_idx);
                    newstate = .variable;
                }
            },
            Token.dir_end => {
                if (prevTokEql(Token.esc, fmt, i)) continue;
                if (state == .literal and prevTokEql(Token.dir_end, fmt, i))
                    showError("Unmatched '{c}' at position {}", .{ Token.dir_end, i });
                if (prevTokEql(Token.dir_end, fmt, i)) {
                    const trimmed = std.mem.trim(u8, fmt[start_idx .. i - 1], " ");
                    opt_escaped = escape(trimmed, output[output_idx..], i - 1, start_idx);
                    if (std.mem.startsWith(u8, opt_escaped.?, "for"))
                        state = if (std.mem.indexOf(u8, opt_escaped.?, "..") != null) .for_range else .for_each;

                    newstate = .literal;
                }
            },
            else => {},
        }

        if (opt_escaped) |escaped| {
            if (escaped.len > 0) {
                const escaped0 = std.mem.spanZ(@ptrCast(*const [:0]u8, &escaped).*); // remove trailing zeros
                build_fragments = build_fragments ++ switch (state) {
                    .literal, .variable => &[1]Frag{@unionInit(Frag, @tagName(state), escaped0)},
                    .for_range => blk: {
                        const for_range = parseForRange(escaped0) catch |e| showError("couldn't parse for range: {s}", .{@errorName(e)});
                        break :blk &[1]Frag{@unionInit(Frag, @tagName(state), for_range)};
                    },
                    .for_each => blk: {
                        const for_each = parseForEach(escaped0) catch |e| showError("couldn't parse for each: {s}", .{@errorName(e)});
                        break :blk &[1]Frag{@unionInit(Frag, @tagName(state), for_each)};
                    },
                };
                output_idx += escaped0.len;
            }
            state = newstate;
            start_idx = i + 1;
        }
    }
    if (start_idx < fmt.len) {
        const escaped = escape(fmt[start_idx..], output[output_idx..], fmt.len, start_idx);
        build_fragments = build_fragments ++ &[1]Frag{.{ .literal = escaped }};
    }

    return struct {
        pub const fragments = blk: {
            var result: []const Frag = &[0]Frag{};
            var i: usize = 0;
            while (i < build_fragments.len) : (i += 1) {
                var frag = build_fragments[i];
                if (frag == .for_range or frag == .for_each) {
                    var frag_tag = &@field(frag, @tagName(frag));
                    frag_tag.body = &[0]Frag{};
                    i += 1;
                    while (!(build_fragments[i] == .variable and std.mem.eql(u8, build_fragments[i].variable, "end"))) : (i += 1) {
                        frag_tag.body = frag_tag.body ++ &[1]Frag{build_fragments[i]};
                    }
                }
                result = result ++ &[1]Frag{frag};
            }
            break :blk result;
        };

        // const Error = error{BufferTooSmall};
        // inline fn bufPrintFrag(comptime frag: Frag, buf: []u8, args: anytype) Error!usize {
        //     comptime {
        //         var bufi: usize = 0;
        //         if (frag == .for_range) {
        //             var j = frag.for_range.start;
        //             while (j < frag.for_range.end) : (j += 1) {
        //                 for (frag.for_range.body) |body_frag| {
        //                     // if (bufi + target.len > buf.len) return error.BufferTooSmall;
        //                     // std.mem.copy(u8, buf[bufi..], target);
        //                     // bufi += target.len;
        //                     // bufPrint(buf[bufi..])
        //                     if (body_frag == .variable and std.mem.eql(u8, body_frag.variable, frag.for_range.capture_name)) {
        //                         const T = comptime @Type(.{
        //                             // std.builtin.TypeInfo
        //                             .Struct = .{
        //                                 .layout = .Auto,
        //                                 .fields = &[_]std.builtin.TypeInfo.StructField{.{
        //                                     .name = body_frag.variable,
        //                                     .field_type = isize,
        //                                     .default_value = null,
        //                                     .is_comptime = false,
        //                                     .alignment = @alignOf(isize),
        //                                 }},
        //                             },
        //                         });
        //                         const t: T = undefined;
        //                         @field(t, body_frag.variable) = j;
        //                         bufi += try bufPrintFrag(body_frag, buf[bufi..], t); //catch |e| showError("Error: bufPrintFrag {}", .{@errorName(e)});
        //                     } else
        //                         bufi += try bufPrintFrag(body_frag, buf[bufi..], args); //catch |e| showError("Error: bufPrintFrag {}", .{@errorName(e)});
        //                 }
        //             }
        //         } else {
        //             const target = switch (frag) {
        //                 .literal => frag.literal,
        //                 .variable => @field(args, frag.variable),
        //                 else => unreachable,
        //             };
        //             if (target.len > buf.len) return error.BufferTooSmall;
        //             std.mem.copy(u8, buf, target);
        //             bufi = target.len;
        //         }
        //         return bufi;
        //     }
        // }

        pub fn bufPrint(buf: []u8, args: anytype) ![]u8 {
            var bufi: usize = 0;
            inline for (fragments) |frag, i| {
                const target = switch (frag) {
                    .literal => frag.literal,
                    .variable => @field(args, frag.variable),
                    .for_range => {
                        var j = frag.for_range.start;
                        while (j < frag.for_range.end) : (j += 1) {
                            for (frag.for_range.body) |body_frag| {
                                if (body_frag == .variable and
                                    std.mem.eql(u8, body_frag.variable, frag.for_range.capture_name))
                                {
                                    bufi += std.fmt.formatIntBuf(buf[bufi..], j, 10, false, .{});
                                } else {
                                    const bodytarget = switch (body_frag) {
                                        .literal => body_frag.literal,
                                        .variable => blk: {
                                            inline for (std.meta.fields(@TypeOf(args))) |f| {
                                                if (std.mem.eql(u8, f.name, body_frag.variable))
                                                    break :blk @field(args, f.name);
                                            }
                                            unreachable;
                                        },
                                        else => unreachable,
                                    };
                                    if (bodytarget.len > buf.len) return error.BufferTooSmall;
                                    std.mem.copy(u8, buf[bufi..], bodytarget);
                                    bufi += bodytarget.len;
                                }
                            }
                        }
                        continue;
                    },
                    .for_each => {
                        const slice = @field(args, frag.for_each.slice_name);
                        for (slice) |item, index| {
                            for (frag.for_each.body) |body_frag| {
                                if (body_frag == .variable and frag.for_each.capture_index_name != null and
                                    std.mem.eql(u8, body_frag.variable, frag.for_each.capture_index_name.?))
                                {
                                    bufi += std.fmt.formatIntBuf(buf[bufi..], index, 10, false, .{});
                                } else if (body_frag == .variable and
                                    std.mem.eql(u8, body_frag.variable, frag.for_each.capture_name))
                                {
                                    const result = std.fmt.bufPrint(buf[bufi..], "{}", .{item}) catch unreachable;
                                    bufi += result.len;
                                } else {
                                    const bodytarget = switch (body_frag) {
                                        .literal => body_frag.literal,
                                        .variable => blk: {
                                            inline for (std.meta.fields(@TypeOf(args))) |f| {
                                                if (std.mem.eql(u8, f.name, body_frag.variable))
                                                    break :blk @field(args, f.name);
                                            }
                                            unreachable;
                                        },
                                        else => unreachable,
                                    };
                                    if (bodytarget.len > buf.len) return error.BufferTooSmall;
                                    std.mem.copy(u8, buf[bufi..], bodytarget);
                                    bufi += bodytarget.len;
                                }
                            }
                        }
                        continue;
                    },
                };
                if (target.len > buf.len) return error.BufferTooSmall;
                std.mem.copy(u8, buf[bufi..], target);
                bufi += target.len;
            }
            return buf[0..bufi];
        }

        pub fn allocPrint(allocator: *std.mem.Allocator, args: anytype) ![]u8 {
            const result = try allocator.alloc(u8, countSize(fragments, args));
            return try bufPrint(result, args);
        }

        fn countSize(comptime frags: []const Frag, args: anytype) usize {
            var size: usize = 0;
            inline for (frags) |frag, i| {
                switch (frag) {
                    .literal => size += frag.literal.len,
                    .variable => size += @field(args, frag.variable).len,
                    .for_range => @panic("todo"),
                    // {
                    //     var j = frag.for_range.start;
                    //     while (j < frag.for_range.end) : (j += 1)
                    //         size += countSize(frag.for_range.body, args);
                    // },
                    .for_each => @panic("todo"),
                }
            }
            return size;
        }
    };
}
