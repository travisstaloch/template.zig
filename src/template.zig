const std = @import("std");
const mem = std.mem;

// TODO: wrap in node type with fields next and node?
// think more about how pipelines work

pub const NodeType = enum { text, action, range, end };
pub const Node = union(NodeType) {
    text: []const u8,
    action: []const u8,
    range: Branch,
    end,
    pub const Branch = struct {
        pipeline: ?Pipeline = null,
        list: ?List = null,
        else_list: ?List = null,
    };
    pub const List = struct {
        len: usize,
        root: []const Node,
        pub fn format(value: List, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) FormatError!void {
            try writer.print("{}", .{value.root});
        }
    };
    pub const Pipeline = struct {
        is_assign: bool, // The variables are being assigned, not declared.
        decls: []const []const u8, // Variables in lexical order.
        cmds: []const Command, // The commands in lexical order.
    };
    pub const CommandType = enum { identifier, field, constant, range };
    pub const Command = union(CommandType) {
        identifier: []const u8,
        field: []const u8,
        constant: []const u8,
        range: struct { start: usize, end: usize },
        // Arguments in lexical order: Identifier, field, or constant.
    };

    const FormatError = error{ DiskQuota, FileTooBig, InputOutput, NoSpaceLeft, AccessDenied, BrokenPipe, SystemResources, OperationAborted, NotOpenForWriting, WouldBlock, Unexpected };
    pub fn format(value: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) FormatError!void {
        switch (value) {
            .text => try writer.print("'{s}'", .{value.text}),
            .action => try writer.print("'{{{{{s}}}}}'", .{value.action}),
            .range => {
                try writer.print("range {}", .{value.range.pipeline});
                // for (value.range.body) |child| try writer.print("  {}", .{child});
                try writer.print("\n  {}", .{value.range.list});
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

fn todo(comptime msg: []const u8, args: anytype) noreturn {
    showError("todo " ++ msg, args);
}

// identifier | field | constant | range
fn parseCommand(comptime input: []const u8) Node.Command {
    if (std.mem.indexOf(u8, input, "..")) |dots_idx| {
        const start = std.fmt.parseUnsigned(usize, input[0..dots_idx], 10) catch |e| showError("invalid range start '{}'. {s}", .{ input[0..dots_idx], @errorName(e) });
        const end = std.fmt.parseUnsigned(usize, input[dots_idx + 2 ..], 10) catch |e| showError("invalid range end '{}'. {s}", .{ input[0..dots_idx], @errorName(e) });
        return .{ .range = .{ .start = start, .end = end } };
    }
    return switch (input[0]) {
        '.' => .{ .field = input[1..] },
        'a'...'z', 'A'...'Z' => .{ .identifier = input },
        '0'...'9', '-' => .{ .constant = std.fmt.parseInt(usize, input[0..dots_idx], 10) catch |e| showError("invalid constant value '{}'. {s}", .{ input[0..dots_idx], @errorName(e) }) },
        else => showError("parseCommand: unsupported command type {s}", .{input}),
    };
}

// Pipeline:
//	declarations? command ('|' command)*
fn parsePipeline(comptime input: []const u8) Node.Pipeline {
    var result: Node.Pipeline = .{ .is_assign = false, .cmds = &[_]Node.Command{}, .decls = &[_][]const u8{} };

    var parser = Parser{ .buf = trim(input) };
    if (std.mem.indexOfScalar(u8, input, '=')) |eqpos_| {
        // @compileLog("eqpos_", eqpos_);
        result.is_assign = true;
        const is_decl_assign = input[eqpos_ - 1] == ':'; // check for :=
        const eqpos = if (is_decl_assign) eqpos_ - 1 else eqpos_;
        while (parser.pos < eqpos_) {
            // declarations
            // @compileLog(parser.pos, eqpos_, parser.buf.len);
            var next = trim(parser.untilOneOf(",=:"));

            // @compileLog(next);
            if (next.len == 0) break;
            if (next[0] == '$') {
                result.decls = result.decls ++ &[1][]const u8{next};
            } else {
                todo("declarations: '{s}'", .{input});
            }
            parser.pos += 1;
        }
        parser.pos += 1;
    }
    // @compileLog(parser.pos);
    // commands
    while (true) {
        var cmd_text = trim(parser.until('|'));
        // @compileLog(cmd_text);
        if (cmd_text.len == 0) break;
        result.cmds = result.cmds ++ [1]Node.Command{parseCommand(cmd_text)};
    }
    return result;
}

// {{range pipeline}} T1 {{end}}
// {{range pipeline}} T1 {{else}} T0 {{end}}
fn parseRange(comptime input: []const u8) Node {
    std.debug.assert(mem.startsWith(u8, input, "range"));
    var result: Node = .{ .range = .{ .pipeline = parsePipeline(input[5..]) } };
    return result;
}

/// trim trailing and leading whitespace
fn trim(s_: []const u8) []const u8 {
    if (s_.len == 0) return s_;
    var s = s_;
    while (mem.indexOfScalar(u8, &std.ascii.spaces, s[0])) |_|
        s = s[1..];
    while (mem.indexOfScalar(u8, &std.ascii.spaces, s[s.len - 1])) |_|
        s.len -= 1;
    return s;
}

inline fn parseNodes(comptime fmt: []const u8) []const Node {
    var nodes: []const Node = &[0]Node{};
    var parser = Parser{ .buf = fmt };

    while (!parser.eos()) : (parser.pos += 1) {
        const c = parser.peek(0) orelse break;
        switch (c) {
            '{' => {
                const next = parser.peek(1) orelse break;
                if (next != '{') continue;
                const lit = parser.fromMark();

                if (lit.len != 0)
                    nodes = nodes ++ [1]Node{@unionInit(Node, "text", escape(lit))};
                parser.mark(2);
                parser.pos += 1;
            },
            '}' => {
                const next = parser.peek(1) orelse break;
                if (next != c) continue;
                var action = parser.fromMark();
                if (action.len != 0) {
                    action = trim(action);
                    // showError("{s}", .{action});
                    if (mem.startsWith(u8, action, "range")) {
                        nodes = nodes ++ [1]Node{parseRange(escape(action))};
                    } else if (memeql(action, "end"))
                        nodes = nodes ++ [1]Node{.end}
                    else
                        nodes = nodes ++ [1]Node{.{ .action = escape(action) }};
                }
                parser.mark(2);
                parser.pos += 1;
            },
            else => {},
        }
    }
    if (parser.marked_pos < parser.buf.len) {
        const lit = parser.buf[parser.marked_pos..];
        nodes = nodes ++ [1]Node{.{ .text = escape(lit) }};
    }
    return nodes;
}
/// recursively appends children of for_range and for_each loops to their bodies
/// until end action reached
fn parseTree(comptime flat_nodes: []const Node) Node.List {
    var result: []const Node = &[0]Node{};

    var i: usize = 0;
    while (i < flat_nodes.len) : (i += 1) {
        var node = flat_nodes[i];
        // append children until end
        switch (node) {
            .range => {
                i += 1;
                const list = parseTree(flat_nodes[i..]);
                node.range.list = list;
                if (list.len > 0)
                    i += list.len;
            },
            .end => break,
            else => {},
        }
        result = result ++ &[1]Node{node};
    }
    return .{ .len = i, .root = result };
}

// fn visitNodes(nodes: []const Node, ctx: anytype, cb: fn (Node, @TypeOf(ctx)) void) void {
//     for (nodes) |node| {
//         switch (node) {
//             .for_each, .for_range => {
//                 cb(node, ctx);
//                 visitTree(@field(node, @tagName(node)).body, ctx, cb);
//             },
//             else => cb(node, ctx),
//         }
//     }
// }

const Options = struct {
    eval_branch_quota: usize = 1000,
    name: ?[]const u8 = null,
};
pub fn Template(comptime fmt: []const u8, comptime options_: Options) type {
    @setEvalBranchQuota(options_.eval_branch_quota);
    var flat_nodes = parseNodes(fmt);
    // flat_nodes is a flat list of nodes here.
    // need to populate for_range.body and for_each.body with
    // nested nodes.
    var tree_ = parseTree(flat_nodes);
    var root = tree_.root;
    // append remaining non root
    if (tree_.len < flat_nodes.len) {
        for (flat_nodes[tree_.len..]) |node|
            root = root ++ [1]Node{node};
        tree_.len = flat_nodes.len;
    }
    tree_.root = root;

    return struct {
        pub const options = options_;
        pub const tree = tree_;

        pub fn bufPrint(buf: []u8, args: anytype) ![]u8 {
            var fbs = std.io.fixedBufferStream(buf);
            comptime var scopes: []const []const ScopeEntry = &[_][]const ScopeEntry{};
            try bufPrintImpl(scopes, tree.root, fbs.writer(), args);
            return fbs.getWritten();
        }

        pub const BufPrintError = error{DuplicateKey} || std.io.FixedBufferStream([]u8).WriteError;
        fn bufPrintImpl(comptime scopes: []const []const ScopeEntry, comptime nodes: []const Node, writer: anytype, args: anytype) BufPrintError!void {
            comptime var i: comptime_int = 0;
            inline while (i < nodes.len) : (i += 1) {
                const node = nodes[i];
                // std.debug.print(
                //     "bufPrintImpl node {}:{} i {} scopes.len {}\n",
                //     .{ std.meta.activeTag(node), node, i, scopes.len },
                // );
                switch (node) {
                    .text => _ = try writer.write(node.text),
                    .action => {
                        if (@hasField(@TypeOf(args), node.action)) {
                            _ = try writer.write(@field(args, node.action));
                        } else {
                            // search scopes
                            inline for (scopes) |scope| {
                                inline for (scope) |entry| {
                                    // std.debug.print("searching scopes for '{s}' action '{s}'\n", .{ entry.key, node.action });
                                    if (memeql(entry.key, node.action)) {
                                        // std.debug.print("found scope entry {s} {}\n", .{ entry.key, entry.value });
                                        _ = try writer.print("{}", .{entry.value});
                                    }
                                }
                            }
                        }
                    },
                    .range => {
                        if (node.range.pipeline) |pipeline| {
                            if (pipeline.is_assign and pipeline.decls.len == 2) {
                                std.debug.assert(pipeline.cmds.len == 1);
                                if (pipeline.cmds[0] == .range) {
                                    const idx_name = pipeline.decls[0];
                                    const item_name = pipeline.decls[1];
                                    comptime var item = pipeline.cmds[0].range.start;
                                    comptime var index: usize = 0;
                                    inline while (item <= pipeline.cmds[0].range.end) : ({
                                        item += 1;
                                        index += 1;
                                    }) {
                                        // std.debug.print("item {} index {}\n", .{ item, index });
                                        const new_scope_entries = [_]ScopeEntry{
                                            .{ .key = item_name, .value = item },
                                            .{ .key = idx_name, .value = index },
                                        };
                                        try bufPrintImpl(
                                            try appendScope(scopes, &new_scope_entries),
                                            node.range.list.?.root,
                                            writer,
                                            args,
                                        );
                                    }
                                } else if (pipeline.cmds[0] == .field) {
                                    const idx_name = pipeline.decls[0];
                                    const item_name = pipeline.decls[1];
                                    const items = @field(args, pipeline.cmds[0].field);
                                    inline for (items) |item, index| {
                                        const new_scope_entries = [_]ScopeEntry{
                                            .{ .key = item_name, .value = item },
                                            .{ .key = idx_name, .value = index },
                                        };
                                        try bufPrintImpl(
                                            try appendScope(scopes, &new_scope_entries),
                                            node.range.list.?.root,
                                            writer,
                                            args,
                                        );
                                    }
                                } else showError("unsupported range command", .{});
                            } else {
                                std.debug.assert(pipeline.cmds.len == 1);
                                if (pipeline.cmds[0] == .range) {
                                    const item_name = pipeline.decls[0];
                                    comptime var item = pipeline.cmds[0].range.start;
                                    inline while (item <= pipeline.cmds[0].range.end) : ({
                                        item += 1;
                                    }) {
                                        // std.debug.print("item {} index {}\n", .{ item, index });
                                        const new_scope_entries = [_]ScopeEntry{.{ .key = item_name, .value = item }};
                                        try bufPrintImpl(
                                            try appendScope(scopes, &new_scope_entries),
                                            node.range.list.?.root,
                                            writer,
                                            args,
                                        );
                                    }
                                } else if (pipeline.cmds[0] == .field) {
                                    const items = @field(args, pipeline.cmds[0].field);
                                    inline for (items) |item| {
                                        // std.debug.print("item {} index {}\n", .{ item, index });
                                        const item_name = pipeline.decls[0];
                                        const new_scope_entries = [_]ScopeEntry{.{ .key = item_name, .value = item }};
                                        try bufPrintImpl(
                                            try appendScope(scopes, &new_scope_entries),
                                            node.range.list.?.root,
                                            writer,
                                            args,
                                        );
                                    }
                                } else comptime showError("unsupported range command {}", .{pipeline.cmds[0]});
                            }
                        }
                    },
                    // .if_ => {
                    //     const condition = @field(args, node.if_.condition);
                    //     if (!try empty(condition))
                    //         try bufPrintImpl(scopes, node.if_.body, writer, args)
                    //     else if (i + 1 < nodes.len and nodes[i + 1] == .else_) {
                    //         try bufPrintImpl(scopes, node.if_.body, writer, args);
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
            try bufPrintImpl(scopes, tree.root, writer.writer(), args);
            const buf = try allocator.alloc(u8, writer.bytes_written);
            return try bufPrint(buf, args);
        }

        fn appendScope(comptime scopes: []const []const ScopeEntry, comptime entries: []const ScopeEntry) ![]const []const ScopeEntry {
            for (scopes) |scope| for (scope) |entry| for (entries) |newentry| if (memeql(entry.key, newentry.key)) return error.DuplicateKey;
            return scopes ++ [_][]const ScopeEntry{entries};
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
    const tree = comptime parseNodes(text);
    std.testing.expectEqual(tree.len, 1);
    std.testing.expect(tree[0] == .end);
}
