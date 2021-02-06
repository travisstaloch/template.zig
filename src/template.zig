const std = @import("std");
const mem = std.mem;

pub const NodeType = enum {
    text, action, range, if_, else_, end
    //, comment, template, block, with
};
pub const Node = union(NodeType) {
    text: []const u8,
    /// something bounded by delimiters
    /// action represents simple field evaluations and parenthesized pipelines.
    action: Pipeline,
    range: Branch,
    if_: Branch,
    else_,
    end,
    pub const Branch = struct {
        pipeline: ?Pipeline = null,
        list: ?List = null,
        else_list: ?List = null,
    };
    pub const List = struct {
        len: usize,
        root: []const Node,
        pub fn format(value: List, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{}", .{value.root});
        }
    };
    pub const Pipeline = struct {
        is_assign: bool, // The variables are being assigned, not declared.
        decls: []const []const u8, // Variables in lexical order.
        cmds: []const Command, // The commands in lexical order.
    };
    pub const CommandType = enum { variable, func, field, constant, range };
    pub const Command = union(CommandType) {
        variable: []const u8, // TODO: support chained variables ($id.field1.field2...)
        func: []const []const u8, // TODO: support chained funcs (func1.field1.field2...)
        field: []const u8, // TODO: support chained fields (.field1.field2...)
        constant: []const u8, // TODO: add different constant types
        range: struct { start: usize, end: usize }, // TODO: rename to avoid naming conflict with Node.range
        // Arguments in lexical order: Identifier, field, or constant.
    };

    // const FormatError = error{ DiskQuota, FileTooBig, InputOutput, NoSpaceLeft, AccessDenied, BrokenPipe, SystemResources, OperationAborted, NotOpenForWriting, WouldBlock, Unexpected };
    pub fn format(value: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (value) {
            .text => try writer.print("'{s}'", .{value.text}),
            .action => {
                try writer.print("'{{{{{}}}}}'", .{value.action});
            },
            .range => {
                try writer.print("range {}", .{value.range.pipeline});
                // for (value.range.body) |child| try writer.print("  {}", .{child});
                try writer.print("\n  {}", .{value.range.list});
            },
            .if_ => {
                try writer.print("if {}", .{value.if_.pipeline});
                try writer.print("\n  {}", .{value.if_.list});
            },
            .end, .else_ => unreachable,
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
    pub fn peek(self: *@This(), comptime n: comptime_int) ?u8 {
        const signed_pos = @as(isize, self.pos);
        return if (signed_pos + n < self.buf.len) self.buf[signed_pos + n] else null;
    }

    /// end of stream
    pub fn eos(self: @This()) bool {
        return self.pos >= self.buf.len;
    }

    /// sets `marked_pos` to `current pos + n`
    pub fn mark(self: *@This(), comptime n: usize) void {
        self.marked_pos = self.pos + n;
    }

    /// Returns a substring of the input starting from
    /// `marked_pos` until `pos`
    pub fn fromMark(self: @This()) []const u8 {
        return self.buf[self.marked_pos..self.pos];
    }
};

inline fn escape(comptime input: []const u8) []u8 {
    var output = [1]u8{0} ** input.len;
    const n1 = mem.replace(u8, input, "\\{", "{", &output);
    const n2 = mem.replace(u8, &output, "\\}", "}", &output);
    return output[0 .. input.len - (n1 + n2)];
}

/// trim trailing and leading whitespace
fn trimSpaces(comptime input: []const u8) []const u8 {
    return @call(.{ .modifier = .always_inline }, trim, .{ input, &std.ascii.spaces });
}

/// trim leading whitespace
fn trimSpacesLeft(comptime input: []const u8) []const u8 {
    return @call(.{ .modifier = .always_inline }, trimLeft, .{ input, &std.ascii.spaces });
}

/// trim trailing whitespace
fn trimSpacesRight(comptime input: []const u8) []const u8 {
    return @call(.{ .modifier = .always_inline }, trimRight, .{ input, &std.ascii.spaces });
}

fn trim(comptime input: []const u8, comptime trim_chars: []const u8) []const u8 {
    if (input.len == 0) return input;
    var start: usize = 0;
    while (mem.indexOfScalar(u8, trim_chars, input[start])) |_|
        start += 1;
    var end: usize = input.len;
    while (mem.indexOfScalar(u8, trim_chars, input[end - 1])) |_|
        end -= 1;
    return input[start..end];
}

fn trimLeft(comptime input: []const u8, comptime trim_chars: []const u8) []const u8 {
    if (input.len == 0) return input;
    var start: usize = 0;
    while (mem.indexOfScalar(u8, trim_chars, input[start])) |_|
        start += 1;
    return input[start..];
}

fn trimRight(comptime input: []const u8, comptime trim_chars: []const u8) []const u8 {
    if (input.len == 0) return input;
    var end: usize = input.len;
    while (mem.indexOfScalar(u8, trim_chars, input[end - 1])) |_|
        end -= 1;
    return input[0..end];
}

fn showError(comptime msg: []const u8, args: anytype) noreturn {
    @compileError(std.fmt.comptimePrint(msg, args));
}

fn todo(comptime msg: []const u8, args: anytype) noreturn {
    comptime showError("TODO: " ++ msg, args);
}

fn parseIdentifier(comptime input: []const u8) []const []const u8 {
    var result: []const []const u8 = &[0][]const u8{};
    var it = std.mem.split(input, ".");
    while (it.next()) |part| result = result ++ [1][]const u8{trimSpaces(part)};
    return result;
}
// func | variable | field | constant | range
fn parseCommand(comptime input: []const u8) Node.Command {
    if (std.mem.indexOf(u8, input, "..")) |dots_idx| {
        const start = std.fmt.parseUnsigned(usize, input[0..dots_idx], 10) catch |e| showError("invalid range start '{}'. {s}", .{ input[0..dots_idx], @errorName(e) });
        const end = std.fmt.parseUnsigned(usize, input[dots_idx + 2 ..], 10) catch |e| showError("invalid range end '{}'. {s}", .{ input[0..dots_idx], @errorName(e) });
        return .{ .range = .{ .start = start, .end = end } };
    }
    return switch (input[0]) {
        '.' => .{ .field = input[1..] },
        'a'...'z', 'A'...'Z' => .{ .func = parseIdentifier(input) },
        '$' => .{ .variable = input },
        '0'...'9', '-' => .{ .constant = input }, // TODO: add different constant types

        else => todo("parseCommand: unsupported command type '{s}'", .{input}),
    };
}

// Pipeline:
//	declarations? command ('|' command)*
fn parsePipeline(comptime input: []const u8) Node.Pipeline {
    var result: Node.Pipeline = .{ .is_assign = false, .cmds = &[_]Node.Command{}, .decls = &[_][]const u8{} };
    var parser = Parser{ .buf = trimSpaces(input) };
    if (std.mem.indexOfScalar(u8, input, '=')) |eqpos_| {
        // @compileLog("eqpos_", eqpos_);
        result.is_assign = true;
        const is_decl_assign = input[eqpos_ - 1] == ':'; // check for :=
        const eqpos = if (is_decl_assign) eqpos_ - 1 else eqpos_;
        while (parser.pos < eqpos_) {
            // declarations
            // @compileLog(parser.pos, eqpos_, parser.buf.len);
            var next = trimSpaces(parser.untilOneOf(",=:"));

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
    // commands
    // @compileLog(parser.pos);
    while (true) {
        // @compileLog(parser.pos);
        var cmd_text = trimSpaces(parser.until('|'));
        // @compileLog(parser.pos);
        // @compileError(cmd_text);
        if (cmd_text.len == 0) break;
        result.cmds = result.cmds ++ [1]Node.Command{parseCommand(cmd_text)};
        parser.pos += 1; // skip '|'
    }
    return result;
}

// {{range pipeline}} T1 {{end}}
// {{range pipeline}} T1 {{else}} T0 {{end}}
fn parseRange(comptime input: []const u8) Node {
    std.debug.assert(mem.startsWith(u8, input, "range"));
    return .{ .range = .{ .pipeline = parsePipeline(input[5..]) } };
}

fn parseIf(comptime input: []const u8) Node {
    std.debug.assert(mem.startsWith(u8, input, "if"));
    return .{ .if_ = .{ .pipeline = parsePipeline(input[2..]) } };
}

inline fn parseNodes(comptime fmt: []const u8) []const Node {
    var nodes: []const Node = &[0]Node{};
    var parser = Parser{ .buf = fmt };
    var trim_right = false;
    var trim_left = false;
    while (!parser.eos()) : (parser.pos += 1) {
        const c = parser.peek(0) orelse break;
        switch (c) {
            '{' => {
                const next = parser.peek(1) orelse break;
                if (next != '{') continue;
                var lit = parser.fromMark();
                if (nodes.len > 0) {
                    if (parser.peek(2)) |c2| {
                        if (parser.peek(3)) |c3| {
                            if (c2 == '-' and c3 == ' ') { // make sure '-' is followed by space too
                                lit = trimSpacesRight(lit);
                                trim_left = true;
                            }
                        }
                    }
                }
                if (trim_right) {
                    lit = trimSpacesLeft(lit);
                    trim_right = false;
                }

                if (lit.len != 0)
                    nodes = nodes ++ [1]Node{.{ .text = escape(lit) }};
                parser.mark(2);
                parser.pos += 1;
            },
            '}' => {
                const next = parser.peek(1) orelse break;
                if (next != c) continue;
                trim_right = if (parser.peek(-1)) |c2| if (parser.peek(-2)) |c3| c2 == '-' and c3 == ' ';
                var action = parser.fromMark();
                if (action.len != 0) {
                    if (trim_right) action = action[0 .. action.len - 1]; // remove the '-'
                    if (trim_left) {
                        action = action[1..]; // remove the '-'
                        trim_left = false;
                    }

                    action = trimSpaces(action);
                    if (mem.startsWith(u8, action, "range")) {
                        nodes = nodes ++ [1]Node{parseRange(escape(action))};
                    } else if (mem.startsWith(u8, action, "if")) {
                        nodes = nodes ++ [1]Node{parseIf(escape(action))};
                    } else if (mem.startsWith(u8, action, "else if")) {
                        // Treat
                        //	{{if a}}_{{else if b}}_{{end}}
                        // as
                        //	{{if a}}_{{else}}{{if b}}_{{end}}{{end}}.
                        nodes = nodes ++ [2]Node{ .else_, parseIf(escape(action[5..])) };
                    } else if (memeql(action, "else")) {
                        nodes = nodes ++ [1]Node{.else_};
                    } else if (memeql(action, "end"))
                        nodes = nodes ++ [1]Node{.end}
                    else
                        nodes = nodes ++ [1]Node{.{ .action = parsePipeline(escape(action)) }};
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
/// recursively appends children of range/ifs to their lists
/// until end action reached
fn parseTree(comptime flat_nodes: []const Node, comptime stop_types: []const NodeType) Node.List {
    var result: []const Node = &[0]Node{};

    var i: usize = 0;
    while (i < flat_nodes.len) : (i += 1) {
        var node = flat_nodes[i];
        // append children until end
        switch (node) {
            .range => {
                i += 1;
                const list = parseTree(flat_nodes[i..], &[_]NodeType{.end});
                node.range.list = list;
                if (list.len > 0)
                    i += list.len;
            },
            .if_ => {
                {
                    i += 1;
                    const list = parseTree(flat_nodes[i..], &[_]NodeType{ .end, .else_ });
                    node.if_.list = list;
                    if (list.len > 0)
                        i += list.len;
                }
                if (i < flat_nodes.len and flat_nodes[i] == .else_) {
                    i += 1;
                    const list = parseTree(flat_nodes[i..], &[_]NodeType{.end});
                    node.if_.else_list = list;
                    if (list.len > 0)
                        i += list.len;
                }
            },
            else => {
                if (std.mem.indexOfScalar(NodeType, stop_types, node)) |_| break;
            },
        }
        result = result ++ &[1]Node{node};
    }
    return .{ .len = i, .root = result };
}

// fn visitNodes(nodes: []const Node, ctx: anytype, cb: fn (Node, @TypeOf(ctx)) void) void {
//     for (nodes) |node| {
//         switch (node) {
//             .range => {
//                 cb(node, ctx);
//                 visitTree(@field(node, @tagName(node)).body, ctx, cb);
//             },
//             else => cb(node, ctx),
//         }
//     }
// }

const Options = struct {
    eval_branch_quota: usize = 2000,
    name: ?[]const u8 = null,
};
pub fn Template(comptime fmt: []const u8, comptime options_: Options) type {
    @setEvalBranchQuota(options_.eval_branch_quota);
    var flat_nodes = parseNodes(fmt);
    // flat_nodes is a flat list of nodes here.
    // need to nest range nodes in range / if.
    var tree_ = parseTree(flat_nodes, &[_]NodeType{});
    var root = tree_.root;
    // append remaining non-nested nodes
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

        fn writeNestedCmdFields(comptime cmds: []const Node.Command, writer: anytype, args: anytype) !void {
            std.debug.assert(cmds.len > 0);
            // make a tuple of all the nested types in args
            //   example - cmds: ".field1.field2"
            //   types[0] = @TypeOf(args)
            //   types[1] = @TypeOf(@field(args, "field1"))
            //   types[2] = @TypeOf(@field(@field(args, "field1"), "field2"))
            comptime var types: []const type = &[_]type{@TypeOf(args)};
            inline for (cmds) |cmd, i| {
                comptime var dummy: types[i] = undefined;
                switch (cmd) {
                    .field => if (@hasField(types[i], cmd.field) or @hasDecl(types[i], cmd.field)) {
                        types = types ++ [_]type{@TypeOf(@field(dummy, cmd.field))};
                    } else comptime showError("type {} has no field or public decl '{s}'", .{ types[i], cmd.field }),
                    .func => if (@hasField(types[i], cmd.func[0]) or @hasDecl(types[i], cmd.func[0])) {
                        const T = @TypeOf(@field(dummy, cmd.func[0]));
                        types = types ++ [_]type{T};
                    } else comptime showError("type {} has no public decl '{s}'", .{ types[i], cmd.func[0] }),
                    else => todo("support {s} command type", .{std.meta.tagName(cmd)}),
                }
            }
            const Tuple = std.meta.Tuple(types);
            comptime var tuple: Tuple = undefined;

            // assign nested values until we find one to write
            tuple[0] = args;
            inline for (std.meta.fields(Tuple)[1..]) |_, fi| {
                const field = switch (cmds[fi]) {
                    .field => @field(tuple[fi], cmds[fi].field),
                    // TODO: function arguments
                    // TODO: support func.fields
                    .func => @call(.{}, @field(tuple[fi], cmds[fi].func[0]), .{}),
                    else => todo("unsupported command type {}", .{cmds[fi]}),
                };

                const ti = @typeInfo(@TypeOf(field));
                switch (ti) {
                    .Struct, .Union, .Enum => tuple[fi + 1] = field,
                    else => {
                        // TODO: verify field is a string or use another method like print or std.fmt.formatType
                        _ = try writer.write(field);
                        break;
                    },
                }
            }
            // std.debug.print("{}\n", .{tuple});
        }

        pub const BufPrintError = error{ DuplicateKey, InvalidConditionType } || std.io.FixedBufferStream([]u8).WriteError;
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
                        if (node.action.cmds.len == 0 and node.action.decls.len == 0) @compileError("no commands or declarations present");
                        if (node.action.decls.len != 0) todo("support action.decls", .{});
                        if (node.action.cmds.len == 0) @compileError("action with no commands");

                        const first_cmd = node.action.cmds[0];
                        switch (first_cmd) {
                            .field, .func => try writeNestedCmdFields(node.action.cmds, writer, args),
                            .constant => _ = try writer.write(first_cmd.constant),
                            .variable => {
                                // search scopes
                                inline for (scopes) |scope| {
                                    inline for (scope) |entry| {
                                        // std.debug.print("searching scopes for '{s}' action '{s}'\n", .{ entry.key, node.action });
                                        if (memeql(entry.key, first_cmd.variable)) {
                                            // std.debug.print("found scope entry {s} {}\n", .{ entry.key, entry.value });
                                            _ = try writer.print("{}", .{entry.value});
                                        }
                                    }
                                }
                            },
                            .range => unreachable,
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
                    .if_ => {
                        const field = node.if_.pipeline.?.cmds[0].field;
                        const condition = if (@hasField(@TypeOf(args), field)) @field(args, field) else null;
                        if (!try empty(condition))
                            try bufPrintImpl(scopes, node.if_.list.?.root, writer, args)
                        else if (node.if_.else_list) |else_list|
                            try bufPrintImpl(scopes, else_list.root, writer, args);
                    },
                    .else_, .end => unreachable,
                }
            }
        }

        // TODO: review rules for truthiness
        // TODO: support maps
        fn empty(value: anytype) !bool {
            const V = @TypeOf(value);
            const vti = @typeInfo(V);
            return switch (vti) {
                .Int, .Float, .ComptimeInt, .ComptimeFloat => value == 0,
                .Pointer => switch (vti.Pointer.size) {
                    .One => empty(value.*),
                    .Slice => value.len == 0,
                    else => error.InvalidConditionType,
                    // other types to consider: .Many, .C
                },
                .Optional => value == null or empty(value.?),
                .Bool => !value,
                .Array, .Vector => value.len == 0,
                .Null => true,
                else => if (@hasField(V, "len") or @hasDecl(V, "len"))
                    value.len == 0
                else
                    error.InvalidConditionType,
                    // other types to consider: .Void, .Struct, .Union, .Enum, .EnumLiteral
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
