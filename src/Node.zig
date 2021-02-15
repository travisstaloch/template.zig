const std = @import("std");

pub const NodeType = std.meta.TagType(Node);
pub const Node = union(enum) {
    text: []const u8,
    /// something bounded by delimiters
    /// action represents simple field evaluations and parenthesized pipelines.
    action: Pipeline,
    range: Branch,
    if_: Branch,
    command: Command,
    identifier: []const []const u8,
    else_,
    end,
    // err: struct { err: anyerror, message: []const u8 },
    with: Branch,
    comment,
    template: TemplateInternal,
    field: []const []const u8,
    chain: Chain,
    variable: []const []const u8,
    constant: Constant,
    pipeline: Pipeline,
    interval: struct { start: usize, end: usize },
    dot,
    // bool: bool,
    // string: []const u8,
    // number: []const u8,
    nil,
    define: TemplateInternal,
    list: List,

    pub const Branch = struct {
        pipeline: ?Pipeline = null,
        list: ?List = null,
        else_list: ?List = null,
    };
    pub const List = struct {
        len: usize,
        root: []const Node,
        pub fn init() List {
            return .{ .len = 0, .root = &[0]Node{} };
        }
    };
    pub const Pipeline = struct {
        is_assign: bool, // The variables are being assigned, not declared.
        decls: []const []const u8, // Variables in lexical order.
        cmds: []const Command, // The commands in lexical order.
        pub fn init() Pipeline {
            return .{ .is_assign = false, .cmds = &[0]Node.Command{}, .decls = &[0][]const u8{} };
        }
    };
    pub const Command = struct {
        args: []const Node,
    };
    pub const Chain = struct {
        node: *const Node,
        field: []const []const u8,
    };
    pub const Constant = union(enum) {
        int: []const u8,
        string: []const u8,
        char: []const u8,
        bool: []const u8,
        pub fn payload(self: Constant) []const u8 {
            return switch (self) {
                .int => self.int,
                .string => self.string,
                .char => self.char,
                .bool => self.bool,
            };
        }
    };
    pub const TemplateInternal = struct {
        name: []const u8,
        pipeline: ?Pipeline,
    };

    fn formatStringList(strs: []const []const u8, separator: []const u8, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        for (strs) |str, i| {
            _ = try writer.write(str);
            if (i != strs.len - 1)
                _ = try writer.write(separator);
        }
    }

    fn formatPipeline(pipeline: Node.Pipeline, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        if (pipeline.decls.len > 0) {
            for (pipeline.decls) |decl, i| {
                if (i > 0) _ = try writer.write(", ");
                _ = try writer.write(decl);
            }
            _ = try writer.write(" := ");
        }
        for (pipeline.cmds) |cmd, i| {
            if (i > 0) _ = try writer.write(" | ");
            try format(.{ .command = cmd }, fmt, options, writer);
        }
    }

    fn formatBranch(comptime branch_name: []const u8, branch: Node.Branch, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.write("{{" ++ branch_name);
        try formatPipeline(branch.pipeline.?, fmt, options, writer);
        _ = try writer.write("}}");
        if (branch.list) |list| for (list.root) |node| try format(node, fmt, options, writer);
        if (branch.else_list) |list| {
            _ = try writer.write("{{else}}");
            for (list.root) |node| try format(node, fmt, options, writer);
        }
        _ = try writer.write("{{end}}");
    }

    pub fn format(node: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (node) {
            .text => _ = try writer.write(node.text),
            .action => {
                _ = try writer.write("{{");
                try formatPipeline(node.action, fmt, options, writer);
                _ = try writer.write("}}");
            },
            .range => try formatBranch("range ", node.range, fmt, options, writer),
            .with => try formatBranch("with ", node.with, fmt, options, writer),
            .if_ => try formatBranch("if ", node.if_, fmt, options, writer),
            .comment => _ = try writer.write("/* comment omitted */"),
            .field => {
                _ = try writer.write(".");
                try formatStringList(node.field, ".", fmt, options, writer);
            },
            .variable => try formatStringList(node.variable, ".", fmt, options, writer),
            .identifier => _ = try formatStringList(node.identifier, ".", fmt, options, writer),
            .template => {
                try writer.print("{{{{{s} {s}", .{ @tagName(node), node.template.name });
                if (node.template.pipeline) |pipeline| {
                    _ = try writer.writeByte(' ');
                    try formatPipeline(pipeline, fmt, options, writer);
                }
                _ = try writer.write("}}");
            },
            .pipeline => try formatPipeline(node.pipeline, fmt, options, writer),
            .constant => _ = try writer.write(node.constant.payload()),
            .interval => try writer.print("{}..{}", .{ node.interval.start, node.interval.end }),
            .dot => _ = try writer.write("."),
            .end => _ = try writer.write("end"),
            .else_ => _ = try writer.write("else"),
            .command => {
                for (node.command.args) |arg, i| {
                    if (i > 0) _ = try writer.writeByte(' ');
                    if (arg == .pipeline) {
                        _ = try writer.writeByte('(');
                        try formatPipeline(arg.pipeline, fmt, options, writer);
                        _ = try writer.writeByte(')');
                        continue;
                    }
                    try writer.print("{}", .{arg});
                }
            },
            .chain => {
                const n = node.chain.node;
                if (n.* == .pipeline) {
                    _ = try writer.writeByte('(');
                    try formatPipeline(n.pipeline, fmt, options, writer);
                    _ = try writer.writeByte(')');
                    // } else try writer.print("{}", .{n.*});
                } else try format(n.*, fmt, options, writer);
                if (node.chain.field.len > 0) {
                    _ = try writer.writeByte('.');
                    try formatStringList(node.chain.field, ".", fmt, options, writer);
                }
            },
            .nil => _ = try writer.write("nil"),
            .define => {
                _ = try writer.write("define ...");
                @panic("TODO");
            },
            .list => for (node.list.root) |n| try format(n, fmt, options, writer),
        }
    }
};
