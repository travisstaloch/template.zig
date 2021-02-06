const std = @import("std");
usingnamespace @import("template.zig");

// -------------
// --- Tests ---
// -------------
const t = std.testing;
const allocator = t.allocator;

fn t_expectEqual(actual: anytype, expected: @TypeOf(actual)) void {
    t.expectEqual(expected, actual);
}

fn expectNode(node: Node, comptime node_type: NodeType, expected_text: comptime []const u8) void {
    t.expect(node == node_type);
    t.expectEqualStrings(expected_text, @field(node, @tagName(node_type)));
}

fn expectPrinted(expected: []const u8, tmpl: anytype, args: anytype) !void {
    t.expectEqualStrings(expected, try tmpl.bufPrint(&print_buf, args));
    const msg = try tmpl.allocPrint(t.allocator, args);
    defer t.allocator.free(msg);
    t.expectEqualStrings(expected, msg);
}

test "parser" {
    const text = "aabbccdd";
    var p = Parser{ .buf = text, .pos = 0 };
    t.expectEqualStrings("aa", p.untilStr("bb"));
    t.expectEqualStrings("bbccdd", p.buf[p.pos..]);
    t.expectEqualStrings("bb", p.untilStr("cc"));
    t.expectEqualStrings("ccdd", p.buf[p.pos..]);
    t.expectEqualStrings("ccdd", p.untilStr("{{"));
}

test "text" {
    {
        const text = "";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.tree.root.len, 0);
        t.expectEqualStrings(text, try tmpl.bufPrint(&print_buf, .{}));
        try expectPrinted(text, tmpl, .{});
    }
    {
        const text = "hello";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.tree.root.len, 1);
        expectNode(tmpl.tree.root[0], .text, text);
        t.expectEqualStrings(text, try tmpl.bufPrint(&print_buf, .{}));
        try expectPrinted(text, tmpl, .{});
    }
    {
        const text = "}{hello{}";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.tree.root.len, 1);
        expectNode(tmpl.tree.root[0], .text, text);
        try expectPrinted(text, tmpl, .{});
    }
}

test "escapes" {
    const tmpl = Template("\\{\\{0\\}\\}", .{});
    t_expectEqual(tmpl.tree.root.len, 1);
    expectNode(tmpl.tree.root[0], .text, "{{0}}");
    try expectPrinted("{{0}}", tmpl, .{});
}

test "variables" {
    {
        const text = "{{.name}}";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.tree.root.len, 1);
        // expectNode(tmpl.tree.root[0], .action, "name");
        try expectPrinted("zero", tmpl, .{ .name = "zero" });
    }
    {
        const text = "Hi {{.name}} at index #{{.index}}";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.tree.root.len, 4);
        expectNode(tmpl.tree.root[0], .text, "Hi ");
        // expectNode(tmpl.tree.root[1], .action, "name");
        t.expectEqualStrings("name", tmpl.tree.root[1].action.cmds[0].field);
        expectNode(tmpl.tree.root[2], .text, " at index #");
        t.expectEqualStrings("index", tmpl.tree.root[3].action.cmds[0].field);
        try expectPrinted("Hi zero at index #000", tmpl, .{ .name = "zero", .index = "000" });
    }
}

test "range" {
    {
        const text = "{{ range $i, $e := 1..2 }} a {{$i}}:{{$e}} b {{ end }} c";
        const tmpl = Template(text, .{ .eval_branch_quota = 8000 });
        t_expectEqual(tmpl.tree.root.len, 2);
        t.expect(tmpl.tree.root[0] == .range);
        t.expect(tmpl.tree.root[1] == .text);
        // std.debug.print("tmpl {}\n", .{tmpl.tree.root});
        t_expectEqual(tmpl.tree.root[0].range.pipeline.?.decls.len, 2);
        t.expectEqualStrings(tmpl.tree.root[0].range.pipeline.?.decls[0], "$i");
        t.expectEqualStrings(tmpl.tree.root[0].range.pipeline.?.decls[1], "$e");
        t_expectEqual(tmpl.tree.root[0].range.pipeline.?.cmds[0].range.start, 1);
        t_expectEqual(tmpl.tree.root[0].range.pipeline.?.cmds[0].range.end, 2);
        t_expectEqual(tmpl.tree.root[0].range.list.?.len, 5);
        try expectPrinted(" a 0:1 b  a 1:2 b  c", tmpl, .{});
    }
    {
        const text =
            \\{{ range $index := 0..1 }}level1 {{$index}}{{
            \\      range $index2 := 0..0}} level2 {{$index}}{{$index2}}{{ end }} endlevel1 {{ end }}
        ;
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.tree.root.len, 1);
        t.expect(tmpl.tree.root[0] == .range);
        t_expectEqual(tmpl.tree.root[0].range.list.?.len, 8);
        expectNode(tmpl.tree.root[0].range.list.?.root[0], .text, "level1 ");
        // expectNode(tmpl.tree.root[0].range.list.?.root[1], .action, "$index");
        t.expectEqualStrings("$index", tmpl.tree.root[0].range.list.?.root[1].action.cmds[0].identifier);
        t.expect(tmpl.tree.root[0].range.list.?.root[2] == .range);
        t.expectEqualStrings("$index2", tmpl.tree.root[0].range.list.?.root[2].range.pipeline.?.decls[0]);
        // expectNode(tmpl.tree.root[0].range.list.?.root[1], .action, "$index");
        t_expectEqual(tmpl.tree.root[0].range.list.?.root[2].range.list.?.root.len, 3);
        expectNode(tmpl.tree.root[0].range.list.?.root[2].range.list.?.root[0], .text, " level2 ");
        t.expectEqualStrings("$index", tmpl.tree.root[0].range.list.?.root[2].range.list.?.root[1].action.cmds[0].identifier);
        t.expectEqualStrings("$index2", tmpl.tree.root[0].range.list.?.root[2].range.list.?.root[2].action.cmds[0].identifier);
        expectNode(tmpl.tree.root[0].range.list.?.root[3], .text, " endlevel1 ");
        try expectPrinted("level1 0 level2 00 endlevel1 level1 1 level2 10 endlevel1 ", tmpl, .{});
    }
}

// TODO convert to range
test "range2" {
    {
        const text =
            \\{{ range $index, $item := .list }}
            \\Print {{$item}} at {{$index}}{{ end }}
        ;
        const tmpl = Template(text, .{ .eval_branch_quota = 4000 });
        t_expectEqual(tmpl.tree.root.len, 1);
        t.expect(tmpl.tree.root[0] == .range);
        t.expectEqualStrings("$index", tmpl.tree.root[0].range.pipeline.?.decls[0]);
        t.expectEqualStrings("$item", tmpl.tree.root[0].range.pipeline.?.decls[1]);
        t.expectEqualStrings("list", tmpl.tree.root[0].range.pipeline.?.cmds[0].field);
        t_expectEqual(tmpl.tree.root[0].range.list.?.root.len, 4);
        expectNode(tmpl.tree.root[0].range.list.?.root[0], .text, "\nPrint ");
        t.expectEqualStrings("$item", tmpl.tree.root[0].range.list.?.root[1].action.cmds[0].identifier);
        t.expectEqualStrings("$index", tmpl.tree.root[0].range.list.?.root[3].action.cmds[0].identifier);
        expectNode(tmpl.tree.root[0].range.list.?.root[2], .text, " at ");
        const list = [_]u8{ 84, 168 };
        try expectPrinted("\nPrint 84 at 0\nPrint 168 at 1", tmpl, .{ .list = list });
    }
}

test "scopes" {
    const text = "{{ range $i := 0..1}}{{ range $i := 0..1}}{{end}}{{end}}";
    const tmpl = Template(text, .{ .eval_branch_quota = 4000 });
    t.expectError(error.DuplicateKey, tmpl.bufPrint(&print_buf, .{}));
}

test "mixed scope types" { // no for_each capture index
    const text = "{{ range $j := 0..0 }}{{ range $i := .list }}{{$i}} - {{$j}}, {{ end }}{{ end }}";
    const tmpl = Template(text, .{ .eval_branch_quota = 4000 });
    t_expectEqual(tmpl.tree.root.len, 1);
    const list = [_]u128{ 1, 2 };
    try expectPrinted("1 - 0, 2 - 0, ", tmpl, .{ .list = list });
}

test "if" {
    {
        const text = "{{if .cond}}a{{end}}";
        const tmpl = Template(text, .{});
        t.expect(tmpl.tree.root[0] == .if_);
        t.expectEqualStrings("cond", tmpl.tree.root[0].if_.pipeline.?.cmds[0].field);
        t_expectEqual(tmpl.tree.root[0].if_.list.?.root.len, 1);
        try expectPrinted("a", tmpl, .{ .cond = "foo" });
        try expectPrinted("", tmpl, .{ .cond = "" });
    }
    {
        const text = "{{if .cond}}a{{else}}b{{end}}";
        const tmpl = Template(text, .{});
        t.expect(tmpl.tree.root[0] == .if_);
        t.expectEqualStrings("cond", tmpl.tree.root[0].if_.pipeline.?.cmds[0].field);
        t_expectEqual(tmpl.tree.root[0].if_.list.?.len, 1);
        t.expectEqual(tmpl.tree.root[0].if_.else_list.?.len, 1);
        t.expect(tmpl.tree.root[0].if_.else_list.?.root[0] == .text);
        try expectPrinted("a", tmpl, .{ .cond = "foo" });
        try expectPrinted("b", tmpl, .{ .cond = "" });
    }
    {
        const text = "{{if .cond}}a{{else if .cond2}}b{{else}}c{{end}}";
        const tmpl = Template(text, .{ .eval_branch_quota = 4000 });
        t.expect(tmpl.tree.root[0] == .if_);
        t.expectEqualStrings("cond", tmpl.tree.root[0].if_.pipeline.?.cmds[0].field);
        t_expectEqual(tmpl.tree.root[0].if_.list.?.len, 1);
        t.expectEqual(tmpl.tree.root[0].if_.else_list.?.len, 5);
        t.expect(tmpl.tree.root[0].if_.else_list.?.root[0] == .if_);
        t.expect(tmpl.tree.root[0].if_.else_list.?.root[0].if_.pipeline.?.cmds[0] == .field);
        t.expect(tmpl.tree.root[0].if_.else_list.?.root[0].if_.list.?.root[0] == .text);
        t.expect(tmpl.tree.root[0].if_.else_list.?.root[0].if_.else_list.?.root[0] == .text);
        t.expectEqualStrings("c", tmpl.tree.root[0].if_.else_list.?.root[0].if_.else_list.?.root[0].text);
        try expectPrinted("a", tmpl, .{ .cond = true });
        try expectPrinted("a", tmpl, .{ .cond = [1]bool{false} });
        try expectPrinted("b", tmpl, .{ .cond2 = true });
        try expectPrinted("c", tmpl, .{ .cond = @as(?u8, null), .cond2 = [0]bool{} });
        try expectPrinted("c", tmpl, .{ .cond = 0, .cond2 = 0.0 });
        try expectPrinted("c", tmpl, .{ .cond = @as(u8, 0), .cond2 = @as(f32, 0.0) });
    }
}

test "multiple templates" {
    const template = @import("template.zig");
    const T1 = Template("T1", .{ .name = "T1" });
    const T2 = Template("T2", .{ .name = "T2" });
    t.expectEqualStrings("T1", T1.options.name.?);
    t.expectEqualStrings("T2", T2.options.name.?);
    try expectPrinted("T1", T1, .{});
    try expectPrinted("T2", T2, .{});
}

test "trim spaces" {
    const tmpl = Template("{{23 -}} < {{- 45}}", .{});
    try expectPrinted("23<45", tmpl, .{});
}

test "constants" {
    const tmpl = Template("{{-23 -}} < {{- -45}}", .{});
    try expectPrinted("-23<-45", tmpl, .{});
}

test "pipe" {
    const tmpl = Template("{{.field1 | .field2}}", .{});
    t.expect(tmpl.tree.root[0].action.cmds.len == 2);
    t.expectEqualStrings("field1", tmpl.tree.root[0].action.cmds[0].field);
    t.expectEqualStrings("field2", tmpl.tree.root[0].action.cmds[1].field);
    try expectPrinted("value2", tmpl, .{ .field1 = .{ .field2 = "value2" } });
}

// --------------------
// --- readme tests ---
// --------------------
var print_buf: [1000]u8 = undefined;

test "template variables" {
    const Tmpl = @import("template.zig").Template;
    const tmpl = Tmpl(
        "Hello {{.world}}",
        .{ .eval_branch_quota = 1000 }, // default value. same as .{}
    );
    // bufPrint
    const message = try tmpl.bufPrint(&print_buf, .{ .world = "friends" });
    std.testing.expectEqualStrings("Hello friends", message);
    // allocPrint
    const message2 = try tmpl.allocPrint(std.testing.allocator, .{ .world = "again friends" });
    defer std.testing.allocator.free(message2);
    std.testing.expectEqualStrings("Hello again friends", message2);
}

test "for range loop" {
    const Tmpl = @import("template.zig").Template;
    const tmpl = Tmpl(
        "5 times: {{ range $index := 0..4 }}{{ $index }}{{ end }}",
        .{ .eval_branch_quota = 4000 },
    );
    // bufPrint
    const message = try tmpl.bufPrint(&print_buf, .{});
    std.testing.expectEqualStrings("5 times: 01234", message);
    // allocPrint
    const message2 = try tmpl.allocPrint(std.testing.allocator, .{});
    defer std.testing.allocator.free(message2);
    std.testing.expectEqualStrings("5 times: 01234", message2);
}

test "for each loop" {
    const Tmpl = @import("template.zig").Template;
    const tmpl = Tmpl(
        "5 times: {{ range $index, $item := .items }}{{$item}}-{{ $index }},{{ end }}",
        .{ .eval_branch_quota = 6000 },
    );
    // bufPrint
    const items = [_]u8{ 0, 1, 2, 3, 4 };
    const message = try tmpl.bufPrint(&print_buf, .{ .items = items });
    std.testing.expectEqualStrings("5 times: 0-0,1-1,2-2,3-3,4-4,", message);
    // allocPrint
    const message2 = try tmpl.allocPrint(std.testing.allocator, .{ .items = items });
    defer std.testing.allocator.free(message2);
    std.testing.expectEqualStrings("5 times: 0-0,1-1,2-2,3-3,4-4,", message2);
}

test "if - else if - else" {
    const Tmpl = @import("template.zig").Template;
    const tmpl = Tmpl("{{if .cond}}a{{else if .cond2}}b{{else}}c{{end}}", .{ .eval_branch_quota = 2000 });
    std.testing.expectEqualStrings("a", try tmpl.bufPrint(&print_buf, .{ .cond = true }));
    std.testing.expectEqualStrings("b", try tmpl.bufPrint(&print_buf, .{ .cond2 = 1 }));
    std.testing.expectEqualStrings("c", try tmpl.bufPrint(&print_buf, .{}));
}

// --------------------
// - end readme tests -
// --------------------
