const std = @import("std");
usingnamespace @import("template.zig");

// -------------
// --- Tests ---
// -------------
const t = std.testing;
const allocator = t.allocator;
var print_buf: [1000]u8 = undefined;

fn t_expectEqual(actual: anytype, expected: @TypeOf(actual)) void {
    t.expectEqual(expected, actual);
}

fn expectFragment(frag: Frag, comptime frag_type: FragType, expected_text: comptime []const u8) void {
    t.expect(frag == frag_type);
    t.expectEqualStrings(expected_text, @field(frag, @tagName(frag_type)));
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

test "literal" {
    {
        const text = "";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 0);
        t.expectEqualStrings(text, try tmpl.bufPrint(&print_buf, .{}));
        try expectPrinted(text, tmpl, .{});
    }
    {
        const text = "hello";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 1);
        expectFragment(tmpl.fragments[0], .literal, text);
        t.expectEqualStrings(text, try tmpl.bufPrint(&print_buf, .{}));
        try expectPrinted(text, tmpl, .{});
    }
    {
        const text = "}{hello{}";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 1);
        expectFragment(tmpl.fragments[0], .literal, text);
        try expectPrinted(text, tmpl, .{});
    }
}

test "escapes" {
    const tmpl = Template("\\{\\{0\\}\\}", .{});
    t_expectEqual(tmpl.fragments.len, 1);
    expectFragment(tmpl.fragments[0], .literal, "{{0}}");
    try expectPrinted("{{0}}", tmpl, .{"zug"});
}

test "variables" {
    {
        const text = "{{name}}";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 1);
        expectFragment(tmpl.fragments[0], .action, "name");
        try expectPrinted("zero", tmpl, .{ .name = "zero" });
    }
    {
        const text = "Hi {{name}} at index #{{index}}";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 4);
        expectFragment(tmpl.fragments[0], .literal, "Hi ");
        expectFragment(tmpl.fragments[1], .action, "name");
        expectFragment(tmpl.fragments[2], .literal, " at index #");
        expectFragment(tmpl.fragments[3], .action, "index");
        try expectPrinted("Hi zero at index #000", tmpl, .{ .name = "zero", .index = "000" });
    }
}

test "for range" {
    {
        const text = "{{ for(0..2) |index| }} a {{index}} b {{ end }} c";
        const tmpl = Template(text, .{ .eval_branch_quota = 8000 });
        t_expectEqual(tmpl.fragments.len, 2);
        t.expect(tmpl.fragments[0] == .for_range);
        t_expectEqual(tmpl.fragments[0].for_range.start, 0);
        t_expectEqual(tmpl.fragments[0].for_range.end, 2);
        t.expectEqualStrings("index", tmpl.fragments[0].for_range.capture_name);
        t_expectEqual(tmpl.fragments[0].for_range.body.len, 3);
        expectFragment(tmpl.fragments[0].for_range.body[0], .literal, " a ");
        expectFragment(tmpl.fragments[0].for_range.body[1], .action, "index");
        expectFragment(tmpl.fragments[0].for_range.body[2], .literal, " b ");
        t.expect(tmpl.fragments[1] == .literal);
        t.expectEqualStrings(" c", tmpl.fragments[1].literal);
        try expectPrinted(" a 0 b  a 1 b  c", tmpl, .{});
    }
    {
        const text =
            \\{{ for(0..2) |index| }}level1 {{index}}{{
            \\      for(0..1) |index2|}} level2 {{index}}{{index2}}{{ end }} endlevel1 {{ end }}
        ;
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 1);
        t.expect(tmpl.fragments[0] == .for_range);
        t_expectEqual(tmpl.fragments[0].for_range.body.len, 4);
        expectFragment(tmpl.fragments[0].for_range.body[0], .literal, "level1 ");
        expectFragment(tmpl.fragments[0].for_range.body[1], .action, "index");
        t.expect(tmpl.fragments[0].for_range.body[2] == .for_range);
        t.expectEqualStrings("index2", tmpl.fragments[0].for_range.body[2].for_range.capture_name);
        expectFragment(tmpl.fragments[0].for_range.body[1], .action, "index");
        t_expectEqual(tmpl.fragments[0].for_range.body[2].for_range.body.len, 3);
        expectFragment(tmpl.fragments[0].for_range.body[2].for_range.body[0], .literal, " level2 ");
        expectFragment(tmpl.fragments[0].for_range.body[2].for_range.body[1], .action, "index");
        expectFragment(tmpl.fragments[0].for_range.body[2].for_range.body[2], .action, "index2");
        expectFragment(tmpl.fragments[0].for_range.body[3], .literal, " endlevel1 ");
        try expectPrinted("level1 0 level2 00 endlevel1 level1 1 level2 10 endlevel1 ", tmpl, .{});
    }
}

test "foreach" {
    {
        const text =
            \\{{ for(list) |item, index| }}
            \\Print {{item}} at {{index}}{{ end }}
        ;
        const tmpl = Template(text, .{ .eval_branch_quota = 4000 });
        t_expectEqual(tmpl.fragments.len, 1);
        t.expect(tmpl.fragments[0] == .for_each);
        t.expectEqualStrings("list", tmpl.fragments[0].for_each.slice_name);
        t.expectEqualStrings("item", tmpl.fragments[0].for_each.capture_name);
        t.expectEqualStrings("index", tmpl.fragments[0].for_each.capture_index_name.?);
        t_expectEqual(tmpl.fragments[0].for_each.body.len, 4);
        expectFragment(tmpl.fragments[0].for_each.body[0], .literal, "\nPrint ");
        expectFragment(tmpl.fragments[0].for_each.body[1], .action, "item");
        expectFragment(tmpl.fragments[0].for_each.body[2], .literal, " at ");
        expectFragment(tmpl.fragments[0].for_each.body[3], .action, "index");
        const list = [_]u8{ 84, 168 };
        try expectPrinted("\nPrint 84 at 0\nPrint 168 at 1", tmpl, .{ .list = list });
    }
}

test "scopes" {
    const text = "{{for(0..1)|i|}}{{for(0..1)|i|}}{{end}}{{end}}";
    const tmpl = Template(text, .{ .eval_branch_quota = 2000 });
    t.expectError(error.DuplicateKey, tmpl.bufPrint(&print_buf, .{}));
}

test "mixed scope types" { // no for_each capture index
    const text = "{{ for(0..1)|j| }}{{ for(list)|i| }}{{i}} - {{j}}, {{ end }}{{ end }}";
    const tmpl = Template(text, .{ .eval_branch_quota = 2000 });
    t_expectEqual(tmpl.fragments.len, 1);
    const list = [_]u128{ 1, 2 };
    try expectPrinted("1 - 0, 2 - 0, ", tmpl, .{ .list = list });
}

test "if" {
    {
        const text = "{{if cond}}a{{end}}";
        const tmpl = Template(text, .{});
        t.expect(tmpl.fragments[0] == .if_);
        t.expectEqualStrings("cond", tmpl.fragments[0].if_.condition);
        t_expectEqual(tmpl.fragments[0].if_.body.len, 1);
        try expectPrinted("a", tmpl, .{ .cond = "asd" });
        try expectPrinted("", tmpl, .{ .cond = "" });
    }
    {
        const text = "{{if cond}}a{{else}}b{{end}}";
        const tmpl = Template(text, .{});
        t.expect(tmpl.fragments[0] == .if_);
        t.expectEqualStrings("cond", tmpl.fragments[0].if_.condition);
        t_expectEqual(tmpl.fragments[0].if_.body.len, 3);
        t.expect(tmpl.fragments[0].if_.body[1] == .else_);
        try expectPrinted("a", tmpl, .{ .cond = "asd" });
        try expectPrinted("b", tmpl, .{ .cond = "" });
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

// --------------------
// --- readme tests ---
// --------------------
test "template variables" {
    const Tmpl = @import("template.zig").Template;
    const tmpl = Tmpl(
        "Hello {{world}}",
        .{ .eval_branch_quota = 1000 }, // default value. same as .{}
    );
    // bufPrint
    var buf: [100]u8 = undefined;
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
        "5 times: {{ for( 0..5 ) | index | }}{{ index }}{{ end }}",
        .{ .eval_branch_quota = 4000 },
    );
    // bufPrint
    var buf: [100]u8 = undefined;
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
        "5 times: {{ for( items ) | item, index | }}{{item}}-{{ index }},{{ end }}",
        .{ .eval_branch_quota = 4000 },
    );
    // bufPrint
    var buf: [100]u8 = undefined;
    const items = [_]u8{ 0, 1, 2, 3, 4 };
    const message = try tmpl.bufPrint(&print_buf, .{ .items = items });
    std.testing.expectEqualStrings("5 times: 0-0,1-1,2-2,3-3,4-4,", message);
    // allocPrint
    const message2 = try tmpl.allocPrint(std.testing.allocator, .{ .items = items });
    defer std.testing.allocator.free(message2);
    std.testing.expectEqualStrings("5 times: 0-0,1-1,2-2,3-3,4-4,", message2);
}
// --------------------
// - end readme tests -
// --------------------
