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
        expectFragment(tmpl.fragments[0], .variable, "name");
        try expectPrinted("zero", tmpl, .{ .name = "zero" });
    }
    {
        const text = "Hi {{name}} at index #{{index}}";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 4);
        expectFragment(tmpl.fragments[0], .literal, "Hi ");
        expectFragment(tmpl.fragments[1], .variable, "name");
        expectFragment(tmpl.fragments[2], .literal, " at index #");
        expectFragment(tmpl.fragments[3], .variable, "index");
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
        expectFragment(tmpl.fragments[0].for_range.body[1], .variable, "index");
        expectFragment(tmpl.fragments[0].for_range.body[2], .literal, " b ");
        t.expect(tmpl.fragments[1] == .literal);
        t.expectEqualStrings(" c", tmpl.fragments[1].literal);
        try expectPrinted(" a 0 b  a 1 b  c", tmpl, .{});
    }
    {
        const text =
            \\{{ for(0..2) |index| }}level1 {{index}}{{
            \\      for(0..1) |index2|}} level2 {{index}}{{index2}}{{ end }} endlevel1 {{ end }}"
        ;
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 2);
        t.expect(tmpl.fragments[0] == .for_range);
        t_expectEqual(tmpl.fragments[0].for_range.body.len, 4);
        expectFragment(tmpl.fragments[0].for_range.body[0], .literal, "level1 ");
        expectFragment(tmpl.fragments[0].for_range.body[1], .variable, "index");
        t.expect(tmpl.fragments[0].for_range.body[2] == .for_range);
        t.expectEqualStrings("index2", tmpl.fragments[0].for_range.body[2].for_range.capture_name);
        expectFragment(tmpl.fragments[0].for_range.body[1], .variable, "index");
        t_expectEqual(tmpl.fragments[0].for_range.body[2].for_range.body.len, 3);
        expectFragment(tmpl.fragments[0].for_range.body[2].for_range.body[0], .literal, " level2 ");
        expectFragment(tmpl.fragments[0].for_range.body[2].for_range.body[1], .variable, "index");
        expectFragment(tmpl.fragments[0].for_range.body[2].for_range.body[2], .variable, "index2");
        expectFragment(tmpl.fragments[0].for_range.body[3], .literal, " endlevel1 ");
        try expectPrinted("level1 0 level2 00 endlevel1 level1 1 level2 10 endlevel1 ", tmpl, .{});
    }
}

test "foreach" {
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
    expectFragment(tmpl.fragments[0].for_each.body[1], .variable, "item");
    expectFragment(tmpl.fragments[0].for_each.body[2], .literal, " at ");
    expectFragment(tmpl.fragments[0].for_each.body[3], .variable, "index");
    try expectPrinted("\nPrint 84 at 0\nPrint 168 at 1", tmpl, .{ .list = &[_]u8{ 84, 168 } });
}
