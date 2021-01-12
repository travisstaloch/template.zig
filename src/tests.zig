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

fn expectFragment(frag: Frag, comptime frag_type: FragType, expected_text: comptime []const u8) void {
    t.expect(frag == frag_type);
    t.expectEqualStrings(expected_text, @field(frag, @tagName(frag_type)));
}

var out_buf: [1000]u8 = undefined;
test "literal" {
    {
        const text = "";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 0);
        t.expectEqualStrings(text, try tmpl.bufPrint(&out_buf, .{}));
    }
    {
        const text = "hello";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 1);
        expectFragment(tmpl.fragments[0], .literal, text);
        t.expectEqualStrings(text, try tmpl.bufPrint(&out_buf, .{}));
    }
    {
        const text = "}{hello{}";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 1);
        expectFragment(tmpl.fragments[0], .literal, text);
    }
}

test "escapes" {
    const tmpl = Template("\\{\\{0\\}\\}", .{});
    t_expectEqual(tmpl.fragments.len, 1);
    expectFragment(tmpl.fragments[0], .literal, "{{0}}");
}

test "named" {
    {
        const text = "{{name}}";
        const tmpl = Template(text, .{});
        t_expectEqual(tmpl.fragments.len, 1);
        expectFragment(tmpl.fragments[0], .variable, "name");
    }
    {
        const text = "Hi {{name}} at index #{{index}}";
        const tmpl = Template(text, .{ .eval_branch_quota = 8000 });
        t_expectEqual(tmpl.fragments.len, 4);
        expectFragment(tmpl.fragments[0], .literal, "Hi ");
        expectFragment(tmpl.fragments[1], .variable, "name");
        expectFragment(tmpl.fragments[2], .literal, " at index #");
        expectFragment(tmpl.fragments[3], .variable, "index");
        t.expectEqualStrings("Hi zero at index #000", try tmpl.bufPrint(&out_buf, .{ .name = "zero", .index = "000" }));
        const msg = try tmpl.allocPrint(t.allocator, .{ .name = "zero", .index = "000" });
        defer t.allocator.free(msg);
        t.expectEqualStrings("Hi zero at index #000", msg);
    }
}

test "for range" {
    const text =
        \\{{ for(0..2) |index| }}
        \\Hello {{index}}!
        \\{{ end }}
    ;
    const tmpl = Template(text, .{ .eval_branch_quota = 32000 });
    t_expectEqual(tmpl.fragments.len, 1);
    t.expect(tmpl.fragments[0] == .for_range);
    t_expectEqual(tmpl.fragments[0].for_range.start, 0);
    t_expectEqual(tmpl.fragments[0].for_range.end, 2);
    t.expectEqualStrings("index", tmpl.fragments[0].for_range.capture_name);
    t_expectEqual(tmpl.fragments[0].for_range.body.len, 3);
    expectFragment(tmpl.fragments[0].for_range.body[0], .literal, "\nHello ");
    expectFragment(tmpl.fragments[0].for_range.body[1], .variable, "index");
    expectFragment(tmpl.fragments[0].for_range.body[2], .literal, "!\n");
    t.expectEqualStrings("\nHello 0!\n\nHello 1!\n", try tmpl.bufPrint(&out_buf, .{}));
    // const msg = try tmpl.allocPrint(t.allocator, .{});
    // t.expectEqualStrings("\nHello 0!\n\nHello 1!\n", msg);
}

test "foreach" {
    const text =
        \\{{ for(list) |item, index| }}
        \\Print {{item}} at {{index}}{{ end }}
    ;
    const tmpl = Template(text, .{ .eval_branch_quota = 32000 });
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
    t.expectEqualStrings("\nPrint 84 at 0\nPrint 168 at 1", try tmpl.bufPrint(&out_buf, .{ .list = &[_]u8{ 84, 168 } }));
}
