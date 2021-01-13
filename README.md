# template.zig
comptime templates in zig

# usage
```console
$ zig build test
All 12 tests passed.
$ zig build # results in zig-cache/lib/libtemplate.zig.a
```

from end of [tests.zig](src/tests.zig)
```zig
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
```

# resources
- [fengb/zig-tmpl](https://github.com/fengb/zig-tmpl)
- [mlarouche/stringtime](https://github.com/mlarouche/stringtime)
- [zig/lib/std/fmt.zig](https://github.com/ziglang/zig/blob/master/lib/std/fmt.zig)
- [golang/text/template/](https://golang.org/src/text/template/)