const std = @import("std");
const template = @import("template.zig");
const Template = template.Template;
const lex = @import("lex.zig");
const Tree = @import("Tree.zig").TreeOpts;
const Options = @import("Options.zig");

const ParseTest = struct {
    name: []const u8,
    input: []const u8,
    ok: bool,
    result: []const u8, // what the user would see in an error message.
    pub fn init(name: []const u8, input: []const u8, ok_: bool, result: []const u8) ParseTest {
        return .{ .name = name, .input = input, .ok = ok_, .result = result };
    }
};

const ok = true;
const not_ok = false;

const parse_tests = [_]ParseTest{
    ParseTest.init("empty", "", ok, ""),
    ParseTest.init("comment", "{{/*\n\n\n*/}}", ok,
        \\
    ),
    ParseTest.init("spaces", " \t\n", ok, " \t\n"),
    ParseTest.init("text",
        \\"some text"
    , ok,
        \\"some text"
    ),
    ParseTest.init("emptyAction", "{{}}", not_ok,
        \\{{}}
    ),
    ParseTest.init("field", "{{.X}}", ok,
        \\{{.X}}
    ),
    ParseTest.init("simple command", "{{printf}}", ok,
        \\{{printf}}
    ),
    ParseTest.init("$ invocation", "{{$}}", ok, "{{$}}"),
    ParseTest.init("variable invocation", "{{with $x := 3}}{{$x 23}}{{end}}", ok, "{{with $x := 3}}{{$x 23}}{{end}}"),
    ParseTest.init("variable with fields", "{{$.I}}", ok, "{{$.I}}"),
    ParseTest.init("multi-word command", "{{printf \"%d\" 23}}", ok, "{{printf \"%d\" 23}}"),
    ParseTest.init("pipeline", "{{.X | .Y}}", ok,
        \\{{.X | .Y}}
    ),
    ParseTest.init("pipeline with decl", "{{$x := .X | .Y}}", ok,
        \\{{$x := .X | .Y}}
    ),
    ParseTest.init("nested pipeline", "{{.X (.Y .Z) (.A | .B .C) (.E)}}", ok,
        \\{{.X (.Y .Z) (.A | .B .C) (.E)}}
    ),
    ParseTest.init("field applied to parentheses", "{{(.Y .Z).Field}}", ok,
        \\{{(.Y .Z).Field}}
    ),
    ParseTest.init("simple if", "{{if .X}}hello{{end}}", ok,
        \\{{if .X}}hello{{end}}
    ),
    ParseTest.init("if with else", "{{if .X}}true{{else}}false{{end}}", ok,
        \\{{if .X}}true{{else}}false{{end}}
    ),
    ParseTest.init("if with else if", "{{if .X}}true{{else if .Y}}false{{end}}", ok,
        \\{{if .X}}true{{else}}{{if .Y}}false{{end}}{{end}}
    ),
    ParseTest.init("if else chain", "+{{if .X}}X{{else if .Y}}Y{{else if .Z}}Z{{end}}+", ok,
        \\+{{if .X}}X{{else}}{{if .Y}}Y{{else}}{{if .Z}}Z{{end}}{{end}}{{end}}+
    ),
    ParseTest.init("simple range", "{{range .X}}hello{{end}}", ok,
        \\{{range .X}}hello{{end}}
    ),
    ParseTest.init("chained field range", "{{range .X.Y.Z}}hello{{end}}", ok,
        \\{{range .X.Y.Z}}hello{{end}}
    ),
    ParseTest.init("nested range", "{{range .X}}hello{{range .Y}}goodbye{{end}}{{end}}", ok,
        \\{{range .X}}hello{{range .Y}}goodbye{{end}}{{end}}
    ),
    ParseTest.init("range with else", "{{range .X}}true{{else}}false{{end}}", ok,
        \\{{range .X}}true{{else}}false{{end}}
    ),
    ParseTest.init("range over pipeline", "{{range .X|.M}}true{{else}}false{{end}}", ok,
        \\{{range .X | .M}}true{{else}}false{{end}}
    ),
    ParseTest.init("range []int", "{{range .SI}}{{.}}{{end}}", ok,
        \\{{range .SI}}{{.}}{{end}}
    ),
    ParseTest.init("range 1 var", "{{range $x := .SI}}{{.}}{{end}}", ok,
        \\{{range $x := .SI}}{{.}}{{end}}
    ),
    ParseTest.init("range 2 vars", "{{range $x, $y := .SI}}{{.}}{{end}}", ok,
        \\{{range $x, $y := .SI}}{{.}}{{end}}
    ),
    ParseTest.init("constants", "{{range .SI 1 -3.2i true false 'a' nil}}{{end}}", ok,
        \\{{range .SI 1 -3.2i true false 'a' nil}}{{end}}
    ),
    ParseTest.init("template", "{{template \"x\"}}", ok,
        \\{{template "x"}}
    ),
    ParseTest.init("template with arg", "{{template \"x\" .Y}}", ok,
        \\{{template "x" .Y}}
    ),
    ParseTest.init("with", "{{with .X}}hello{{end}}", ok,
        \\{{with .X}}hello{{end}}
    ),
    ParseTest.init("with with else", "{{with .X}}hello{{else}}goodbye{{end}}", ok,
        \\{{with .X}}hello{{else}}goodbye{{end}}
    ),
    // Trimming spaces.
    ParseTest.init("trim left", "x \r\n\t{{- 3}}", ok,
        \\x{{3}}
    ),
    ParseTest.init("trim right", "{{3 -}}\n\n\ty", ok,
        \\{{3}}y
    ),
    ParseTest.init("trim left and right", "x \r\n\t{{- 3 -}}\n\n\ty", ok,
        \\x{{3}}y
    ),
    ParseTest.init("trim with extra spaces", "x\n{{-  3   -}}\ny", ok,
        \\x{{3}}y
    ),
    ParseTest.init("comment trim left", "x \r\n\t{{- /* hi */}}", ok,
        \\x
    ),
    ParseTest.init("comment trim right", "{{/* hi */ -}}\n\n\ty", ok,
        \\y
    ),
    ParseTest.init("comment trim left and right", "x \r\n\t{{- /* */ -}}\n\n\ty", ok,
        \\xy
    ),
    ParseTest.init("block definition",
        \\{{block "foo" .}}hello{{end}}
    , ok,
        \\{{template "foo" .}}
    ),
    // Errors.
    ParseTest.init("unclosed action", "hello{{range", not_ok, ""),
    ParseTest.init("unmatched end", "{{end}}", not_ok, ""),
    ParseTest.init("unmatched else", "{{else}}", not_ok, ""),
    ParseTest.init("unmatched else after if", "{{if .X}}hello{{end}}{{else}}", not_ok, ""),
    ParseTest.init("multiple else", "{{if .X}}1{{else}}2{{else}}3{{end}}", not_ok, ""),
    ParseTest.init("missing end", "hello{{range .x}}", not_ok, ""),
    ParseTest.init("missing end after else", "hello{{range .x}}{{else}}", not_ok, ""),
    ParseTest.init("undefined function", "hello{{undefined}}", not_ok, ""),
    ParseTest.init("undefined variable", "{{$x}}", not_ok, ""),
    ParseTest.init("variable undefined after end", "{{with $x := 4}}{{end}}{{$x}}", not_ok, ""),
    ParseTest.init("variable undefined in template", "{{template $v}}", not_ok, ""),
    ParseTest.init("declare with field", "{{with $x.Y := 4}}{{end}}", not_ok, ""),
    ParseTest.init("template with field ref", "{{template .X}}", not_ok, ""),
    ParseTest.init("template with var", "{{template $v}}", not_ok, ""),
    ParseTest.init("invalid punctuation", "{{printf 3, 4}}", not_ok, ""),
    ParseTest.init("multidecl outside range", "{{with $v, $u := 3}}{{end}}", not_ok, ""),
    ParseTest.init("too many decls in range", "{{range $u, $v, $w := 3}}{{end}}", not_ok, ""),
    ParseTest.init("dot applied to parentheses", "{{printf (printf .).}}", not_ok, ""),
    ParseTest.init("adjacent args", "{{printf 3\"$1\"}}", not_ok, ""),
    ParseTest.init("adjacent args with .", "{{printf \"$1\".}}", not_ok, ""),
    ParseTest.init("extra end after if", "{{if .X}}a{{else if .Y}}b{{end}}{{end}}", not_ok, ""),
    // Other kinds of assignments and operators aren't available yet.
    ParseTest.init("bug0a", "{{$x := 0}}{{$x}}", ok, "{{$x := 0}}{{$x}}"),
    ParseTest.init("bug0b", "{{$x += 1}}{{$x}}", not_ok, ""),
    ParseTest.init("bug0c", "{{$x ! 2}}{{$x}}", not_ok, ""),
    ParseTest.init("bug0d", "{{$x % 3}}{{$x}}", not_ok, ""),
    // Check the parse fails for := rather than comma.
    ParseTest.init("bug0e", "{{range $x := $y := 3}}{{end}}", not_ok, ""),
    // Another bug: variable read must ignore following punctuation.
    ParseTest.init("bug1a", "{{$x:=.}}{{$x!2}}", not_ok, ""), // ! is just illegal here.
    ParseTest.init("bug1b", "{{$x:=.}}{{$x+2}}", not_ok, ""), // $x+2 should not parse as ($x) (+2).
    ParseTest.init("bug1c", "{{$x:=.}}{{$x +2}}", ok, "{{$x := .}}{{$x +2}}"), // It's OK with a space.
    // dot following a literal value
    ParseTest.init("dot after integer", "{{1.E}}", not_ok, ""),
    ParseTest.init("dot after float", "{{0.1.E}}", not_ok, ""),
    ParseTest.init("dot after boolean", "{{true.E}}", not_ok, ""),
    ParseTest.init("dot after char", "{{'a'.any}}", not_ok, ""),
    ParseTest.init("dot after string",
        \\{{"hello".guys}}
    , not_ok, ""),
    ParseTest.init("dot after dot", "{{..E}}", not_ok, ""),
    ParseTest.init("dot after nil", "{{nil.E}}", not_ok, ""),
    // Wrong pipeline
    ParseTest.init("wrong pipeline dot", "{{12|.}}", not_ok, ""),
    ParseTest.init("wrong pipeline number", "{{.|12|printf}}", not_ok, ""),
    ParseTest.init("wrong pipeline string", "{{.|printf|\"error\"}}", not_ok, ""),
    ParseTest.init("wrong pipeline char", "{{12|printf|'e'}}", not_ok, ""),
    ParseTest.init("wrong pipeline boolean", "{{.|true}}", not_ok, ""),
    ParseTest.init("wrong pipeline nil", "{{'c'|nil}}", not_ok, ""),
    ParseTest.init("empty pipeline",
        \\{{printf "%d" ( ) }}
    , not_ok, ""),
    // Missing pipeline in block
    ParseTest.init("block definition",
        \\{{block "foo"}}hello{{end}}
    , not_ok, ""),
};

const t = std.testing;
const options = Options{ .is_comptime = false, .compile_log = false };

test "parse" {
    // t.log_level = .debug;
    inline for (parse_tests) |pt, i| {
        // const tmpl = Template(parse_test.input, .{ .eval_branch_quota = 128000 });
        if ( //
        true and
            // i == 14 and
            pt.ok //
        ) {
            // @compileLog(pt.name, pt.input);
            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            var arena = std.heap.ArenaAllocator.init(&gpa.allocator);
            const local_options = comptime options.with(.{ .eval_branch_quota = 64000 });

            var tree = if (local_options.is_comptime)
                comptime Tree(local_options).parse(pt.input, pt.name, "", "", {}, {})
            else
                Tree(local_options).parseAlloc(pt.input, pt.name, "", "", {}, {}, &arena.allocator);
            defer arena.deinit();

            var buf = [1]u8{0} ** 0x1000;
            var fbs = std.io.fixedBufferStream(&buf);
            for (tree.root.root) |node| try fbs.writer().print("{}", .{node});

            // std.debug.print("printed {s}\n", .{std.mem.spanZ(&buf)});
            const written = fbs.getWritten();
            if (!std.mem.eql(u8, pt.result, written)) {
                std.debug.print(
                    \\---{}:{s}---
                    \\title:     {s}
                    \\ok:        {}
                    \\input:    '{s}'
                    \\print:    '{s}'
                    \\expected: '{s}'
                    \\
                    \\
                , .{
                    i,
                    if (tree.root.root.len > 0) @tagName(tree.root.root[0]) else "",
                    pt.name,
                    pt.ok,
                    pt.input,
                    fbs.getWritten(),
                    pt.result,
                });
                t.expectEqualStrings(pt.result, written);
            }
        }
    }
}

fn parseText(comptime opts: Options, comptime text: []const u8, allocator: *std.mem.Allocator) Tree(opts) {
    return if (opts.is_comptime)
        Tree(opts).parse(text, "", "", "", {}, {})
    else
        Tree(opts).parseAlloc(text, "", "", "", {}, {}, allocator);
}

test "chain" {
    var arena = std.heap.ArenaAllocator.init(t.allocator);
    // t.log_level = .debug;
    const tree = parseText(options, "{{(.Y .Z).Field}}", &arena.allocator);

    t.expectEqual(tree.root.root.len, 1);
    t.expect(tree.root.root[0] == .action);
    t.expectEqual(tree.root.root[0].action.cmds.len, 1);
    t.expectEqual(tree.root.root[0].action.cmds[0].args.len, 1);
    t.expect(tree.root.root[0].action.cmds[0].args[0] == .chain);
    t.expect(tree.root.root[0].action.cmds[0].args[0].chain.node.* == .pipeline);
    t.expectEqual(tree.root.root[0].action.cmds[0].args[0].chain.node.*.pipeline.cmds.len, 1);
    t.expectEqual(tree.root.root[0].action.cmds[0].args[0].chain.node.*.pipeline.cmds[0].args.len, 2);
    t.expect(tree.root.root[0].action.cmds[0].args[0].chain.node.*.pipeline.cmds[0].args[0] == .field);
    t.expectEqualStrings("Y", tree.root.root[0].action.cmds[0].args[0].chain.node.*.pipeline.cmds[0].args[0].field[0]);
    t.expect(tree.root.root[0].action.cmds[0].args[0].chain.node.*.pipeline.cmds[0].args[1] == .field);
    t.expectEqualStrings("Z", tree.root.root[0].action.cmds[0].args[0].chain.node.*.pipeline.cmds[0].args[1].field[0]);
    t.expectEqual(tree.root.root[0].action.cmds[0].args[0].chain.node.*.pipeline.decls.len, 0);
    t.expectEqual(tree.root.root[0].action.cmds[0].args[0].chain.field.len, 1);
    t.expectEqualStrings("Field", tree.root.root[0].action.cmds[0].args[0].chain.field[0]);
    defer arena.deinit();
}

test "range fields" {
    // t.log_level = .debug;
    var arena = std.heap.ArenaAllocator.init(t.allocator);
    defer arena.deinit();
    const tree = parseText(options, "{{range .X.Y.Z}}hello{{end}}", &arena.allocator);
    t.expectEqual(tree.root.root.len, 1);
    t.expect(tree.root.root[0] == .range);
    t.expectEqual(tree.root.root[0].range.pipeline.?.cmds.len, 1);
    t.expectEqual(tree.root.root[0].range.pipeline.?.cmds[0].args.len, 1);
    t.expect(tree.root.root[0].range.pipeline.?.cmds[0].args[0] == .field);
    t.expectEqual(tree.root.root[0].range.pipeline.?.cmds[0].args[0].field.len, 3);
}

test "nested pipeline" {
    var arena = std.heap.ArenaAllocator.init(t.allocator);
    defer arena.deinit();

    // const tree = parseText(options, "{{.X (.Y .Z) (.A | .B .C) (.E)}}", &arena.allocator);
    const tree = parseText(options, parse_tests[13].input, &arena.allocator);
    t.expectEqual(tree.root.root.len, 1);
    t.expect(tree.root.root[0] == .action);
    t.expectEqual(tree.root.root[0].action.cmds.len, 1);
    t.expectEqual(tree.root.root[0].action.cmds[0].args.len, 4);
    t.expect(tree.root.root[0].action.cmds[0].args[0] == .field);
    t.expect(tree.root.root[0].action.cmds[0].args[1] == .pipeline);
    t.expect(tree.root.root[0].action.cmds[0].args[2] == .pipeline);
    t.expect(tree.root.root[0].action.cmds[0].args[3] == .pipeline);
}
