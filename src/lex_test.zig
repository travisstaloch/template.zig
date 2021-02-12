const std = @import("std");
const t = std.testing;
usingnamespace @import("lex.zig");
usingnamespace @import("util.zig");

// Make the types prettyprint.
const itemName = std.ComptimeStringMap(ItemType, .{
    .{ .err, "error" },
    .{ .bool, "bool" },
    .{ .char, "char" },
    .{ .char_constant, "charconst" },
    .{ .complex, "complex" },
    .{ .declare, ":=" },
    .{ .EOF, "EOF" },
    .{ .field, "field" },
    .{ .identifier, "identifier" },
    .{ .left_delim, "left delim" },
    .{ .left_paren, "(" },
    .{ .number, "number" },
    .{ .pipe, "pipe" },
    .{ .raw_string, "raw string" },
    .{ .right_delim, "right delim" },
    .{ .right_paren, ")" },
    .{ .space, "space" },
    .{ .string, "string" },
    .{ .variable, "variable" },

    // keywords
    .{ .dot, "." },
    .{ .block, "block" },
    .{ .define, "define" },
    .{ .else_, "else" },
    .{ .if_, "if" },
    .{ .end, "end" },
    .{ .nil, "nil" },
    .{ .range, "range" },
    .{ .template, "template" },
    .{ .with, "with" },
});

const lexTest = struct {
    name: string,
    input: string,
    items: []const Item,
    pub fn init(name: string, input: string, items: []const Item) lexTest {
        return .{ .name = name, .input = input, .items = items };
    }
    pub fn format(value: lexTest, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("name {s} input {s} items {}\n", .{ value.name, value.input, value.items });
    }
};

fn mkItem(typ: ItemType, text: string) Item {
    return Item{
        .typ = typ,
        .val = text,
        .pos = 0,
        .line = 0,
    };
}

const tDot = mkItem(.dot, ".");
const tBlock = mkItem(.block, "block");
const tEOF = mkItem(.EOF, "");
const tFor = mkItem(.identifier, "for");
const tLeft = mkItem(.left_delim, "{{");
const tLpar = mkItem(.left_paren, "(");
const tPipe = mkItem(.pipe, "|");
const tQuote = mkItem(.string,
    \\"abc \n\t\" "
);
const tRange = mkItem(.range, "range");
const tRight = mkItem(.right_delim, "}}");
const tRpar = mkItem(.right_paren, ")");
const tSpace = mkItem(.space, " ");
const raw =
    \\`abc\n\t\" `
;
const rawNL =
    \\`now is{{
    \\}}the time`
;
const tRawQuote = mkItem(.raw_string, raw);
const tRawQuoteNL = mkItem(.raw_string, rawNL);

const lexTests = [_]lexTest{
    lexTest.init("empty", "", &[_]Item{tEOF}),
    lexTest.init("spaces", " \t\n", &[_]Item{ mkItem(.text, " \t\n"), tEOF }),
    lexTest.init("text", "now is the time", &[_]Item{ mkItem(.text, "now is the time"), tEOF }),
    lexTest.init(
        "text with comment",
        "hello-{{/* this is a comment */}}-world",
        &[_]Item{
            mkItem(.text, "hello-"),
            mkItem(.text, "-world"),
            tEOF,
        },
    ),
    lexTest.init(
        "punctuation",
        "{{,@% }}",
        &[_]Item{
            tLeft,
            mkItem(.char, ","),
            mkItem(.char, "@"),
            mkItem(.char, "%"),
            tSpace,
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "parens",
        "{{((3))}}",
        &[_]Item{
            tLeft,
            tLpar,
            tLpar,
            mkItem(.number, "3"),
            tRpar,
            tRpar,
            tRight,
            tEOF,
        },
    ),
    lexTest.init("empty action", "{{}}", &[_]Item{ tLeft, tRight, tEOF }),
    lexTest.init("for", "{{for}}", &[_]Item{ tLeft, tFor, tRight, tEOF }),
    lexTest.init(
        "block",
        \\{{block "foo" .}}
    ,
        &[_]Item{
            tLeft,  tBlock, tSpace,
            mkItem(.string,
                \\"foo"
            ),
            tSpace, tDot,   tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "quote",
        \\{{"abc \n\t\" "}}
    ,
        &[_]Item{ tLeft, tQuote, tRight, tEOF },
    ),
    lexTest.init("raw quote", "{{" ++ raw ++ "}}", &[_]Item{ tLeft, tRawQuote, tRight, tEOF }),
    lexTest.init("raw quote with newline", "{{" ++ rawNL ++ "}}", &[_]Item{ tLeft, tRawQuoteNL, tRight, tEOF }),
    lexTest.init(
        "numbers",
        "{{1 02 0x14 0X14 -7.2i 1e3 1E3 +1.2e-4 4.2i 1+2i 1_2 0x1.e_fp4 0X1.E_FP4}}",
        &[_]Item{
            tLeft,
            mkItem(.number, "1"),
            tSpace,
            mkItem(.number, "02"),
            tSpace,
            mkItem(.number, "0x14"),
            tSpace,
            mkItem(.number, "0X14"),
            tSpace,
            mkItem(.number, "-7.2i"),
            tSpace,
            mkItem(.number, "1e3"),
            tSpace,
            mkItem(.number, "1E3"),
            tSpace,
            mkItem(.number, "+1.2e-4"),
            tSpace,
            mkItem(.number, "4.2i"),
            tSpace,
            mkItem(.complex, "1+2i"),
            tSpace,
            mkItem(.number, "1_2"),
            tSpace,
            mkItem(.number, "0x1.e_fp4"),
            tSpace,
            mkItem(.number, "0X1.E_FP4"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "characters",
        \\{{'a' '\n' '\'' '\\' '\u00FF' '\xFF' '本'}}
    ,
        &[_]Item{
            tLeft,
            mkItem(.char_constant,
                \\'a'
            ),
            tSpace,

            mkItem(.char_constant,
                \\'\n'
            ),
            tSpace,
            mkItem(.char_constant,
                \\'\''
            ),
            tSpace,
            mkItem(.char_constant,
                \\'\\'
            ),
            tSpace,
            mkItem(.char_constant,
                \\'\u00FF'
            ),
            tSpace,
            mkItem(.char_constant,
                \\'\xFF'
            ),
            tSpace,
            mkItem(.char_constant,
                \\'本'
            ),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "bools",
        "{{true false}}",
        &[_]Item{
            tLeft,
            mkItem(.bool, "true"),
            tSpace,
            mkItem(.bool, "false"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "dot",
        "{{.}}",
        &[_]Item{
            tLeft,
            tDot,
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "nil",
        "{{nil}}",
        &[_]Item{
            tLeft,
            mkItem(.nil, "nil"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "dots",
        "{{.x . .2 .x.y.z}}",
        &[_]Item{
            tLeft,
            mkItem(.field, ".x"),
            tSpace,
            tDot,
            tSpace,
            mkItem(.number, ".2"),
            tSpace,
            mkItem(.field, ".x"),
            mkItem(.field, ".y"),
            mkItem(.field, ".z"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "keywords",
        "{{range if else end with}}",
        &[_]Item{
            tLeft,
            mkItem(.range, "range"),
            tSpace,
            mkItem(.if_, "if"),
            tSpace,
            mkItem(.else_, "else"),
            tSpace,
            mkItem(.end, "end"),
            tSpace,
            mkItem(.with, "with"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "variables",
        "{{$c := printf $ $hello $23 $ $var.field .Method}}",
        &[_]Item{
            tLeft,
            mkItem(.variable, "$c"),
            tSpace,
            mkItem(.declare, ":="),
            tSpace,
            mkItem(.identifier, "printf"),
            tSpace,
            mkItem(.variable, "$"),
            tSpace,
            mkItem(.variable, "$hello"),
            tSpace,
            mkItem(.variable, "$23"),
            tSpace,
            mkItem(.variable, "$"),
            tSpace,
            mkItem(.variable, "$var"),
            mkItem(.field, ".field"),
            tSpace,
            mkItem(.field, ".Method"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "variable invocation",
        "{{$x 23}}",
        &[_]Item{
            tLeft,
            mkItem(.variable, "$x"),
            tSpace,
            mkItem(.number, "23"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "pipeline",
        \\intro {{echo hi 1.2 |noargs|args 1 "hi"}} outro
    ,
        &[_]Item{
            mkItem(.text, "intro "),
            tLeft,
            mkItem(.identifier, "echo"),
            tSpace,
            mkItem(.identifier, "hi"),
            tSpace,
            mkItem(.number, "1.2"),
            tSpace,
            tPipe,
            mkItem(.identifier, "noargs"),
            tPipe,
            mkItem(.identifier, "args"),
            tSpace,
            mkItem(.number, "1"),
            tSpace,
            mkItem(.string,
                \\"hi"
            ),
            tRight,
            mkItem(.text, " outro"),
            tEOF,
        },
    ),
    lexTest.init(
        "declaration",
        "{{$v := 3}}",
        &[_]Item{
            tLeft,
            mkItem(.variable, "$v"),
            tSpace,
            mkItem(.declare, ":="),
            tSpace,
            mkItem(.number, "3"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "2 declarations",
        "{{$v , $w := 3}}",
        &[_]Item{
            tLeft,
            mkItem(.variable, "$v"),
            tSpace,
            mkItem(.char, ","),
            tSpace,
            mkItem(.variable, "$w"),
            tSpace,
            mkItem(.declare, ":="),
            tSpace,
            mkItem(.number, "3"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "field of parenthesized expression",
        "{{(.X).Y}}",
        &[_]Item{
            tLeft,
            tLpar,
            mkItem(.field, ".X"),
            tRpar,
            mkItem(.field, ".Y"),
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "trimming spaces before and after",
        "hello- {{- 3 -}} -world",
        &[_]Item{
            mkItem(.text, "hello-"),
            tLeft,
            mkItem(.number, "3"),
            tRight,
            mkItem(.text, "-world"),
            tEOF,
        },
    ),
    lexTest.init(
        "trimming spaces before and after comment",
        "hello- {{- /* hello */ -}} -world",
        &[_]Item{
            mkItem(.text, "hello-"),
            mkItem(.text, "-world"),
            tEOF,
        },
    ),
    // errors
    lexTest.init(
        "badchar",
        "#{{\x01}}",
        &[_]Item{
            mkItem(.text, "#"),
            tLeft,
            mkItem(.err, "unrecognized character in action"),
        },
    ),
    lexTest.init(
        "unclosed action",
        "{{\n}}",
        &[_]Item{
            tLeft,
            mkItem(.err, "unclosed action"),
        },
    ),
    lexTest.init(
        "EOF in action",
        "{{range",
        &[_]Item{
            tLeft,
            tRange,
            mkItem(.err, "unclosed action"),
        },
    ),
    lexTest.init(
        "unclosed quote",
        "{{\"\n\"}}",
        &[_]Item{
            tLeft,
            mkItem(.err, "unterminated quoted string"),
        },
    ),
    lexTest.init(
        "unclosed raw quote",
        "{{`xx}}",
        &[_]Item{
            tLeft,
            mkItem(.err, "unterminated raw quoted string"),
        },
    ),
    lexTest.init(
        "unclosed char constant",
        "{{'\n}}",
        &[_]Item{
            tLeft,
            mkItem(.err, "unterminated character constant"),
        },
    ),
    lexTest.init(
        "bad number",
        "{{3k}}",
        &[_]Item{
            tLeft,
            mkItem(.err,
                \\bad number syntax: "3k"
            ),
        },
    ),
    lexTest.init(
        "unclosed paren",
        "{{(3}}",
        &[_]Item{
            tLeft,
            tLpar,
            mkItem(.number, "3"),
            mkItem(.err,
                \\unclosed left paren
            ),
        },
    ),
    lexTest.init(
        "extra right paren",
        "{{3)}}",
        &[_]Item{
            tLeft,
            mkItem(.number, "3"),
            tRpar,
            mkItem(.err,
                \\unexpected right paren ')'
            ),
        },
    ),

    // Fixed bugs
    // Many elements in an action blew the lookahead until
    // we made lexInsideAction not loop.
    lexTest.init(
        "long pipeline deadlock",
        "{{|||||}}",
        &[_]Item{
            tLeft,
            tPipe,
            tPipe,
            tPipe,
            tPipe,
            tPipe,
            tRight,
            tEOF,
        },
    ),
    lexTest.init(
        "text with bad comment",
        "hello-{{/*/}}-world",
        &[_]Item{
            mkItem(.text, "hello-"),
            mkItem(.err,
                \\unclosed comment
            ),
        },
    ),
    lexTest.init(
        "text with comment close separated from delim",
        "hello-{{/* */ }}-world",
        &[_]Item{
            mkItem(.text, "hello-"),
            mkItem(.err,
                \\comment ends before closing delimiter
            ),
        },
    ),
    // This one is an error that we can't catch because it breaks templates with
    // minimized JavaScript. Should have fixed it before Go 1.1.
    lexTest.init(
        "unmatched right delimiter",
        "hello-{.}}-world",
        &[_]Item{
            mkItem(.text, "hello-{.}}-world"),
            tEOF,
        },
    ),
};

// collect gathers the emitted items into a slice.
fn collect(lt: lexTest, left: string, comptime right: string) ![]const Item {
    var items_buf: [0x10]Item = undefined;
    var l = Lexer.init(lt.name, lt.input, left, right, &items_buf);
    var items = std.ArrayList(Item).init(t.allocator);

    while (l.nextItem()) |itm| {
        std.log.debug("collect itm {} l.items.len {}", .{ itm, l.items.len });
        try items.append(itm);
        if (itm.typ == .EOF or itm.typ == .err) {
            break;
        }
    }
    return items.toOwnedSlice();
}

fn equal(it1: []const Item, expected: []const Item, checkPos: bool) bool {
    var result = true;
    if (it1.len != expected.len) {
        std.log.err("len mismatch got {} expecting {}\n", .{ it1.len, expected.len });
        result = false;
    }
    for (it1) |_, k| {
        if (expected.len < k) break;
        if (it1[k].typ != expected[k].typ) {
            std.log.err("type mismatch got {} expecting {}\n", .{ it1[k].typ, expected[k].typ });
            result = false;
        }
        if (!std.mem.startsWith(u8, it1[k].val, expected[k].val)) {
            std.log.err("var mismatch got {s} expecting {s}\n", .{ it1[k].val, expected[k].val });
            result = false;
        }
        if (checkPos and it1[k].pos != expected[k].pos) {
            std.log.err("pos mismatch got {} expecting {}\n", .{ it1[k].pos, expected[k].pos });
            result = false;
        }
        if (checkPos and it1[k].line != expected[k].line) {
            std.log.err("line mismatch got {} expecting {}\n", .{ it1[k].line, expected[k].line });
            result = false;
        }
    }
    return result;
}

test "lex" {
    // std.testing.log_level = .debug;
    inline for (lexTests) |tst| {
        const items = try collect(tst, "", "");
        defer t.allocator.free(items);
        if (!equal(items, tst.items, false)) {
            std.log.err("test '{s}': got\n\t{}\nexpected\n\t{}", .{ tst.name, items, tst.items });
            t.expect(false);
        }
    }
}

// Some easy cases from above, but with delimiters $$ and @@
const lexDelimTests = [_]lexTest{
    lexTest.init("punctuation", "$$,@%{{}}@@", &[_]Item{
        tLeftDelim,
        mkItem(.char, ","),
        mkItem(.char, "@"),
        mkItem(.char, "%"),
        mkItem(.char, "{"),
        mkItem(.char, "{"),
        mkItem(.char, "}"),
        mkItem(.char, "}"),
        tRightDelim,
        tEOF,
    }),
    lexTest.init("empty action", "$$@@", &[_]Item{ tLeftDelim, tRightDelim, tEOF }),
    lexTest.init("for", "$$for@@", &[_]Item{ tLeftDelim, tFor, tRightDelim, tEOF }),
    lexTest.init("quote",
        \\$$"abc \n\t\" "@@
    , &[_]Item{ tLeftDelim, tQuote, tRightDelim, tEOF }),
    lexTest.init("raw quote", "$$" ++ raw ++ "@@", &[_]Item{ tLeftDelim, tRawQuote, tRightDelim, tEOF }),
};

const tLeftDelim = mkItem(.left_delim, "$$");
const tRightDelim = mkItem(.right_delim, "@@");

test "delims" {
    for (lexDelimTests) |tst| {
        const items = try collect(tst, "$$", "@@");
        defer t.allocator.free(items);
        if (!equal(items, tst.items, false)) {
            std.log.err("test '{s}': got\n\t{}\nexpected\n\t{}", .{ tst.name, items, tst.items });
            t.expect(false);
        }
    }
}

const lexPosTests = [_]lexTest{
    lexTest.init("empty", "", &[_]Item{Item.init(.EOF, 0, "", 1)}),
    lexTest.init("punctuation", "{{,@%#}}", &[_]Item{
        Item.init(.left_delim, 0, "{{", 1),
        Item.init(.char, 2, ",", 1),
        Item.init(.char, 3, "@", 1),
        Item.init(.char, 4, "%", 1),
        Item.init(.char, 5, "#", 1),
        Item.init(.right_delim, 6, "}}", 1),
        Item.init(.EOF, 8, "", 1),
    }),
    lexTest.init("sample", "0123{{hello}}xyz", &[_]Item{
        Item.init(.text, 0, "0123", 1),
        Item.init(.left_delim, 4, "{{", 1),
        Item.init(.identifier, 6, "hello", 1),
        Item.init(.right_delim, 11, "}}", 1),
        Item.init(.text, 13, "xyz", 1),
        Item.init(.EOF, 16, "", 1),
    }),
    lexTest.init("trimafter", "{{x -}}\n{{y}}", &[_]Item{
        Item.init(.left_delim, 0, "{{", 1),
        Item.init(.identifier, 2, "x", 1),
        Item.init(.right_delim, 5, "}}", 1),
        Item.init(.left_delim, 8, "{{", 2),
        Item.init(.identifier, 10, "y", 2),
        Item.init(.right_delim, 11, "}}", 2),
        Item.init(.EOF, 13, "", 2),
    }),
    lexTest.init("trimbefore", "{{x}}\n{{- y}}", &[_]Item{
        Item.init(.left_delim, 0, "{{", 1),
        Item.init(.identifier, 2, "x", 1),
        Item.init(.right_delim, 3, "}}", 1),
        Item.init(.left_delim, 6, "{{", 2),
        Item.init(.identifier, 10, "y", 2),
        Item.init(.right_delim, 11, "}}", 2),
        Item.init(.EOF, 13, "", 2),
    }),
};

// The other tests don't check position, to make the test cases easier to construct.
// This one does.
test "pos" {
    for (lexPosTests) |tst| {
        const items = try collect(tst, "", "");
        defer t.allocator.free(items);
        if (!equal(items, tst.items, true)) {
            std.log.err("test '{s}': got\n\t{}\nexpected\n\t{}", .{ tst.name, items, tst.items });
            t.expect(false);
        }
    }
}
