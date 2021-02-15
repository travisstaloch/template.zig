const std = @import("std");
usingnamespace @import("util.zig");
const RingBuffer = @import("ring_buffer.zig").RingBuffer;
const Options = @import("Options.zig");

pub const ItemType = enum {
    err, // error occurred; value is text of error
    bool, // boolean constant
    char, // printable ASCII character; grab bag for comma etc.
    char_constant, // character constant
    complex, // complex constant (1+2i); imaginary is just a number
    assign, // equals ('=') introducing an assignment
    declare, // colon-equals (':=') introducing a declaration
    EOF,
    field, // alphanumeric identifier starting with '.'
    identifier, // alphanumeric identifier not starting with '.'
    left_delim, // left action delimiter
    left_paren, // '(' inside action
    number, // simple number, including imaginary
    pipe, // pipe symbol
    raw_string, // raw quoted string (includes quotes)
    right_delim, // right action delimiter
    right_paren, // ')' inside action
    space, // run of spaces separating arguments
    string, // quoted string (includes quotes)
    text, // plain text
    variable, // variable starting with '$', such as '$' or  '$1' or '$hello'
    // Keywords appear after all the rest.
    keyword, // used only to delimit the keywords
    block, // block keyword
    dot, // the cursor, spelled '.'
    define, // define keyword
    else_, // else keyword
    end, // end keyword
    if_, // if keyword
    nil, // the untyped nil constant, easiest to treat as a keyword
    range, // range keyword
    template, // template keyword
    with, // with keyword
};
pub const Pos = usize;
pub const Item = struct {
    typ: ItemType,
    pos: Pos,
    val: string,
    line: usize,
    pub fn init(typ: ItemType, pos: Pos, val: string, line: usize) Item {
        return .{
            .typ = typ,
            .pos = pos,
            .val = val,
            .line = line,
        };
    }

    pub fn format(value: Item, comptime fmt: string, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("typ: {}, pos:line {d}-{d} val: '{s}'", .{ value.typ, value.pos, value.line, value.val });
    }
};

const key = std.ComptimeStringMap(ItemType, .{
    .{ ".", .dot },
    .{ "block", .block },
    .{ "define", .define },
    .{ "else", .else_ },
    .{ "end", .end },
    .{ "if", .if_ },
    .{ "range", .range },
    .{ "nil", .nil },
    .{ "template", .template },
    .{ "with", .with },
});

const leftTrimMarker = "- ";
const rightTrimMarker = " -";

pub fn Lexer(comptime options: Options) type {
    return struct {
        name: string, // the name of the input; used only for error reports
        input: std.unicode.Utf8Iterator,
        leftDelim: string, // start of action
        rightDelim: string, // end of action
        rightDelimCp: u21, // end of action codepoint
        trimRightDelim: string, // end of action with trim marker
        start: Pos, // start position of this item
        width: Pos, // width of last rune read from input
        // items: std.fifo.LinearFifo(Item, .{ .Static = fifo_size }), // buffer of scanned items
        // items: []Item, // buffer of scanned items
        items: RingBuffer(Item),
        // items_count: usize, // count of scanned items
        parenDepth: int, // nesting depth of ( ) exprs
        line: int, // 1+number of newlines seen
        startLine: int, // start line of this item
        state: StateFn,

        const eof = @bitCast(u21, @as(i21, -1));
        const Lex = @This();

        // creates a new scanner for the input string.
        pub fn init(name: string, input: string, left_: string, comptime right_: string, items_buf: []Item) @This() {
            var left = left_;
            comptime var right = right_;
            if (left.len == 0) {
                left = leftDelim;
            }
            if (right.len == 0) {
                right = rightDelim;
            }
            debug("lex name '{s}' input '{s}' left '{s}' right '{s}' rightDelimCp '{u}'", .{ name, input, left, right, comptime utf8DecodeRuneInString(right).rune });
            const l = Lex{
                .name = name,
                .input = std.unicode.Utf8Iterator{ .bytes = input, .i = 0 },
                .leftDelim = left,
                .rightDelim = right,
                .rightDelimCp = comptime utf8DecodeRuneInString(right).rune,
                .trimRightDelim = rightTrimMarker ++ right,
                // .items =  std.fifo.LinearFifo(Item, .{ .Static = fifo_size }).init(),
                .items = RingBuffer(Item).init(items_buf),
                // .items_count = 0,
                .line = 1,
                .startLine = 1,
                .start = 0,
                .width = 0,
                .parenDepth = 0,
                .state = .lexText,
            };
            return l;
        }

        pub const debug = options.debug;

        // next returns the next rune in the input.
        fn next(l: *Lex) rune {
            const r = l.input.nextCodepoint() orelse {
                l.width = 0;
                return eof;
            };
            l.width = std.unicode.utf8CodepointSequenceLength(r) catch unreachable;
            if (r == '\n')
                l.line += 1;
            return r;
        }

        // peek returns but does not consume the next rune in the input.
        fn peek(l: *Lex) rune {
            defer l.backup();
            return l.next();
        }

        // backup steps back one rune. Can only be called once per call of next.
        fn backup(l: *Lex) void {
            l.input.i -= l.width;
            // Correct newline count.
            if (l.width == 1 and l.input.bytes[l.input.i] == '\n') {
                l.line -= 1;
            }
        }

        // emit passes an item back to the client.
        fn emit(l: *Lex, t: ItemType) void {
            debug("emit {s}", .{@tagName(t)});
            l.items.tryWrite(Item.init(
                t,
                l.start,
                l.input.bytes[l.start..l.input.i],
                l.startLine,
            )) catch |e| {
                _ = l.errorf("emit error {}", .{@errorName(e)});
            };

            l.start = l.input.i;
            l.startLine = l.line;
        }

        // ignore skips over the pending input before this point.
        fn ignore(l: *Lex) void {
            l.line += std.mem.count(u8, l.input.bytes[l.start..l.input.i], "\n");
            l.start = l.input.i;
            l.startLine = l.line;
        }

        fn containsRune(s: string, r: rune) !bool {
            var buf: [4]u8 = undefined;
            const len = try std.unicode.utf8Encode(r, &buf);
            return std.mem.indexOf(u8, s, buf[0..len]) != null;
        }

        // accept consumes the next rune if it's from the valid set.
        fn accept(l: *Lex, valid: string) bool {
            if (containsRune(valid, l.next()) catch false) {
                return true;
            }

            l.backup();
            return false;
        }

        // acceptRun consumes a run of runes from the valid set.
        fn acceptRun(l: *Lex, valid: string) void {
            while (containsRune(valid, l.next()) catch false) {}
            l.backup();
        }

        var error_buf: [0x1000]u8 = undefined;

        // errorf returns an error token and terminates the scan by passing
        // back a nil pointer that will be the next state, terminating l.nextItem.
        fn errorf(l: *Lex, comptime format: string, args: anytype) StateFn {
            l.items.tryWrite(Item.init(
                .err,
                l.start,
                std.fmt.bufPrint(&error_buf, format, args) catch unreachable,
                l.startLine,
            )) catch |e| {
                std.debug.panic("error while writing error: {s}", .{@errorName(e)});
            };
            return .lexDone;
        }

        // nextItem returns the next item from the input.
        pub fn nextItem(l: *Lex) ?Item {
            debug("nextItem state {} items {}", .{ l.state, l.items.len });
            if (l.items.read()) |i| return i;
            while (l.state != .lexDone) {
                debug("nextItem step state {}", .{l.state});
                l.state = l.step();
                if (l.items.read()) |item| {
                    debug("nextItem item {} items.len {}", .{ item, l.items.len });
                    return item;
                }
            }
            return null;
        }

        // step the state machine for the lexer.
        fn step(l: *Lex) StateFn {
            if (l.state != .lexDone) {
                const next_fn = fn_map[@enumToInt(l.state)];
                return next_fn(l);
            }
            return .lexDone;
        }

        // state functions
        const leftDelim = "{{";
        const rightDelim = "}}";
        const leftComment = "/*";
        const rightComment = "*/";

        const stateFn = fn (*Lex) StateFn;
        const StateFn = enum {
            lexText,
            lexLeftDelim,
            lexComment,
            lexRightDelim,
            lexInsideAction,
            lexSpace,
            lexIdentifier,
            lexField,
            lexVariable,
            lexChar,
            lexNumber,
            lexQuote,
            lexRawQuote,
            lexDone,
        };
        const fn_map = blk: {
            var map: []const stateFn = &[_]stateFn{};
            inline for (std.meta.fields(StateFn)) |f| {
                const field = @field(StateFn, f.name);
                std.debug.assert(map.len == @enumToInt(field));
                if (field != .lexDone)
                    map = map ++ [1]stateFn{@field(@This(), f.name)};
            }
            break :blk map;
        };
        // lexText scans until an opening action delimiter, "{{".
        fn lexText(l: *Lex) StateFn {
            l.width = 0;

            if (std.mem.indexOf(u8, l.input.bytes[l.input.i..], l.leftDelim)) |x| {
                const ldn = l.leftDelim.len;
                l.input.i += x;
                var trimLength: usize = 0;
                if (std.mem.startsWith(u8, l.input.bytes[l.input.i + ldn ..], leftTrimMarker)) {
                    trimLength = rightTrimLength(l.input.bytes[l.start..l.input.i]);
                }
                l.input.i -= trimLength;
                if (l.input.i > l.start) {
                    l.line += std.mem.count(u8, l.input.bytes[l.start..l.input.i], "\n");
                    l.emit(.text);
                }
                l.input.i += trimLength;
                l.ignore();
                return .lexLeftDelim;
            }
            l.input.i = l.input.bytes.len;
            // Correctly reached EOF.
            if (l.input.i > l.start) {
                l.line += std.mem.count(u8, l.input.bytes[l.start..l.input.i], "\n");
                l.emit(.text);
            }
            l.emit(.EOF);
            return .lexDone;
        }

        // rightTrimLength returns the length of the spaces at the end of the string.
        fn rightTrimLength(s: string) Pos {
            return s.len - std.mem.trimRight(u8, s, &std.ascii.spaces).len;
        }

        // atRightDelim reports whether the lexer is at a right delimiter, possibly preceded by a trim marker.
        fn atRightDelim(l: *Lex) [2]bool {
            if (std.mem.startsWith(u8, l.input.bytes[l.input.i..], l.trimRightDelim)) { // with trim marker.
                return .{ true, true };
            }
            if (std.mem.startsWith(u8, l.input.bytes[l.input.i..], l.rightDelim)) { // Without trim marker.
                return .{ true, false };
            }
            return .{ false, false };
        }

        // leftTrimLength returns the length of the spaces at the beginning of the string.
        fn leftTrimLength(s: string) Pos {
            return s.len - std.mem.trimLeft(u8, s, &std.ascii.spaces).len;
        }

        // lexLeftDelim scans the left delimiter, which is known to be present, possibly with a trim marker.
        fn lexLeftDelim(l: *Lex) StateFn {
            l.input.i += l.leftDelim.len;
            const trimSpace = std.mem.startsWith(u8, l.input.bytes[l.input.i..], leftTrimMarker);
            var afterMarker: usize = 0;
            if (trimSpace) {
                afterMarker = leftTrimMarker.len;
            }
            if (std.mem.startsWith(u8, l.input.bytes[l.input.i + afterMarker ..], leftComment)) {
                l.input.i += afterMarker;
                l.ignore();
                return .lexComment;
            }
            l.emit(.left_delim);
            l.input.i += afterMarker;
            l.ignore();
            l.parenDepth = 0;
            return .lexInsideAction;
        }

        // lexComment scans a comment. The left comment marker is known to be present.
        fn lexComment(l: *Lex) StateFn {
            l.input.i += leftComment.len;
            const i = std.mem.indexOf(u8, l.input.bytes[l.input.i..], rightComment) orelse
                return l.errorf("unclosed comment", .{});

            l.input.i += i + rightComment.len;
            const delimtrimSpace = l.atRightDelim();
            const delim = delimtrimSpace[0];
            const trimSpace = delimtrimSpace[1];
            if (!delim) {
                return l.errorf("comment ends before closing delimiter", .{});
            }
            if (trimSpace) {
                l.input.i += leftTrimMarker.len;
            }
            l.input.i += l.rightDelim.len;
            if (trimSpace) {
                l.input.i += leftTrimLength(l.input.bytes[l.input.i..]);
            }
            l.ignore();
            return .lexText;
        }

        // lexRightDelim scans the right delimiter, which is known to be present, possibly with a trim marker.
        fn lexRightDelim(l: *Lex) StateFn {
            const trimSpace = std.mem.startsWith(u8, l.input.bytes[l.input.i..], rightTrimMarker);
            debug("lexRightDelim {} '{s}'", .{ trimSpace, l.input.bytes[l.input.i..] });
            if (trimSpace) {
                l.input.i += leftTrimMarker.len;
                l.ignore();
            }
            l.input.i += l.rightDelim.len;
            l.emit(.right_delim);
            if (trimSpace) {
                l.input.i += leftTrimLength(l.input.bytes[l.input.i..]);
                l.ignore();
            }
            return .lexText;
        }

        // lexInsideAction scans the elements inside action delimiters.
        fn lexInsideAction(l: *Lex) StateFn {
            // Either number, quoted string, or identifier.
            // Spaces separate arguments; runs of spaces turn into itemSpace.
            // pipe symbols separate and are emitted.
            const delim = l.atRightDelim();
            debug("lexInsideAction {}", .{delim});
            if (delim[0]) {
                if (l.parenDepth == 0) {
                    return .lexRightDelim;
                }
                return l.errorf("unclosed left paren", .{});
            }
            var r = l.next();

            if (r == eof or isEndOfLine(r))
                return l.errorf("unclosed action", .{})
            else if (isSpace(r)) {
                l.backup(); // Put space back in case we have " -}}".
                return .lexSpace;
            } else if (r == '=')
                l.emit(.assign)
            else if (r == ':') {
                if (l.next() != '=')
                    return l.errorf("expected :=", .{});
                l.emit(.declare);
            } else if (r == '|')
                l.emit(.pipe)
            else if (r == '"')
                return .lexQuote
            else if (r == '`')
                return .lexRawQuote
            else if (r == '$')
                return .lexVariable
            else if (r == '\'')
                return .lexChar
            else if (r == '.') {
                // special look-ahead for ".field" so we don't break l.backup().
                if (l.input.i < l.input.bytes.len) {
                    r = l.input.bytes[l.input.i];
                    if (r < '0' or '9' < r) {
                        return .lexField;
                    }
                }
                // fallthrough // '.' can start a number.
            } else if (r == '+' or r == '-' or ('0' <= r and r <= '9')) {
                l.backup();
                return .lexNumber;
            } else if (isAlphaNumeric(r)) {
                l.backup();
                return .lexIdentifier;
            } else if (r == '(') {
                l.emit(.left_paren);
                l.parenDepth += 1;
            } else if (r == ')') {
                l.emit(.right_paren);
                l.parenDepth -%= 1;
                if (l.parenDepth == std.math.maxInt(@TypeOf(l.parenDepth))) {
                    return l.errorf("unexpected right paren '{u}'", .{r});
                }
            } else if (r < 128 and std.ascii.isPrint(@truncate(u8, r)))
                l.emit(.char)
            else
                return l.errorf("unrecognized character in action: {u}", .{r});

            return .lexInsideAction;
        }

        // lexSpace scans a run of space characters.
        // We have not consumed the first space, which is known to be present.
        // Take care if there is a trim-marked right delimiter, which starts with a space.
        fn lexSpace(l: *Lex) StateFn {
            var numSpaces: int = 0;
            while (true) {
                const r = l.peek();
                if (!isSpace(r))
                    break;

                _ = l.next();
                numSpaces += 1;
            }
            // Be careful about a trim-marked closing delimiter, which has a minus
            // after a space. We know there is a space, so check for the '-' that might follow.
            if (std.mem.startsWith(u8, l.input.bytes[l.input.i - 1 ..], l.trimRightDelim)) {
                l.backup(); // Before the space.
                if (numSpaces == 1) {
                    return .lexRightDelim; // On the delim, so go right to that.
                }
            }
            l.emit(.space);
            return .lexInsideAction;
        }

        // lexIdentifier scans an alphanumeric.
        fn lexIdentifier(l: *Lex) StateFn {
            while (true) {
                const r = l.next();
                // debug("lexIdentifier {u}", .{r});
                if (isAlphaNumeric(r)) {}
                // absorb.
                else {
                    l.backup();
                    const word = l.input.bytes[l.start..l.input.i];
                    debug("lexIdentifier else. word {s} l.atTerminator(): {}", .{ word, l.atTerminator() });

                    if (!l.atTerminator()) {
                        return l.errorf("bad character {u}", .{r});
                    }
                    const kwint = @enumToInt(ItemType.keyword);
                    const typ = key.get(word) orelse ItemType.err;
                    debug("kwint {} typ {}", .{ kwint, typ });
                    if (@enumToInt(typ) > kwint)
                        l.emit(typ)
                    else if (word[0] == '.')
                        l.emit(.field)
                    else if (std.mem.eql(u8, word, "true") or std.mem.eql(u8, word, "false"))
                        l.emit(.bool)
                    else
                        l.emit(.identifier);
                    break;
                }
            }

            return .lexInsideAction;
        }

        // lexField scans a field: .Alphanumeric.
        // The . has been scanned.
        fn lexField(l: *Lex) StateFn {
            return lexFieldOrVariable(l, .field);
        }

        // lexVariable scans a variable: $Alphanumeric.
        // The $ has been scanned.
        fn lexVariable(l: *Lex) StateFn {
            if (l.atTerminator()) { // Nothing interesting follows -> "$".
                l.emit(.variable);
                return .lexInsideAction;
            }
            return lexFieldOrVariable(l, .variable);
        }

        // lexVariable scans a field or variable: [.$]Alphanumeric.
        // The . or $ has been scanned.
        fn lexFieldOrVariable(l: *Lex, typ: ItemType) StateFn {
            if (l.atTerminator()) { // Nothing interesting follows -> "." or "$".
                if (typ == .variable) {
                    l.emit(.variable);
                } else {
                    l.emit(.dot);
                }
                return .lexInsideAction;
            }
            var r: rune = undefined;
            while (true) {
                r = l.next();
                if (!isAlphaNumeric(r)) {
                    l.backup();
                    break;
                }
            }
            if (!l.atTerminator()) {
                return l.errorf("bad character {u}", .{r});
            }
            l.emit(typ);
            return .lexInsideAction;
        }

        // atTerminator reports whether the input is at valid termination character to
        // appear after an identifier. Breaks .X.Y into two pieces. Also catches cases
        // like "$x+2" not being acceptable without a space, in case we decide one
        // day to implement arithmetic.
        fn atTerminator(l: *Lex) bool {
            const r = l.peek();
            // debug("atTerminator {u} == '{u}': {}", .{ r, l.rightDelimCp, r == l.rightDelimCp });
            if (isSpace(r) or isEndOfLine(r)) {
                return true;
            }
            switch (r) {
                eof, '.', ',', '|', ':', ')', '(' => return true,
                else => {},
            }
            // Does r start the delimiter? This can be ambiguous (with delim=="//", $x/2 will
            // succeed but should fail) but only in extremely rare cases caused by willfully
            // bad choice of delimiter.
            if (l.rightDelimCp == r) {
                return true;
            }
            return false;
        }

        // lexChar scans a character constant. The initial quote is already
        // scanned. Syntax checking is done by the parser.
        fn lexChar(l: *Lex) StateFn {
            while (true) {
                var r = l.next();
                if (r == '\\') {
                    const nextr = l.next();
                    if (nextr != eof and nextr != '\n')
                        continue;
                } else if (r == '\'')
                    break;
                // fallthrough
                if (r == eof or r == '\n')
                    return l.errorf("unterminated character constant", .{});
            }
            l.emit(.char_constant);
            return .lexInsideAction;
        }

        // lexNumber scans a number: decimal, octal, hex, float, or imaginary. This
        // isn't a perfect number scanner - for instance it accepts "." and "0x0.2"
        // and "089" - but when it's wrong the input is invalid and the parser (via
        // strconv) will notice.
        fn lexNumber(l: *Lex) StateFn {
            if (!l.scanNumber()) {
                return l.errorf("bad number syntax: \"{s}\"", .{l.input.bytes[l.start..l.input.i]});
            }
            const sign = l.peek();
            if (sign == '+' or sign == '-') {
                // complex: 1+2i. No spaces, must end in 'i'.
                if (!l.scanNumber() or l.input.bytes[l.input.i - 1] != 'i') {
                    return l.errorf("bad number syntax: \"{s}\"", .{l.input.bytes[l.start..l.input.i]});
                }
                l.emit(.complex);
            } else {
                l.emit(.number);
            }
            return .lexInsideAction;
        }

        fn scanNumber(l: *Lex) bool {
            // Optional leading sign.
            _ = l.accept("+-");
            // Is it hex?
            var digits: []const u8 = "0123456789_";
            if (l.accept("0")) {
                // Note: Leading 0 does not mean octal in floats.
                if (l.accept("xX")) {
                    digits = "0123456789abcdefABCDEF_";
                } else if (l.accept("oO")) {
                    digits = "01234567_";
                } else if (l.accept("bB")) {
                    digits = "01_";
                }
            }
            l.acceptRun(digits);
            if (l.accept(".")) {
                l.acceptRun(digits);
            }
            if (digits.len == 10 + 1 and l.accept("eE")) {
                _ = l.accept("+-");
                l.acceptRun("0123456789_");
            }
            if (digits.len == 16 + 6 + 1 and l.accept("pP")) {
                _ = l.accept("+-");
                l.acceptRun("0123456789_");
            }
            // Is it imaginary?
            _ = l.accept("i");
            // Next thing mustn't be alphanumeric.
            if (isAlphaNumeric(l.peek())) {
                _ = l.next();
                return false;
            }
            return true;
        }

        // lexQuote scans a quoted string.
        fn lexQuote(l: *Lex) StateFn {
            while (true) {
                var r = l.next();
                if (r == '\\') {
                    r = l.next();
                    if (r != eof and r != '\n')
                        continue;
                } else if (r == '"')
                    break;
                // fallthrough
                if (r == eof or r == '\n')
                    return l.errorf("unterminated quoted string", .{});
            }
            l.emit(.string);
            return .lexInsideAction;
        }

        // lexRawQuote scans a raw quoted string.
        fn lexRawQuote(l: *Lex) StateFn {
            while (true) {
                const r = l.next();
                if (r == eof)
                    return l.errorf("unterminated raw quoted string", .{})
                else if (r == '`')
                    break;
            }
            l.emit(.raw_string);
            return .lexInsideAction;
        }

        // isSpace reports whether r is a space character.
        fn isSpace(r: rune) bool {
            return r == ' ' or r == '\t';
        }

        // isEndOfLine reports whether r is an end-of-line character.
        fn isEndOfLine(r: rune) bool {
            return r == '\r' or r == '\n';
        }

        // isAlphaNumeric reports whether r is an alphabetic, digit, or underscore.
        fn isAlphaNumeric(r: rune) bool {
            // return r == '_' or std.unicode.isLetter(r) or std.unicode.isDigit(r);
            return r == '_' or (r < 128 and std.ascii.isAlNum(@intCast(u8, r)));
        }
    };
}

test "contains rune" {
    const T = struct {
        str: string,
        r: rune,
        expected: bool,
        fn init(str: string, r: rune, expected: bool) @This() {
            return .{
                .str = str,
                .r = r,
                .expected = expected,
            };
        }
    };
    const tests = [_]T{
        T.init("", 'a', false),
        T.init("a", 'a', true),
        T.init("aaa", 'a', true),
        T.init("abc", 'y', false),
        T.init("abc", 'c', true),
        T.init("a☺b☻c☹d", 'x', false),
        T.init("a☺b☻c☹d", '☻', true),
        T.init("aRegExp*", '*', true),
    };
    for (tests) |tst| {
        std.testing.expectEqual(tst.expected, try Lexer(.{}).containsRune(tst.str, tst.r));
    }
}
