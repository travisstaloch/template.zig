const std = @import("std");
usingnamespace @import("template.zig");
const lex = @import("lex.zig");
const Options = @import("Options.zig");

pub fn TreeOpts(comptime options: Options) type {
    return struct {
        pub const TreeSet = void; //std.StringHashMap(Tree);
        pub const FuncMap = void; //std.AutoHashMap(void);
        const Tree = @This();
        const Lexer = lex.Lexer(options);

        // name: []const u8, // name of the template represented by the tree.
        // parse_name: []const u8, // name of the top-level template during parsing, for error messages.
        root: Node.List, // top-level root of the tree.
        // text: []const u8, // text parsed to create the template (or its parent)
        // Parsing :only,; cleared after parse.
        funcs: FuncMap,
        lex: *Lexer,
        peek_tokens: [3]lex.Item = undefined, // three-token lookahead for parser.
        peek_count: u2 = 0,
        vars: []const []const u8, // variables defined at the moment.
        tree_set: TreeSet,
        allocator: if (options.is_comptime) void else *std.mem.Allocator,

        pub fn init(lexer: *Lexer, funcs: FuncMap, tree_set: TreeSet) Tree {
            comptime std.debug.assert(options.is_comptime);
            return .{
                .root = Node.List.init(),
                .lex = lexer,
                .vars = &[1][]const u8{"$"},
                .funcs = funcs,
                .tree_set = tree_set,
                .allocator = {},
            };
        }

        pub fn initAlloc(lexer: *Lexer, funcs: FuncMap, tree_set: TreeSet, allocator: *std.mem.Allocator) Tree {
            comptime std.debug.assert(!options.is_comptime);
            return .{
                .root = Node.List.init(),
                .lex = lexer,
                .vars = &[1][]const u8{"$"},
                .funcs = funcs,
                .tree_set = tree_set,
                .allocator = allocator,
            };
        }

        pub fn peek(t: *Tree) lex.Item {
            if (t.peek_count > 0) {
                // @compileLog(t.peek_tokens[t.peek_count - 1].typ, t.peek_tokens[t.peek_count - 1].val);
                debug("peek1 {}", .{t.peek_tokens[t.peek_count - 1]});
                return t.peek_tokens[t.peek_count - 1];
            }
            t.peek_count = 1;
            t.peek_tokens[0] = t.lex.nextItem() orelse return .{ .typ = .err, .line = 0, .pos = 0, .val = "peek no nextItem" };
            // @compileLog(t.peek_tokens[0].typ, t.peek_tokens[0].val);
            debug("peek2 {}", .{t.peek_tokens[0]});
            return t.peek_tokens[0];
        }

        pub fn peekNonSpace(t: *Tree) lex.Item {
            defer t.backup();
            return t.nextNonSpace();
        }

        pub fn next(t: *Tree) lex.Item {
            if (t.peek_count > 0)
                t.peek_count -= 1
            else
                t.peek_tokens[0] = t.lex.nextItem() orelse return .{ .typ = .err, .line = 0, .pos = 0, .val = "" };

            debug("next {}", .{t.peek_tokens[t.peek_count]});
            return t.peek_tokens[t.peek_count];
        }

        pub fn nextNonSpace(t: *Tree) lex.Item {
            var token: lex.Item = undefined;
            while (true) {
                token = t.next();
                if (token.typ != .space) break;
            }
            return token;
        }

        // expect consumes the next token and guarantees it has the required type.
        fn expect(t: *Tree, expected: lex.ItemType, context: NodeType) lex.Item {
            const token = t.nextNonSpace();
            if (token.typ != expected) {
                t.unexpected(token, context);
            }
            return token;
        }

        // expectOneOf consumes the next token and guarantees it has the required type.
        fn expectOneOf(t: *Tree, expected1: lex.ItemType, expected2: lex.ItemType, context: NodeType) lex.Item {
            const token = t.nextNonSpace();
            if (token.typ != expected1 or token.typ != expected2)
                t.unexpected(token, context);

            return token;
        }

        // unexpected complains about the token and terminates processing.
        fn unexpected(t: *Tree, token: lex.Item, context: NodeType) noreturn {
            err("unexpected {s} in {s}", .{ @tagName(token.typ), @tagName(context) });
        }

        pub fn backup(t: *Tree) void {
            t.peek_count += 1;
        }

        // backup2 backs the input stream up two tokens.
        // The zeroth token is already there.
        pub fn backup2(t: *Tree, t1: lex.Item) void {
            t.peek_tokens[1] = t1;
            t.peek_count = 2;
        }

        // backup3 backs the input stream up three tokens
        // The zeroth token is already there.
        pub fn backup3(t: *Tree, t2: lex.Item, t1: lex.Item) void { // Reverse order: we're pushing back.
            t.peek_tokens[1] = t1;
            t.peek_tokens[2] = t2;
            t.peek_count = 3;
        }

        pub inline fn push(t: Tree, comptime T: type, list: *[]const T, item: T) void {
            const itemfmt = if (T == []const u8) "{s}" else "{}";
            debug("push. item " ++ itemfmt ++ " list.len {}", .{ item, list.len });

            if (options.is_comptime) {
                list.* = list.* ++ [1]T{item};
            } else {
                var new_list = t.allocator.alloc(T, list.len + 1) catch unreachable;
                std.mem.copy(T, new_list, list.*);
                new_list[list.len] = item;
                list.* = new_list;
            }

            debug("after push. item " ++ itemfmt ++ " list.len {}", .{ item, list.len });
        }

        pub inline fn pushFront(t: Tree, comptime T: type, list: *[]const T, item: T) void {
            const itemfmt = if (T == []const u8) "{s}" else "{}";
            debug("pushFront. item " ++ itemfmt ++ " list.len {}", .{ item, list.len });

            if (options.is_comptime) {
                list.* = [1]T{item} ++ list.*;
            } else {
                var new_list = t.allocator.alloc(T, list.len + 1) catch unreachable;
                std.mem.copy(T, new_list[1..], list.*);
                new_list[0] = item;
                list.* = new_list;
            }
            debug("after pushFront. item " ++ itemfmt ++ " list.len {}", .{ item, list.len });
        }

        pub fn tokenizeOn(t: Tree, input: []const u8, comptime split_str: []const u8) []const []const u8 {
            var result: []const []const u8 = &[0][]const u8{};
            var it = std.mem.tokenize(input, split_str);
            while (it.next()) |part| {
                // result = result ++ [1][]const u8{trimSpaces(part)};
                // std.log.debug("tokenizeOn part {s}", .{part});
                t.push([]const u8, &result, part);
            }
            return result;
        }

        pub fn err(comptime msg: []const u8, args: anytype) noreturn {
            // std.fmt.comptimePrint(
            //     msg ++ "\ninput: '{s}' at position {d}-{d}",
            //     args ++ .{ t.lex.input.bytes, t.lex.line, t.lex.input.i },
            // )
            // comptime var buf: [0x40]u8 = undefined;
            // // comptime var buf2: [0x10]u8 = undefined;
            // const x = std.fmt.bufPrint(&buf, "\ninput: '{s}' at position {d}-{d}", .{ t.lex.input.bytes, t.lex.line, t.lex.input.i }) catch unreachable;
            // @compileError(msg);
            // std.debug.panic(msg, args);
            if (comptime options.is_comptime) {
                // inline for (std.meta.fields(@TypeOf(args))) |f| @compileLog(comptime @field(args, f.name));
                // @compileLog(args);
                // @compileError(std.fmt.comptimePrint(msg, args));
                // @compileError(args);
                std.debug.panic(msg, args);
            } else {
                std.log.err(msg, args);
                // std.log.err("input: '{s}' at line {d}:{d}", .{ t.lex.input.bytes, t.lex.line, t.lex.input.i });
                @panic("err");
            }
        }

        pub fn debug(comptime msg: []const u8, args: anytype) void {
            options.debug(msg, args);
        }

        // func | variable | field | constant | interval
        fn command(t: *Tree) Node.Command {
            var cmd = Node.Command{ .args = &[0]Node{} };
            while (true) {
                _ = t.peekNonSpace(); // skip leading spaces.
                const op = t.operand();
                debug("command op {s} {}", .{ if (op) |o| @tagName(o) else "null", op });
                if (op != null)
                    t.push(Node, &cmd.args, op.?);

                const token = t.next();
                switch (token.typ) {
                    .space => continue,
                    .err => err("error found: '{s}'", .{token.val}),
                    .right_delim, .right_paren => t.backup(),
                    .pipe => {},
                    else => err("unexpected '{s}' in operand. token {s}", .{ token.val, @tagName(token.typ) }),
                }
                break;
            }
            if (cmd.args.len == 0)
                err("empty command", .{});

            return cmd;
        }

        pub fn parseAlloc(
            comptime text: []const u8,
            name: []const u8,
            left_delim: []const u8,
            comptime right_delim: []const u8,
            tree_set: Tree.TreeSet,
            funcs: Tree.FuncMap,
            allocator: *std.mem.Allocator,
        ) Tree {
            var items_buf: [0x10]lex.Item = undefined;
            var lexer = Lexer.init(name, text, left_delim, right_delim, &items_buf);
            var t = Tree.initAlloc(&lexer, tree_set, funcs, allocator);
            return parseImpl(&t);
        }

        pub fn parse(
            comptime text: []const u8,
            comptime name: []const u8,
            comptime left_delim: []const u8,
            comptime right_delim: []const u8,
            comptime tree_set: Tree.TreeSet,
            comptime funcs: Tree.FuncMap,
        ) Tree {
            @setEvalBranchQuota(options.eval_branch_quota);
            comptime {
                std.debug.assert(options.is_comptime);
                var items_buf: [0x10]lex.Item = undefined;
                var lexer = Lexer.init(name, text, left_delim, right_delim, &items_buf);
                var t = Tree.init(&lexer, tree_set, funcs);
                return parseImplComptime(&t);
            }
        }

        fn parseImpl(t: *Tree) Tree {
            debug("\n----\nparse input '{s}'", .{t.lex.input.bytes});

            var list = &t.root;
            while (t.peek().typ != .EOF) {
                // debug("parse item {}", .{item});
                if (t.peek().typ == .left_delim) {
                    const delim = t.next();
                    if (t.nextNonSpace().typ == .define) {
                        var new_t = if (options.is_comptime)
                            Tree.init(t.lex, t.funcs, t.tree_set)
                        else
                            Tree.initAlloc(t.lex, t.funcs, t.tree_set, t.allocator);
                        new_t.parseDefinition();
                        continue;
                    }
                    t.backup2(delim);
                }
                const n = t.textOrAction();
                // @compileLog("parse n ", std.meta.activeTag(n));
                if (n == .end or n == .else_)
                    err("unexpected end or else", .{})
                else
                    t.push(Node, &list.root, n);

                debug("new_node {}\n", .{n});
            }
            return t.*;
        }

        fn parseImplComptime(comptime t: *Tree) Tree {
            debug("\n----\nparse input '{s}'", .{t.lex.input.bytes});

            comptime {
                var list = &t.root;
                inline while (t.peek().typ != .EOF) {
                    // debug("parse item {}", .{item});
                    if (t.peek().typ == .left_delim) {
                        const delim = t.next();
                        if (t.nextNonSpace().typ == .define) {
                            var new_t = if (options.is_comptime)
                                Tree.init(t.lex, t.funcs, t.tree_set)
                            else
                                Tree.initAlloc(t.lex, t.funcs, t.tree_set, t.allocator);
                            new_t.parseDefinition();
                            continue;
                        }
                        t.backup2(delim);
                    }
                    const n = comptime t.textOrAction();
                    // @compileLog("parse n ", std.meta.activeTag(n));
                    if (n == .end or n == .else_)
                        err("unexpected end or else", .{})
                    else
                        t.push(Node, &list.root, n);

                    debug("new_node {}\n", .{n});
                }
                return t.*;
            }
        }

        fn textOrAction(t: *Tree) Node {
            const token = t.nextNonSpace();
            debug("textOrAction {}", .{token});
            return switch (token.typ) {
                .text => .{ .text = token.val },
                .left_delim => t.action(),
                else => err("unexpected input '{}'", .{token.typ}),
            };
        }

        // Action:
        //	control
        //	command ("|" command)*
        // Left delim is past. Now get actions.
        // First word could be a keyword such as range.
        fn action(t: *Tree) Node {
            var token = t.nextNonSpace();
            debug("action {}", .{token});
            switch (token.typ) {
                .block => return t.blockControl(),
                .else_ => return t.elseControl(),
                .end => return t.endControl(),
                .if_ => return t.ifControl(),
                .range => return t.rangeControl(),
                .template => return t.templateControl(),
                .with => return t.withControl(),
                else => {},
            }
            t.backup();
            token = t.peek();
            // Do not pop variables; they persist until "end".
            debug("action2 ", .{});
            return .{ .action = t.pipeline(.command) };
        }

        // operand:
        //	term .Field*
        // An operand is a space-separated component of a command,
        // a term possibly followed by field accesses.
        // A nil return means the next item is not an operand.
        fn operand(t: *Tree) ?Node {
            var node = t.term() orelse return null;
            debug("operand node {}", .{node});
            if (t.peek().typ == .field) {
                const chain_node = if (!options.is_comptime) blk: {
                    var new_node = (t.allocator.create(Node) catch |e| err("{s}", .{@errorName(e)}));
                    new_node.* = node;
                    break :blk new_node;
                } else &node;

                var chain = Node{ .chain = .{ .node = chain_node, .field = &[0][]const u8{} } };
                if (options.is_comptime) {
                    inline while (t.peek().typ == .field)
                        t.push([]const u8, &chain.chain.field, t.next().val[1..]); // skip leading '.'
                } else {
                    while (t.peek().typ == .field) {
                        debug("operand field {s}", .{t.peek().val});
                        t.push([]const u8, &chain.chain.field, t.next().val[1..]); // skip leading '.'
                    }
                }

                // Compatibility with original API: If the term is of type NodeField
                // or NodeVariable, just put more fields on the original.
                // Otherwise, keep the Chain node.
                // Obvious parsing errors involving literal values are detected here.
                // More complex error cases will have to be handled at execution time.
                switch (node) {
                    .field => {
                        t.pushFront([]const u8, &chain.chain.field, node.field[0]);
                        node = .{ .field = chain.chain.field };
                    },
                    .variable => {
                        // t.pushFront([]const u8, &chain.chain.field, std.mem.bytesAsValue(Node, chain.chain.node[0..120]).variable[0]);
                        t.pushFront([]const u8, &chain.chain.field, chain.chain.node.variable[0]);
                        node = .{ .variable = chain.chain.field };
                    },
                    .constant, .nil, .dot => err("unexpected . after term {}", .{node}),
                    else => node = chain,
                }
            }
            return node;
        }

        // term:
        //	literal (number, string, nil, boolean)
        //	function (identifier)
        //	.
        //	.Field
        //	$
        //	'(' pipeline ')'
        // A term is a simple "expression".
        // A nil return means the next item is not a term.
        fn term(t: *Tree) ?Node {
            const token = t.nextNonSpace();
            switch (token.typ) {
                .err => err("{s}", .{token.val}),
                .identifier => {
                    // TODO
                    // if (!tfunction(token.val)) {
                    // 	err("function %q not defined", token.val, t);
                    // }
                    // return NewIdentifier(token.val).SetTree(t).SetPos(token.pos);
                    return Node{ .identifier = t.tokenizeOn(token.val, ".") };
                },

                .dot => return .dot,
                .nil => return .nil,
                .variable => return useVar(t, token.val),
                .field => return Node{ .field = t.tokenizeOn(token.val, ".") }, //[1..] },
                .bool => return Node{ .constant = .{ .bool = token.val } },
                .complex => err("complex numbers not supported yet", .{}),
                .char_constant => return Node{ .constant = .{ .char = token.val } },
                .number => return Node{ .constant = .{ .int = token.val } },
                .left_paren => {
                    const pipe = t.pipeline(.pipeline);
                    const token2 = t.next();
                    if (token2.typ != .right_paren)
                        err("unclosed right paren: unexpected {s}", .{@tagName(token2.typ)});

                    return Node{ .pipeline = pipe };
                },
                .string, .raw_string => {
                    // s, err := strconv.Unquote(token.val) // TODO
                    // if err != nil {
                    // 	t.error(err)
                    // }
                    // return t.newString(token.pos, token.val, s)
                    return Node{ .constant = .{ .string = token.val } };
                },
                else => {},
            }
            t.backup();
            return null;
        }

        fn useVar(t: *Tree, name: []const u8) Node {
            const v = Node{ .variable = t.tokenizeOn(name, ".") };
            for (t.vars) |var_name| {
                if (std.mem.eql(u8, var_name, v.variable[0])) {
                    return v;
                }
            } else
                err("undefined variable {s}", .{v.variable[0]});
            return v;
        }

        inline fn pipelineDecls(t: *Tree, pipe: *Node.Pipeline, context: NodeType) void {
            const v = t.peekNonSpace();
            debug("pipeline v {}", .{v});
            if (v.typ == .variable) {
                _ = t.next();
                const tok_after_var = t.peek();
                const next_tok = t.peekNonSpace();
                debug("pipeline tok_after_var {} next_tok {}", .{ tok_after_var, next_tok });
                switch (next_tok.typ) {
                    .assign, .declare => {
                        pipe.is_assign = true;
                        _ = t.nextNonSpace();
                        t.push([]const u8, &pipe.decls, v.val);
                        t.push([]const u8, &t.vars, v.val);
                    },
                    .char => if (next_tok.val.len == 1 and next_tok.val[0] == ',') {
                        _ = t.nextNonSpace();
                        t.push([]const u8, &pipe.decls, v.val);
                        t.push([]const u8, &t.vars, v.val);

                        if (context == .range and pipe.decls.len < 2) {
                            switch (t.peekNonSpace().typ) {
                                .variable, .right_delim, .right_paren =>
                                // second initialized variable in a range pipeline
                                return pipelineDecls(t, pipe, context),
                                else => err("range can only initialize variables", .{}),
                            }
                        }
                        err("too many declarations in {}", .{context});
                    },
                    else => if (tok_after_var.typ == .space)
                        t.backup3(v, tok_after_var)
                    else
                        t.backup2(v),
                }
            }
        }

        // Pipeline:
        //	declarations? command ('|' command)*
        fn pipeline(t: *Tree, context: NodeType) Node.Pipeline {
            // const token = t.peekNonSpace();
            // debug("pipeline context {s}, token {}", .{ @tagName(context), token });
            var pipe = Node.Pipeline.init();
            pipelineDecls(t, &pipe, context);
            while (true) {
                const token2 = t.nextNonSpace();
                debug("pipeline token2 {}", .{token2});
                switch (token2.typ) {
                    .right_delim, .right_paren => {
                        // At this point, the pipeline is complete
                        // t.checkPipeline(pipe, context) // TODO
                        if (token2.typ == .right_paren) {
                            t.backup();
                        }
                        return pipe;
                    },
                    .bool, .char_constant, .complex, .dot, .field, .identifier, .number, //
                    .nil, .raw_string, .string, .variable, .left_paren => {
                        t.backup();
                        t.push(Node.Command, &pipe.cmds, t.command());
                    },
                    else => t.unexpected(token2, context),
                }
            }
            return pipe;
        }

        const ListAndNext = struct { list: Node.List, next: ?Node };
        // itemList:
        //	textOrAction*
        // Terminates at {{end}} or {{else}}, returned separately.
        fn itemList(t: *Tree) ListAndNext {
            var result = ListAndNext{ .list = Node.List.init(), .next = null };
            while (t.peekNonSpace().typ != .EOF) {
                const n = textOrAction(t);
                switch (n) {
                    .end, .else_ => {
                        result.next = n;
                        return result;
                    },
                    else => t.push(Node, &result.list.root, n),
                }
            }
            err("unexpected EOF", .{});
        }

        // If:
        //	{{if pipeline}} itemList {{end}}
        //	{{if pipeline}} itemList {{else}} itemList {{end}}
        // If keyword is past.
        fn ifControl(t: *Tree) Node {
            return .{ .if_ = t.parseControl(true, .if_) };
        }

        // Range:
        //	{{range pipeline}} itemList {{end}}
        //	{{range pipeline}} itemList {{else}} itemList {{end}}
        // Range keyword is past.
        fn rangeControl(t: *Tree) Node {
            return .{ .range = t.parseControl(false, .range) };
        }

        // With:
        //	{{with pipeline}} itemList {{end}}
        //	{{with pipeline}} itemList {{else}} itemList {{end}}
        // If keyword is past.
        fn withControl(t: *Tree) Node {
            return .{ .with = t.parseControl(false, .with) };
        }

        // End:
        //	{{end}}
        // End keyword is past.
        fn endControl(t: *Tree) Node {
            _ = expect(t, .right_delim, .end);
            return .end;
        }

        // Else:
        //	{{else}}
        // Else keyword is past.
        fn elseControl(t: *Tree) Node {
            // Special .{.else = "else if".
            const token = t.peekNonSpace();
            if (token.typ == .if_) {
                // We see "{{else if ... " but in effect rewrite it to {{else}}{{if ... ".
                // return t.newElse(token.pos, token.line)
                return .else_;
            }
            _ = t.expect(.right_delim, .else_);
            return .else_;
        }

        // popVars trims the variable list to the specified length
        fn popVars(t: *Tree, n: usize) void {
            t.vars = t.vars[0..n];
        }

        fn parseControl(t: *Tree, allowElseIf: bool, context: NodeType) Node.Branch {
            defer popVars(t, t.vars.len);
            var pipe = t.pipeline(context);
            var list_and_next = itemList(t);
            // var else_list_and_next: ?Control = null;
            var else_list: ?Node.List = null;
            const nxt = list_and_next.next orelse err("unexpected end of input", .{});
            switch (nxt) {
                .end => {}, //done
                .else_ => blk: {
                    if (allowElseIf) {
                        // Special case for "else if". If the "else" is followed immediately by an "if",
                        // the elseControl will have left the "if" token pending. Treat
                        //	{{if a}}_{{else if b}}_{{end}}
                        // as
                        //	{{if a}}_{{else}}{{if b}}_{{end}}{{end}}.
                        // To do this, parse the if as usual and stop at it {{end}}; the subsequent{{end}}
                        // is assumed. This technique works even for long if-else-if chains.
                        // TODO: Should we allow else-if in with and range?
                        if (t.peek().typ == .if_) {
                            _ = t.next(); // Consume the "if" token.
                            else_list = Node.List.init();
                            t.push(Node, &else_list.?.root, ifControl(t));
                            // Do not consume the next item - only one {{end}} required.
                            break :blk;
                        }
                    }
                    const else_list_and_next = t.itemList();
                    else_list = else_list_and_next.list;
                    list_and_next.next = else_list_and_next.next;
                    if (list_and_next.next == null or list_and_next.next.? != .end)
                        err("expected end; found {s}", .{list_and_next.next});
                },
                else => {},
            }
            // return pipe.Position(), pipe.Line, pipe, list, else_list
            return .{ .pipeline = pipe, .list = list_and_next.list, .else_list = else_list };
        }

        // Block:
        //	{{block stringValue pipeline}}
        // Block keyword is past.
        // The name must be something that can evaluate to a string.
        // The pipeline is mandatory.
        fn blockControl(t: *Tree) Node {
            const context: NodeType = .template;

            const token = t.nextNonSpace();
            const name = t.parseTemplateName(token, context);
            const pipe = t.pipeline(context);
            debug("blockControl {} {s} {}", .{ token, name, pipe });

            // block.ParseName = t.ParseName
            var block = if (options.is_comptime)
                Tree.init(t.lex, t.funcs, t.tree_set) // name will be updated once we know it.
            else
                Tree.initAlloc(t.lex, t.funcs, t.tree_set, t.allocator); // name will be updated once we know it.
            // block.text = t.text;
            // block.startParse(t.funcs, t.lex, t.treeSet)

            const root_and_end = block.itemList();
            if (root_and_end.next == null or root_and_end.next.? != .end)
                err("unexpected {} in {}", .{ root_and_end.next, context });

            // TODO add block to t.tree_set
            // block.add()
            // block.stopParse()
            debug("blockControl root {} end {}", .{ root_and_end.list, root_and_end.next });

            return .{ .template = .{ .name = name, .pipeline = pipe } };
        }

        // Template:
        //	{{template stringValue pipeline}}
        // Template keyword is past. The name must be something that can evaluate
        // to a string.
        fn templateControl(t: *Tree) Node {
            const context: NodeType = .template;
            const token = t.nextNonSpace();
            const name = t.parseTemplateName(token, context);
            var pipe: ?Node.Pipeline = null;
            const token2 = t.nextNonSpace();

            if (token2.typ != .right_delim) {
                t.backup();
                // Do not pop variables; they persist until "end".
                pipe = t.pipeline(context);
            }
            return .{ .template = .{ .name = name, .pipeline = pipe } };
        }

        fn parseTemplateName(t: *Tree, token: lex.Item, context: NodeType) []const u8 {
            var result: []const u8 = "unnamed template";
            switch (token.typ) {
                .string, .raw_string => {
                    // TODO: unquote
                    // s, err := strconv.Unquote(token.val)
                    // if err != nil {
                    // 	t.error(err)
                    // }
                    result = token.val;
                },
                else => err("unexpected {} in context {}", .{ token, context }),
            }

            return result;
        }

        // parseDefinition parses a {{define}} ...  {{end}} template definition and
        // installs the definition in t.treeSet. The "define" keyword has already
        // been scanned.
        fn parseDefinition(t: *Tree) void {
            const context: NodeType = .define;
            const name = t.expectOneOf(.string, .raw_string, context);
            // TODO: unquote
            // t.Name, err = strconv.Unquote(name.val)
            // if err != nil {
            // 	t.error(err)
            // }
            _ = t.expect(.right_delim, context);
            const root_and_end = t.itemList();
            if (root_and_end.next == null or root_and_end.next.? != .end)
                err("unexpected {} in {}", .{ root_and_end.next, context });

            // TODO: add
            // t.add()
            // t.stopParse()
        }
    };
}
