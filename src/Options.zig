const std = @import("std");

eval_branch_quota: u32 = 4000,
is_comptime: bool = true,
compile_log: bool = false,
name: ?[]const u8 = null,
context: []const u8 = "",

const Options = @This();

pub fn debug(comptime options: Options, comptime fmt: []const u8, args: anytype) void {
    if (options.is_comptime and options.compile_log) {
        // @compileLog(options.context, fmt, args);
        @compileLog(fmt, args);
    } else if (!options.is_comptime) {
        // std.log.debug(options.context ++ " - " ++ fmt, args);
        std.log.debug(fmt, args);
        // std.log.debug("input: '{s}' at line {d}:{d}", .{ t.lex.input.bytes, t.lex.line, t.lex.input.i });
    }
}

pub fn with(comptime options: Options, comptime new_options: anytype) Options {
    var result = options;
    inline for (std.meta.fields(@TypeOf(new_options))) |f| {
        if (!@hasField(Options, f.name)) @compileError("Options has no field '" ++ f.name ++ "'");
        @field(result, f.name) = @field(new_options, f.name);
    }
    return result;
}
