const std = @import("std");

eval_branch_quota: u32 = 2000,
is_comptime: bool = true,
compile_log: bool = false,
name: ?[]const u8 = null,
context: []const u8 = "",

pub fn debug(comptime options: @This(), comptime fmt: []const u8, args: anytype) void {
    if (options.is_comptime and options.compile_log) {
        // @compileLog(options.context, fmt, args);
        @compileLog(fmt, args);
    } else if (!options.is_comptime) {
        // std.log.debug(options.context ++ " - " ++ fmt, args);
        std.log.debug(fmt, args);
        // std.log.debug("input: '{s}' at line {d}:{d}", .{ t.lex.input.bytes, t.lex.line, t.lex.input.i });
    }
}

pub fn withName(comptime options: @This(), comptime name: []const u8) @This() {
    return .{
        .eval_branch_quota = options.eval_branch_quota,
        .is_comptime = options.is_comptime,
        .compile_log = options.compile_log,
        .context = options.context,
        .name = name,
    };
}
