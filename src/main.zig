const std = @import("std");
const Parser = @import("ZigParsec");
const P = struct {
    usingnamespace @import("Parser/Statement.zig");
    usingnamespace @import("Parser/Expression.zig");
};

const Statement = @import("Parser/Statement.zig");

pub const S = struct {
    s1: u8,
    s2: u8,
    s3: u8,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    const allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(allocator);
    const arenaAllocator = arena.allocator();

    defer {
        arena.deinit();
        _ = gpa.deinit();
    }

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    // TODO: real command line arguments handling
    if (args.len != 2) {
        std.log.err("Expected 2 args, no more no less :)", .{});
        return error.InvalidNumberOfArgs;
    }

    const filepath = args[1];

    const file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();

    const buffer = try file.readToEndAlloc(allocator, 100_000);
    defer allocator.free(buffer);

    const s = Parser.Stream.init(buffer, null);
    const state = try P.createParserState(allocator);
    defer P.destroyParserState(allocator, state);

    switch (try Statement.statement(s, arenaAllocator, state)) {
        .Result => |res| {
            std.log.info("{any}", .{res.value});
            res.value.deinit(allocator);
            allocator.destroy(res.value);
        },
        .Error => |err| {
            std.log.err("{s}", .{err.msg.items});
            err.msg.deinit();
        },
    }
}
