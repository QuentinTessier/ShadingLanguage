const std = @import("std");
const Parser = @import("Parser.zig");
const Input = @import("Input.zig");
const Combinator = @import("Combinator.zig");
const Char = @import("Char.zig");

pub fn integer(input: Input, allocator: std.mem.Allocator, comptime T: type) anyerror!Parser.Result(T) {
    return (try Combinator.many1(input, allocator, .{ .parser = Char.digit, .args = .{} })).map(allocator, T, struct {
        pub fn mapfnc(allocator_: std.mem.Allocator, buf: []const u8) anyerror!T {
            const i = try std.fmt.parseInt(T, buf, 10);
            allocator_.free(buf);
            return i;
        }
    }.mapfnc);
}

pub fn identifier(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result([]u8) {
    var array = std.ArrayList(u8).init(allocator);
    switch (try Combinator.choice(input, allocator, .{
        .{ .parser = Char.symbol, .args = .{'_'} },
        .{ .parser = Char.alpha, .args = .{} },
    })) {
        .some => |some| {
            try array.append(some.value);
            switch (try Combinator.many(input, allocator, .{
                .parser = Combinator.choice,
                .args = .{.{
                    .{ .parser = Char.symbol, .args = .{'_'} },
                    .{ .parser = Char.alphaNum, .args = .{} },
                }},
            })) {
                .some => |some2| {
                    try array.appendSlice(some2.value);
                    allocator.free(some2.value);
                    return .{ .some = .{
                        .value = try array.toOwnedSlice(),
                        .tail = some2.tail,
                    } };
                },
                .none => |none| {
                    array.deinit();
                    return .{
                        .none = none,
                    };
                },
            }
        },
        .none => |none| {
            array.deinit();
            return .{
                .none = none,
            };
        },
    }
}
