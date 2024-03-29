const std = @import("std");
const Parser = @import("Parser.zig");
const Input = @import("Input.zig");
const Combinator = @import("Combinator.zig");

pub fn symbol(input: Input, _: std.mem.Allocator, c: u8) anyerror!Parser.Result(u8) {
    const peeked = input.peek(1);
    if (input.isEOF() or peeked.len == 0) {
        return .{
            .none = "Unexpected EOF",
        };
    }
    if (peeked[0] != c) {
        return .{
            .none = "Expected " ++ [1]u8{c},
        };
    }
    return .{
        .some = .{
            .value = peeked[0],
            .tail = input.eat(1),
        },
    };
}

pub fn anySymbol(input: Input, _: std.mem.Allocator) anyerror!Parser.Result(u8) {
    const peeked = input.peek(1);
    if (input.isEOF() or peeked.len == 0) {
        return .{
            .none = "Unexpected EOF",
        };
    }
    return .{
        .some = .{
            .value = peeked[0],
            .tail = input.eat(1),
        },
    };
}

pub fn range(input: Input, _: std.mem.Allocator, l: u8, u: u8) anyerror!Parser.Result(u8) {
    const peeked = input.peek(1);
    if (input.isEOF() or peeked.len == 0) {
        return .{
            .none = "Unexpected EOF",
        };
    }
    if (peeked[0] < l or peeked[0] > u) {
        return .{
            .none = "Expected value between " ++ [1]u8{l} ++ " " ++ [1]u8{u},
        };
    }
    return .{
        .some = .{
            .value = peeked[0],
            .tail = input.eat(1),
        },
    };
}

pub fn oneOf(input: Input, _: std.mem.Allocator, chars: []const u8) anyerror!Parser.Result(u8) {
    const peeked = input.peek(1);
    if (input.isEOF() or peeked.len == 0) {
        return .{
            .none = "Unexpected EOF",
        };
    }
    for (chars) |c| {
        if (peeked[0] == c) {
            return .{
                .some = .{
                    .value = peeked[0],
                    .tail = input.eat(1),
                },
            };
        }
    }
    return .{
        .none = "Expected value in " ++ chars,
    };
}

pub fn noneOf(input: Input, _: std.mem.Allocator, chars: []const u8) anyerror!Parser.Result(u8) {
    const peeked = input.peek(1);
    if (input.isEOF() or peeked.len == 0) {
        return .{
            .none = "Unexpected EOF",
        };
    }
    for (chars) |c| {
        if (peeked[0] == c) {
            return .{
                .none = "Expected value not in " ++ chars,
            };
        }
    }
    return .{
        .some = .{
            .value = peeked[0],
            .tail = input.eat(1),
        },
    };
}

pub fn digit(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(u8) {
    return range(input, allocator, '0', '9');
}

pub fn lowercase(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(u8) {
    return range(input, allocator, 'a', 'z');
}

pub fn uppercase(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(u8) {
    return range(input, allocator, 'A', 'Z');
}

pub fn whitespace(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(u8) {
    return oneOf(input, allocator, "\n\t\r ");
}

pub fn alpha(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(u8) {
    return Combinator.choice(input, allocator, .{
        .{ .parser = lowercase, .args = .{} },
        .{ .parser = uppercase, .args = .{} },
    });
}

pub fn alphaNum(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(u8) {
    return Combinator.choice(input, allocator, .{
        .{ .parser = lowercase, .args = .{} },
        .{ .parser = uppercase, .args = .{} },
        .{ .parser = digit, .args = .{} },
    });
}

pub fn octDigit(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(u8) {
    return oneOf(input, allocator, "01234567");
}

pub fn hexDigit(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(u8) {
    return Combinator.choice(input, allocator, .{
        .{ .parser = range, .args = .{ 'a', 'f' } },
        .{ .parser = range, .args = .{ 'A', 'F' } },
        .{ .parser = digit, .args = .{} },
    });
}

pub fn satisfy(input: Input, _: std.mem.Allocator, fnc: *const fn (u8) bool) anyerror!Parser.Result(u8) {
    const peeked = input.peek(1);
    if (input.isEOF() or peeked.len == 0) {
        return .{
            .none = "Unexpected EOF",
        };
    }
    if (fnc(peeked[0])) {
        return .{
            .none = "error",
        };
    }
    return .{
        .some = .{
            .value = peeked[0],
            .tail = input.eat(1),
        },
    };
}

pub fn keyword(input: Input, _: std.mem.Allocator, str: []const u8) anyerror!Parser.Result([]const u8) {
    const peeked = input.peek(str.len);
    if (peeked.len != str.len) {
        return .{
            .none = "error",
        };
    }
    if (!std.mem.eql(u8, peeked, str)) {
        return .{
            .none = "error",
        };
    }
    return .{ .some = .{
        .value = str,
        .tail = input.eat(str.len),
    } };
}
