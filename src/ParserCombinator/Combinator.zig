const std = @import("std");
const Parser = @import("Parser.zig");
const Input = @import("Input.zig");

pub fn many(input: Input, allocator: std.mem.Allocator, p: anytype) blk: {
    const ResultType = Parser.getParserResultType(p.parser);
    break :blk anyerror!Parser.Result([]ResultType.T);
} {
    const ResultType = Parser.getParserResultType(p.parser);
    const Res = Parser.Result([]ResultType.T);

    var array = std.ArrayList(ResultType.T).init(allocator);
    var i = input;
    while (true) {
        const r = try @call(.auto, p.parser, .{ i, allocator } ++ p.args);
        switch (r) {
            .some => |some| {
                i = some.tail;
                try array.append(some.value);
            },
            .none => break,
        }
    }
    return Res{
        .some = .{
            .value = try array.toOwnedSlice(),
            .tail = i,
        },
    };
}

pub fn many1(input: Input, allocator: std.mem.Allocator, comptime p: anytype) blk: {
    const ResultType = Parser.getParserResultType(p.parser);
    break :blk anyerror!Parser.Result([]ResultType.T);
} {
    const ResultType = Parser.getParserResultType(p.parser);

    var array = std.ArrayList(ResultType.T).init(allocator);
    var i = input;
    const r1 = try @call(.auto, p.parser, .{ i, allocator } ++ p.args);
    switch (r1) {
        .some => |some| {
            i = some.tail;
            try array.append(some.value);
        },
        .none => |none| return .{
            .none = none,
        },
    }
    while (true) {
        const r = try @call(.auto, p.parser, .{ i, allocator } ++ p.args);
        switch (r) {
            .some => |some| {
                i = some.tail;
                try array.append(some.value);
            },
            .none => break,
        }
    }
    return .{
        .some = .{
            .value = try array.toOwnedSlice(),
            .tail = i,
        },
    };
}

pub fn choice(input: Input, allocator: std.mem.Allocator, parsers_args: anytype) anyerror!Parser.getParserResultType(parsers_args[0].parser) {
    inline for (parsers_args) |elem| {
        const parser = elem.parser;
        const args = elem.args;

        const r1 = try @call(.auto, parser, .{ input, allocator } ++ args);
        switch (r1) {
            .some => return r1,
            .none => {},
        }
    }
    return Parser.Result(u8){
        .none = "Parsers failed",
    };
}

pub fn option(input: Input, allocator: std.mem.Allocator, p: anytype, value: Parser.getParserResultType(p).T) anyerror!Parser.getParserResultType(p) {
    const r = try @call(.auto, p.parser, .{ input, allocator } ++ p.args);
    switch (r) {
        .some => return r,
        .none => return .{ .some = .{
            .value = value,
            .tail = input,
        } },
    }
}

pub fn optionMaybe(input: Input, allocator: std.mem.Allocator, p: anytype) blk: {
    const ResultType = Parser.getParserResultType(p.parser);
    break :blk anyerror!Parser.Result(?ResultType.T);
} {
    const r = try @call(.auto, p.parser, .{ input, allocator } ++ p.args);
    switch (r) {
        .some => return r,
        .none => return .{ .some = .{
            .value = null,
            .tail = input,
        } },
    }
}

pub fn between(input: Input, allocator: std.mem.Allocator, start: anytype, elem: anytype, end: anytype) anyerror!Parser.getParserResultType(elem.parser) {
    const s = try @call(.auto, start.parser, .{ input, allocator } ++ start.args);
    switch (s) {
        .some => |start_some| {
            const e = try @call(.auto, elem.parser, .{ start_some.tail, allocator } ++ elem.args);
            switch (e) {
                .some => |elem_some| {
                    return switch (try @call(.auto, end.parser, .{ elem_some.tail, allocator } ++ end.args)) {
                        .some => |end_some| .{ .some = .{
                            .value = elem_some.value,
                            .tail = end_some.tail,
                        } },
                        .none => |none| .{
                            .none = none,
                        },
                    };
                },
                .none => |none| {
                    return .{
                        .none = none,
                    };
                },
            }
        },
        .none => |none| {
            return .{
                .none = none,
            };
        },
    }
}

pub fn count(input: Input, allocator: std.mem.Allocator, n: usize, p: anytype) blk: {
    const ResultType = Parser.getParserResultType(p.parser);
    break :blk anyerror!Parser.Result([]ResultType.T);
} {
    const ResultType = Parser.getParserResultType(p.parser);

    var array = std.ArrayList(ResultType.T).init(allocator);
    errdefer array.deinit();
    var i = input;
    for (0..n) |_| {
        const r = try @call(.auto, p.parser, .{ i, allocator } ++ p.args);
        switch (r) {
            .some => |some| {
                i = some.tail;
                try array.append(some.value);
            },
            .none => break,
        }
    }
    if (array.items.len != n) {
        array.deinit();
        return .{
            .none = "Couldn't parse enough elements",
        };
    }
    return .{ .some = .{
        .value = try array.toOwnedSlice(),
        .tail = i,
    } };
}

pub fn sepBy(input: Input, allocator: std.mem.Allocator, p: anytype, sep: anytype) blk: {
    const ResultType = Parser.getParserResultType(p.parser);
    break :blk anyerror!Parser.Result([]ResultType.T);
} {
    const ResultType = Parser.getParserResultType(p.parser);
    const Res = Parser.Result([]ResultType.T);

    var array = std.ArrayList(ResultType.T).init(allocator);
    var i = input;
    const r1 = try @call(.auto, p.parser, .{ i, allocator } ++ p.args);
    switch (r1) {
        .some => |some| {
            i = some.tail;
            try array.append(some.value);
        },
        .none => return .{
            .some = .{
                .value = &.{},
                .tail = i,
            },
        },
    }
    while (run: {
        switch (try @call(.auto, sep.parser, .{ i, allocator } ++ sep.args)) {
            .some => |some| {
                i = some.tail;
                break :run true;
            },
            .none => |_| {
                break :run false;
            },
        }
    }) {
        const r = try @call(.auto, p.parser, .{ i, allocator } ++ p.args);
        switch (r) {
            .some => |some| {
                i = some.tail;
                try array.append(some.value);
            },
            .none => |none| {
                array.deinit();
                return .{
                    .none = none,
                };
            },
        }
    }
    return Res{
        .some = .{
            .value = try array.toOwnedSlice(),
            .tail = i,
        },
    };
}

pub fn sepBy1(input: Input, allocator: std.mem.Allocator, p: anytype, sep: anytype) blk: {
    const ResultType = Parser.getParserResultType(p.parser);
    break :blk anyerror!Parser.Result([]ResultType.T);
} {
    const ResultType = Parser.getParserResultType(p.parser);

    var array = std.ArrayList(ResultType.T).init(allocator);
    var i = input;
    const r1 = try @call(.auto, p.parser, .{ i, allocator } ++ p.args);
    switch (r1) {
        .some => |some| {
            i = some.tail;
            try array.append(some.value);
        },
        .none => |none| return .{
            .none = none,
        },
    }
    while (run: {
        switch (try @call(.auto, sep.parser, .{ i, allocator } ++ sep.args)) {
            .some => |some| {
                i = some.tail;
                break :run true;
            },
            .none => |_| {
                break :run false;
            },
        }
    }) {
        const r = try @call(.auto, p.parser, .{ i, allocator } ++ p.args);
        switch (r) {
            .some => |some| {
                i = some.tail;
                try array.append(some.value);
            },
            .none => |none| {
                array.deinit();
                return .{
                    .none = none,
                };
            },
        }
    }
    return .{
        .some = .{
            .value = try array.toOwnedSlice(),
            .tail = i,
        },
    };
}

fn scan(input: Input, allocator: std.mem.Allocator, value: anytype, operator: anytype, apply: anytype) anyerror!Parser.getParserResultType(value.parser) {
    const x = try @call(.auto, value.parser, .{ input, allocator } ++ value.args);
    switch (x) {
        .some => |some| {
            return rest(some.tail, allocator, value, operator, apply, some.value);
        },
        .none => |none| return .{ .none = none },
    }
}

fn rest(input: Input, allocator: std.mem.Allocator, value: anytype, operator: anytype, apply: anytype, x: anytype) anyerror!Parser.Result(@TypeOf(x)) {
    switch (try @call(.auto, operator.parser, .{ input, allocator } ++ operator.args)) {
        .some => |some| {
            switch (try scan(some.tail, allocator, value, operator, apply)) {
                .some => |scan_some| {
                    switch (try @call(.auto, apply.parser, .{ input, allocator } ++ .{some.value} ++ apply.args)) {
                        .some => |apply_some| {
                            return .{
                                .some = .{
                                    .value = try apply_some.value(allocator, x, scan_some.value),
                                    .tail = scan_some.tail,
                                },
                            };
                        },
                        .none => |none| return .{ .none = none },
                    }
                },
                .none => {},
            }
        },
        .none => {},
    }
    return .{ .some = .{
        .value = x,
        .tail = input,
    } };
}

pub fn chainl1(input: Input, allocator: std.mem.Allocator, value: anytype, operator: anytype, apply: anytype) anyerror!Parser.getParserResultType(value.parser) {
    return switch (try scan(input, allocator, value, operator, apply)) {
        .some => |some| rest(some.tail, allocator, value, operator, apply, some.value),
        .none => |none| .{ .none = none },
    };
}
