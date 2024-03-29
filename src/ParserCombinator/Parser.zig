const std = @import("std");
const Input = @import("Input.zig");

pub const ResultEnum = enum {
    some,
    none,
};

pub fn Result(comptime _T: type) type {
    return union(ResultEnum) {
        pub const T = _T;

        some: struct {
            value: _T,
            tail: Input,
        },
        none: []const u8,

        pub fn map(self: @This(), allocator: std.mem.Allocator, comptime U: type, mapfnc: *const fn (std.mem.Allocator, _T) anyerror!U) anyerror!Result(U) {
            return switch (self) {
                .some => |some| .{
                    .some = .{
                        .value = try mapfnc(allocator, some.value),
                        .tail = some.tail,
                    },
                },
                .none => |none| .{
                    .none = none,
                },
            };
        }
    };
}

pub const Void = Result(void);

pub fn TypeCheckParser(comptime P: anytype) void {
    const T = @TypeOf(P);
    const TInfos = @typeInfo(T);

    switch (TInfos) {
        .Fn => |f| {
            if (f.params[0].type != null and f.params[0].type.? != Input) @panic("Parser first argument must be an Input");
            if (f.params[1].type != null and f.params[1].type.? != std.mem.Allocator) @panic("Parser second argument must be an std.mem.Allocator");

            if (f.return_type) |return_type| {
                switch (@typeInfo(return_type)) {
                    .ErrorUnion => {},
                    else => @panic("Parser should have return value of type anyerror!Result(T)"),
                }
            } else {
                @panic("Parser should have a return value of type anyerror!Result(T)");
            }
        },
        else => @panic("Parser should be a function"),
    }
}

pub fn getParserResultType(comptime parser: anytype) type {
    switch (@typeInfo(@TypeOf(parser))) {
        .Fn => |f| {
            if (f.return_type) |return_type| {
                switch (@typeInfo(return_type)) {
                    .ErrorUnion => |e| {
                        return e.payload;
                    },
                    else => @panic("Parser should have return value of type anyerror!Result(T)"),
                }
            } else {
                @compileLog("Parser should have a return value of type anyerror!Result(T)", @typeName(@TypeOf(parser)));
                @panic("");
            }
        },
        else => @panic("Parser should be a function"),
    }
}
