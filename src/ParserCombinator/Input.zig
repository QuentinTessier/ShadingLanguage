const std = @import("std");

pub const Input = @This();

pub const Location = struct {
    index: u64 = 0,
    line: u64 = 1,
    character: u64 = 1,

    pub fn format(self: Location, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return std.fmt.format(writer, "{}:{}({})", .{ self.line, self.character, self.index });
    }
};

data: []const u8,
label: ?[]const u8,
currentLocation: Location,
eatWhitespace: bool = true,

pub fn init(bytes: []const u8, label: ?[]const u8) Input {
    return .{
        .data = bytes,
        .label = label,
        .currentLocation = .{},
    };
}

pub fn remaining(self: Input) usize {
    return self.data.len - self.currentLocation.index;
}

pub fn diff(self: Input, tail: Input) []const u8 {
    std.debug.assert(self.currentLocation.index < tail.currentLocation.index);
    return self.data[self.currentLocation.index..tail.currentLocation.index];
}

pub fn peek(self: Input, length: ?usize) []const u8 {
    const len: usize = if (length) |l| l else 1;
    const start = self.currentLocation.index;
    const end = @min(start + len, self.data.len);

    return self.data[start..end];
}

pub fn isEOF(self: Input) bool {
    return self.currentLocation.index >= self.data.len;
}

pub fn eat(self: Input, length: usize) Input {
    const start = self.currentLocation.index;
    const next = @min(self.data.len, start + length);

    var new = self;

    var i = start;
    while (i < next) : (i += 1) {
        switch (self.data[i]) {
            '\n' => {
                new.currentLocation.line += 1;
                new.currentLocation.character = 1;
            },
            else => new.currentLocation.character += 1,
        }
    }

    while (self.eatWhitespace and i < self.data.len and std.ascii.isWhitespace(self.data[i])) : (i += 1) {
        switch (self.data[i]) {
            '\n' => {
                new.currentLocation.line += 1;
                new.currentLocation.character = 1;
            },
            else => new.currentLocation.character += 1,
        }
    }

    new.currentLocation.index = i;

    return new;
}

pub fn format(self: Input, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    return std.fmt.format(writer, "{s}:{}:{}", .{ if (self.label) |label| label else "(null)", self.currentLocation.line, self.currentLocation.character });
}
