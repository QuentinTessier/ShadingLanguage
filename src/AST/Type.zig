const std = @import("std");

pub const LazyType = union(enum(u32)) {
    named: []u8,
    resolved: void, // Will point to a central type definition section
    unresolved: void,

    pub fn format(self: LazyType, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (self) {
            .named => |s| std.fmt.format(writer, "{s}", .{s}),
            else => std.fmt.format(writer, "not impl", .{}),
        };
    }

    pub fn deinit(self: LazyType, allocator: std.mem.Allocator) void {
        switch (self) {
            .named => |s| allocator.free(s),
            else => {},
        }
    }
};
