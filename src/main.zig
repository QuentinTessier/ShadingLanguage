const std = @import("std");
const zParsec = @import("ZigParsec");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    //    const allocator = gpa.allocator();
    const s = zParsec.Stream.init("stream", null);
    std.log.info("{any}", .{s});
}
