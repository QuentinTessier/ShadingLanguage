const std = @import("std");

pub const Header = struct {
    pub const MagicNumber: u32 = 0x07230203;

    magic: u32,
    version: u32,
    generator: u32,
    id: u32,
    schema: u32,
    offset: u32,
};
