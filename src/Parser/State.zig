const std = @import("std");
const Parser = @import("ZigParsec");

pub const Scope = enum(u32) {
    Global,
    Local,
};

pub const UserState = struct {
    current_scope: Scope = .Global,
};

pub const StateTypeDefinition: type = Parser.MakeUserStateType(UserState);
