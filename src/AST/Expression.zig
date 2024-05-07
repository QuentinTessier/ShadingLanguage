const std = @import("std");
pub const LazyType = @import("Type.zig").LazyType;

pub const IntegerLiteral = i64;
pub const FloatLiteral = f64;

pub const BinaryExpressionKind = enum(u32) {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    And, // and
    Or, // or
    Eq, // ==
    NotEq, // !=
    GThan, // >
    LThan, // <
    GThanOrEq, // >=
    LThanOrEq, // <=
    BitwiseNot, // ~
    BitwiseAnd, // &
    BitwiseOr, // |
    BitwiseXor, // ^
    LeftShift, // <<
    RightShift, // >>

    Assignment, // =
    AssignmentAdd, // +=
    AssignmentSub, // -=
    AssignmentMul, // *=
    AssignmentDiv, // /=
    AssignmentMod, // %=
    AssignmentBitwiseAnd, // &=
    AssignmentBitwiseOr, // |=
    AssignmentBitwiseXor, // ^=
    AssignmentLeftShift, // <<=
    AssignmentRightShift, // >>=

    Dot, // .
};

pub const BinaryExpression = struct {
    kind: BinaryExpressionKind,
    lhs: *Expression,
    rhs: *Expression,
    type: LazyType,
};

pub const PrefixUnaryExpressionKind = enum(u32) {
    Negate, // -
    Plus, // +
    Not, // !
    BitwiseNot, // ~
};

pub const PrefixUnaryExpression = struct {
    kind: PrefixUnaryExpressionKind,
    rhs: *Expression,
    type: LazyType,
};

pub const PostfixUnaryExpressionKind = enum(u32) { None };

pub const PostfixUnaryExpression = struct {
    kind: PostfixUnaryExpressionKind,
    rhs: *Expression,
    type: LazyType,
};

pub const Expression = union(enum(u32)) {
    integer: IntegerLiteral,
    float: FloatLiteral,
    identifier: []u8,
    binary: BinaryExpression,
    prefixUnary: PrefixUnaryExpression,
    postfixUnary: PostfixUnaryExpression,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .identifier => |s| allocator.free(s),
            .binary => |b| {
                b.lhs.deinit(allocator);
                b.rhs.deinit(allocator);
                allocator.destroy(b.lhs);
                allocator.destroy(b.rhs);
            },
            .prefixUnary => |preU| {
                preU.rhs.deinit(allocator);
                allocator.destroy(preU.rhs);
            },
            .postfixUnary => |postU| {
                postU.rhs.deinit(allocator);
                allocator.destroy(postU.rhs);
            },
            else => {},
        }
    }
};
