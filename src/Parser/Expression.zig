const std = @import("std");
const Parser = @import("ZigParsec");
const P = @import("Statement.zig");
const S = struct {
    usingnamespace @import("../AST/Statement.zig");
    usingnamespace @import("../AST/Expression.zig");
};
pub const ExprP = Parser.Expression(*S.Expression);

pub fn MakeBinaryOperatorBuilder(comptime op: S.BinaryExpressionKind) fn (std.mem.Allocator, *S.Expression, *S.Expression) anyerror!*S.Expression {
    return struct {
        pub fn inlineBuilder(allocator: std.mem.Allocator, lhs: *S.Expression, rhs: *S.Expression) anyerror!*S.Expression {
            const n = try allocator.create(S.Expression);
            n.* = .{ .binary = .{
                .kind = op,
                .lhs = lhs,
                .rhs = rhs,
                .type = .{
                    .unresolved = void{},
                },
            } };

            return n;
        }
    }.inlineBuilder;
}

pub fn MakeUnaryOperatorBuidler(comptime op: S.PrefixUnaryExpressionKind) fn (std.mem.Allocator, *S.Expression) anyerror!*S.Expression {
    return struct {
        pub fn inlineBuilder(allocaotor: std.mem.Allocator, rhs: *S.Expression) anyerror!*S.Expression {
            const n = try allocaotor.create(S.Expression);
            n.* = .{ .prefixUnary = .{
                .kind = op,
                .rhs = rhs,
                .type = .{
                    .unresolved = void{},
                },
            } };
            return n;
        }
    }.inlineBuilder;
}

pub fn createParserState(allocator: std.mem.Allocator) !*Parser.ZigParsecState {
    const state = try allocator.create(Parser.ZigParsecState);
    state.* = .{
        .extensions = try ExprP.Operators.createStateExtension(allocator, &.{
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "=", "=" } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.Assignment)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "+=", null } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.AssignmentAdd)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "-=", null } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.AssignmentSub)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "*=", null } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.AssignmentMul)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "/=", null } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.AssignmentDiv)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "<<=", null } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.AssignmentLeftShift)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "=>>", null } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.AssignmentRightShift)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "&=", null } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.AssignmentBitwiseAnd)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "|=", null } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.AssignmentBitwiseOr)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "^=", null } }, .{ .RightAssoc = 10 }, MakeBinaryOperatorBuilder(.AssignmentBitwiseXor)),

            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "or", null } }, .{ .LeftAssoc = 20 }, MakeBinaryOperatorBuilder(.Or)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "and", null } }, .{ .LeftAssoc = 21 }, MakeBinaryOperatorBuilder(.And)),

            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "|", "|=" } }, .{ .LeftAssoc = 30 }, MakeBinaryOperatorBuilder(.BitwiseOr)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "^", "=" } }, .{ .LeftAssoc = 31 }, MakeBinaryOperatorBuilder(.BitwiseXor)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "&", "&=" } }, .{ .LeftAssoc = 32 }, MakeBinaryOperatorBuilder(.BitwiseAnd)),

            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "==", null } }, .{ .LeftAssoc = 40 }, MakeBinaryOperatorBuilder(.Eq)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "!=", null } }, .{ .LeftAssoc = 40 }, MakeBinaryOperatorBuilder(.NotEq)),

            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "<", "=" } }, .{ .LeftAssoc = 50 }, MakeBinaryOperatorBuilder(.LThan)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "<=", null } }, .{ .LeftAssoc = 50 }, MakeBinaryOperatorBuilder(.LThanOrEq)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ ">", "=" } }, .{ .LeftAssoc = 50 }, MakeBinaryOperatorBuilder(.GThan)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ ">=", null } }, .{ .LeftAssoc = 50 }, MakeBinaryOperatorBuilder(.GThanOrEq)),

            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "+", "=" } }, .{ .LeftAssoc = 60 }, MakeBinaryOperatorBuilder(.Add)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "-", "=" } }, .{ .LeftAssoc = 60 }, MakeBinaryOperatorBuilder(.Sub)),

            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "*", "=" } }, .{ .LeftAssoc = 70 }, MakeBinaryOperatorBuilder(.Mul)),
            ExprP.InfixOperator.new(.{ Parser.Language.operator, .{ "/", "=" } }, .{ .LeftAssoc = 70 }, MakeBinaryOperatorBuilder(.Div)),
        }, &.{
            ExprP.PrefixOperator.new(.{ Parser.Language.operator, .{ "-", "=" } }, MakeUnaryOperatorBuidler(.Negate)),
            ExprP.PrefixOperator.new(.{ Parser.Language.operator, .{ "+", "=" } }, MakeUnaryOperatorBuidler(.Plus)),
            ExprP.PrefixOperator.new(.{ Parser.Language.operator, .{ "!", "=" } }, MakeUnaryOperatorBuidler(.Not)),
        }, &.{}),
    };
    return state;
}

pub fn destroyParserState(allocator: std.mem.Allocator, state: *Parser.ZigParsecState) void {
    ExprP.Operators.destroyStateExtension(allocator, state);
    allocator.destroy(state);
}

pub fn integerLiteralP(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(*S.Expression) {
    var end = stream;
    switch (try Parser.Char.digit(stream, allocator, state)) {
        .Result => |res| {
            end = res.rest;
        },
        .Error => |err| return Parser.Result(*S.Expression).failure(err.msg, err.rest),
    }
    while (blk: {
        switch (try Parser.Char.digit(end, allocator, state)) {
            .Result => |res| {
                end = res.rest;
                break :blk true;
            },
            .Error => |err| {
                err.msg.deinit();
                break :blk false;
            },
        }
    }) {}
    const diff = stream.diff(end);

    const i: i64 = try std.fmt.parseInt(i64, diff, 10);

    const literal = try allocator.create(S.Expression);
    literal.* = .{
        .integer = i,
    };
    return Parser.Result(*S.Expression).success(literal, end);
}

pub fn floatLiteralP(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(*S.Expression) {
    var end = stream;
    switch (try Parser.Char.digit(stream, allocator, state)) {
        .Result => |res| {
            end = res.rest;
        },
        .Error => |err| return Parser.Result(*S.Expression).failure(err.msg, err.rest),
    }

    while (blk: {
        switch (try Parser.Char.digit(end, allocator, state)) {
            .Result => |res| {
                end = res.rest;
                break :blk true;
            },
            .Error => |err| {
                err.msg.deinit();
                break :blk false;
            },
        }
    }) {}

    switch (try Parser.Char.symbol(end, allocator, state, '.')) {
        .Result => |res| {
            end = res.rest;
        },
        .Error => |err| return Parser.Result(*S.Expression).failure(err.msg, err.rest),
    }

    switch (try Parser.Char.digit(end, allocator, state)) {
        .Result => |res| {
            end = res.rest;
        },
        .Error => |err| return Parser.Result(*S.Expression).failure(err.msg, err.rest),
    }
    while (blk: {
        switch (try Parser.Char.digit(end, allocator, state)) {
            .Result => |res| {
                end = res.rest;
                break :blk true;
            },
            .Error => |err| {
                err.msg.deinit();
                break :blk false;
            },
        }
    }) {}

    const diff = stream.diff(end);
    const f: f64 = try std.fmt.parseFloat(f64, diff);
    const literal = try allocator.create(S.Expression);
    literal.* = .{
        .float = f,
    };
    return Parser.Result(*S.Expression).success(literal, end);
}

pub fn identifierP(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(*S.Expression) {
    return Parser.map(
        stream,
        allocator,
        state,
        []u8,
        Parser.Language.identifier,
        *S.Expression,
        struct {
            pub fn inlineMap(a: std.mem.Allocator, ident: []u8) anyerror!*S.Expression {
                const n = try a.create(S.Expression);
                n.* = .{
                    .identifier = ident,
                };
                return n;
            }
        }.inlineMap,
    );
}

pub fn subExpressionP(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(*S.Expression) {
    return Parser.Combinator.between(
        stream,
        allocator,
        state,
        .{ u8, *S.Expression, u8 },
        .{ Parser.Char.symbol, .{'('} },
        .{ ExprP.expression, .{term} },
        .{ Parser.Char.symbol, .{')'} },
    );
}

pub fn term(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(*S.Expression) {
    return Parser.Language.eatWhitespaceBefore(stream, allocator, state, *S.Expression, .{ Parser.Combinator.choice, .{
        *S.Expression,
        .{
            subExpressionP,
            floatLiteralP,
            integerLiteralP,
            identifierP,
        },
    } });
}

pub fn expression(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(*S.Expression) {
    return ExprP.expression(stream, allocator, state, term);
}
