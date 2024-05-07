const std = @import("std");
const Parser = @import("ZigParsec");
const S = @import("../AST/Statement.zig");

const expression = @import("Expression.zig").expression;

const Result = Parser.Result(*S.Statement);

pub fn typeP(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.LazyType) {
    return Parser.map(stream, allocator, state, []u8, Parser.Language.identifier, S.LazyType, struct {
        pub fn inlineMap(_: std.mem.Allocator, name: []u8) anyerror!S.LazyType {
            return .{
                .named = name,
            };
        }
    }.inlineMap);
}

pub fn optionalTypeP(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.LazyType) {
    return switch (try Parser.Language.eatWhitespaceBefore(stream, allocator, state, u8, .{ Parser.Char.symbol, .{':'} })) {
        .Result => |res| typeP(res.rest, allocator, state),
        .Error => |err| blk: {
            err.msg.deinit();
            break :blk Parser.Result(S.LazyType).success(.{ .unresolved = void{} }, stream);
        },
    };
}

pub fn funArg(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.FunctionArgument) {
    return Parser.toStruct(stream, allocator, state, S.FunctionArgument, &.{
        &.{ .name, []u8, .{ Parser.Language.eatWhitespaceBefore, .{ []u8, Parser.Language.identifier } } },
        &.{ void, u8, .{ Parser.Language.eatWhitespaceBefore, .{ u8, .{ Parser.Char.symbol, .{':'} } } } },
        &.{ .type, S.LazyType, .{ Parser.Language.eatWhitespaceBefore, .{ S.LazyType, typeP } } },
    });
}

pub fn funArgs(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result([]S.FunctionArgument) {
    return Parser.Combinator.between(
        stream,
        allocator,
        state,
        .{ u8, []S.FunctionArgument, u8 },
        .{ Parser.Char.symbol, .{'('} },
        .{ Parser.Combinator.sepBy, .{ S.FunctionArgument, funArg, u8, .{ Parser.Char.symbol, .{','} } } },
        .{ Parser.Char.symbol, .{')'} },
    );
}

pub fn funDefinition(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.FunctionDefinition) {
    return Parser.toStruct(
        stream,
        allocator,
        state,
        S.FunctionDefinition,
        &.{
            .{ void, []const u8, .{ Parser.Language.eatWhitespaceBefore, .{ []const u8, .{ Parser.Language.reserved, .{"fn"} } } } },
            .{ .name, []u8, .{ Parser.Language.eatWhitespaceBefore, .{ []u8, Parser.Language.identifier } } },
            .{ void, u8, .{ Parser.Language.eatWhitespaceBefore, .{ u8, .{ Parser.Char.symbol, .{':'} } } } },
            .{ .arguments, []S.FunctionArgument, .{ Parser.Language.eatWhitespaceBefore, .{ []S.FunctionArgument, funArgs } } },
            .{ void, []const u8, .{ Parser.Language.eatWhitespaceBefore, .{ []const u8, .{ Parser.Language.reserved, .{"->"} } } } },
            .{ .return_type, S.LazyType, .{ Parser.Language.eatWhitespaceBefore, .{ S.LazyType, typeP } } },
            .{ .body, S.BlockStatement, .{ Parser.Language.eatWhitespaceBefore, .{ S.BlockStatement, blockDefinition } } },
        },
    );
}

pub fn entryDefinition(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.EntryDefinition) {
    return Parser.toStruct(
        stream,
        allocator,
        state,
        S.EntryDefinition,
        &.{
            .{ void, []const u8, .{ Parser.Language.eatWhitespaceBefore, .{ []const u8, .{ Parser.Language.reserved, .{"entry"} } } } },
            .{ .modifiers, ?[]S.Modifier, .{ Parser.Language.eatWhitespaceBefore, .{ ?[]S.Modifier, maybeModifiers } } },
            .{ .name, []u8, .{ Parser.Language.eatWhitespaceBefore, .{ []u8, Parser.Language.identifier } } },
            .{ void, u8, .{ Parser.Language.eatWhitespaceBefore, .{ u8, .{ Parser.Char.symbol, .{':'} } } } },
            .{ .arguments, []S.FunctionArgument, .{ Parser.Language.eatWhitespaceBefore, .{ []S.FunctionArgument, funArgs } } },
            .{ void, []const u8, .{ Parser.Language.eatWhitespaceBefore, .{ []const u8, .{ Parser.Language.reserved, .{"->"} } } } },
            .{ .return_type, S.LazyType, .{ Parser.Language.eatWhitespaceBefore, .{ S.LazyType, typeP } } },
            .{ .body, S.BlockStatement, .{ Parser.Language.eatWhitespaceBefore, .{ S.BlockStatement, blockDefinition } } },
        },
    );
}

pub fn expressionDefinition(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.ExpressionStatement) {
    switch (try expression(stream, allocator, state)) {
        .Result => |res| switch (try Parser.Language.eatWhitespaceBefore(
            res.rest,
            allocator,
            state,
            u8,
            .{ Parser.Char.symbol, .{';'} },
        )) {
            .Result => |res1| return Parser.Result(S.ExpressionStatement).success(res.value, res1.rest),
            .Error => |err| {
                res.value.deinit(allocator);
                allocator.destroy(res.value);
                return Parser.Result(S.ExpressionStatement).failure(err.msg, err.rest);
            },
        },
        .Error => |err| return Parser.Result(S.ExpressionStatement).failure(err.msg, err.rest),
    }
}

pub fn blockDefinition(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.BlockStatement) {
    return Parser.Combinator.between(
        stream,
        allocator,
        state,
        .{ u8, []*S.Statement, u8 },
        .{ Parser.Language.eatWhitespaceBefore, .{ u8, .{ Parser.Char.symbol, .{'{'} } } },
        .{ Parser.Combinator.many, .{ *S.Statement, statement } },
        .{ Parser.Language.eatWhitespaceBefore, .{ u8, .{ Parser.Char.symbol, .{'}'} } } },
    );
}

pub fn publicDefinition(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.PublicSymbol) {
    return switch (try Parser.Language.reserved(stream, allocator, state, "pub")) {
        .Result => |res| switch (try statement(res.rest, allocator, state)) {
            .Result => |s| Parser.Result(S.PublicSymbol).success(s.value, s.rest),
            .Error => |err| Parser.Result(S.PublicSymbol).failure(err.msg, err.rest),
        },
        .Error => |err| Parser.Result(S.PublicSymbol).failure(err.msg, err.rest),
    };
}

pub fn constantDefinition(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.ConstantStatement) {
    return Parser.toStruct(stream, allocator, state, S.ConstantStatement, &.{
        .{ void, []const u8, .{ Parser.Language.reserved, .{"const"} } },
        .{ .name, []u8, .{ Parser.Language.eatWhitespaceBefore, .{ []u8, Parser.Language.identifier } } },
        .{ .type, S.LazyType, optionalTypeP },
        .{ void, []const u8, .{ Parser.Language.operator, .{ "=", "=" } } },
        .{ .value, *S.Statement, statement },
    });
}

pub fn stageVertexP(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.Stage) {
    return switch (try Parser.Language.reserved(stream, allocator, state, "vertex")) {
        .Result => |res| Parser.Result(S.Stage).success(.Vertex, res.rest),
        .Error => |err| Parser.Result(S.Stage).failure(err.msg, err.rest),
    };
}

pub fn stageFragmentP(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.Stage) {
    return switch (try Parser.Language.reserved(stream, allocator, state, "fragment")) {
        .Result => |res| Parser.Result(S.Stage).success(.Fragment, res.rest),
        .Error => |err| Parser.Result(S.Stage).failure(err.msg, err.rest),
    };
}

pub const Modifier = union(enum(u32)) {
    stage: S.Stage,
    location: u32,
    binding: u32,
};

pub fn modifier(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.Modifier) {
    return Parser.Language.eatWhitespaceBefore(stream, allocator, state, S.Modifier, .{
        Parser.toTaggedUnion,
        .{
            S.Modifier, &.{
                .{ .stage, S.Stage, stageVertexP },
                .{ .stage, S.Stage, stageFragmentP },
            },
        },
    });
}

pub fn modifiers(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result([]S.Modifier) {
    return Parser.Combinator.between(
        stream,
        allocator,
        state,
        .{ u8, []S.Modifier, u8 },
        .{ Parser.Language.eatWhitespaceBefore, .{ u8, .{ Parser.Char.symbol, .{'<'} } } },
        .{ Parser.Combinator.sepBy1, .{
            S.Modifier,
            modifier,
            u8,
            .{ Parser.Language.eatWhitespaceBefore, .{ u8, .{ Parser.Char.symbol, .{','} } } },
        } },
        .{ Parser.Language.eatWhitespaceBefore, .{ u8, .{ Parser.Char.symbol, .{'>'} } } },
    );
}

pub fn maybeModifiers(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(?[]S.Modifier) {
    return switch (try Parser.Language.eatWhitespaceBefore(
        stream,
        allocator,
        state,
        u8,
        .{ Parser.Char.symbol, .{'@'} },
    )) {
        .Result => |res| Parser.Combinator.optionMaybe(res.rest, allocator, state, []S.Modifier, .{
            Parser.Combinator.choice, .{
                []S.Modifier, .{
                    modifiers, .{
                        Parser.map, .{ S.Modifier, modifier, []S.Modifier, struct {
                            pub fn inlineMap(a: std.mem.Allocator, m: S.Modifier) anyerror![]S.Modifier {
                                const x = try a.alloc(S.Modifier, 1);
                                x[0] = m;
                                return x;
                            }
                        }.inlineMap },
                    },
                },
            },
        }),
        .Error => |err| Parser.Result(?[]S.Modifier).failure(err.msg, err.rest),
    };
}

pub fn statementP(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(S.Statement) {
    return Parser.toTaggedUnion(stream, allocator, state, S.Statement, .{
        .{ .public, S.PublicSymbol, publicDefinition },
        .{ .entry, S.EntryDefinition, entryDefinition },
        .{ .function, S.FunctionDefinition, funDefinition },
        .{ .constant, S.ConstantStatement, constantDefinition },
        .{ .block, S.BlockStatement, blockDefinition },
        .{ .expression, S.ExpressionStatement, expressionDefinition },
    });
}

pub fn promote(allocator: std.mem.Allocator, stmt: S.Statement) anyerror!*S.Statement {
    const s = try allocator.create(S.Statement);
    s.* = stmt;
    return s;
}

pub fn statement(stream: Parser.Stream, allocator: std.mem.Allocator, state: *Parser.ZigParsecState) anyerror!Parser.Result(*S.Statement) {
    return Parser.map(stream, allocator, state, S.Statement, statementP, *S.Statement, promote);
}
