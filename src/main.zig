const std = @import("std");
const Parser = @import("ParserCombinator/Parser.zig");
const Input = @import("ParserCombinator/Input.zig");
const Char = @import("ParserCombinator/Char.zig");
const Utils = @import("ParserCombinator/Utils.zig");
const Combinator = @import("ParserCombinator/Combinator.zig");

pub const BinaryExpression = struct {
    operator: u8,
    left: *Expression,
    right: *Expression,

    pub fn makeAdd(allocator: std.mem.Allocator, left: *Expression, right: *Expression) anyerror!*Expression {
        const new = try allocator.create(Expression);
        new.* = .{
            .binary = .{
                .operator = '+',
                .left = left,
                .right = right,
            },
        };

        return new;
    }

    pub fn makeSub(allocator: std.mem.Allocator, left: *Expression, right: *Expression) anyerror!*Expression {
        const new = try allocator.create(Expression);
        new.* = .{
            .binary = .{
                .operator = '-',
                .left = left,
                .right = right,
            },
        };

        return new;
    }

    pub fn makeMul(allocator: std.mem.Allocator, left: *Expression, right: *Expression) anyerror!*Expression {
        const new = try allocator.create(Expression);
        new.* = .{
            .binary = .{
                .operator = '*',
                .left = left,
                .right = right,
            },
        };

        return new;
    }

    pub fn makeDiv(allocator: std.mem.Allocator, left: *Expression, right: *Expression) anyerror!*Expression {
        const new = try allocator.create(Expression);
        new.* = .{
            .binary = .{
                .operator = '/',
                .left = left,
                .right = right,
            },
        };

        return new;
    }
};

pub const Expression = union(enum(u32)) {
    literal: Literal,
    identifier: []u8,
    binary: BinaryExpression,

    pub fn format(expr: *const Expression, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (expr.*) {
            .literal => |lit| std.fmt.format(writer, "{}", .{lit}),
            .identifier => |ident| std.fmt.format(writer, "{s}", .{ident}),
            .binary => |bin| std.fmt.format(writer, "{c} {} {}", .{ bin.operator, bin.left, bin.right }),
        };
    }

    pub fn deinit(self: *const Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .literal => {},
            .identifier => |ident| allocator.free(ident),
            .binary => |bin| {
                bin.left.deinit(allocator);
                allocator.destroy(bin.left);
                bin.right.deinit(allocator);
                allocator.destroy(bin.right);
            },
        }
    }
};

pub const Literal = u32;

pub fn addOp(input: Input, _: std.mem.Allocator, operator_char: u8) anyerror!Parser.Result(*const fn (std.mem.Allocator, *Expression, *Expression) anyerror!*Expression) {
    return switch (operator_char) {
        '+' => .{
            .some = .{
                .value = BinaryExpression.makeAdd,
                .tail = input,
            },
        },
        '-' => .{
            .some = .{
                .value = BinaryExpression.makeSub,
                .tail = input,
            },
        },
        else => .{
            .none = "Operator not supported",
        },
    };
}

pub fn mulOp(input: Input, _: std.mem.Allocator, operator_char: u8) anyerror!Parser.Result(*const fn (std.mem.Allocator, *Expression, *Expression) anyerror!*Expression) {
    return switch (operator_char) {
        '*' => .{
            .some = .{
                .value = BinaryExpression.makeMul,
                .tail = input,
            },
        },
        '/' => .{
            .some = .{
                .value = BinaryExpression.makeDiv,
                .tail = input,
            },
        },
        else => .{
            .none = "Operator not supported",
        },
    };
}

pub fn factor(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(*Expression) {
    const expr = try primary(input, allocator);
    switch (expr) {
        .some => return expr,
        .none => {
            return Combinator.between(
                input,
                allocator,
                .{ .parser = Char.symbol, .args = .{'('} },
                .{ .parser = expression, .args = .{} },
                .{ .parser = Char.symbol, .args = .{')'} },
            );
        },
    }
}

pub fn term(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(*Expression) {
    return Combinator.chainl1(
        input,
        allocator,
        .{ .parser = factor, .args = .{} },
        .{ .parser = Combinator.choice, .args = .{.{ .{ .parser = Char.symbol, .args = .{'*'} }, .{ .parser = Char.symbol, .args = .{'/'} } }} },
        .{ .parser = mulOp, .args = .{} },
    );
}

pub fn expression(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(*Expression) {
    return Combinator.chainl1(
        input,
        allocator,
        .{ .parser = term, .args = .{} },
        .{ .parser = Char.symbol, .args = .{'+'} },
        .{ .parser = addOp, .args = .{} },
    );
}

pub fn primary(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(*Expression) {
    const i = try (try Utils.integer(input, allocator, u32)).map(allocator, *Expression, struct {
        pub fn mapfnc(a: std.mem.Allocator, integer: u32) anyerror!*Expression {
            const e = try a.create(Expression);
            e.* = .{ .literal = integer };
            return e;
        }
    }.mapfnc);
    switch (i) {
        .some => |_| {
            return i;
        },
        .none => {
            return (try Utils.identifier(input, allocator)).map(allocator, *Expression, struct {
                pub fn mapfnc(a: std.mem.Allocator, ident: []u8) anyerror!*Expression {
                    const e = try a.create(Expression);
                    e.* = .{ .identifier = ident };
                    return e;
                }
            }.mapfnc);
        },
    }
}

pub fn expressions(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result([]*Expression) {
    return Combinator.many(input, allocator, .{ .parser = struct {
        pub fn expression_semicolon(input_: Input, allocator_: std.mem.Allocator) anyerror!Parser.Result(*Expression) {
            switch (try expression(input_, allocator_)) {
                .some => |some| {
                    return switch (try Char.symbol(some.tail, allocator_, ';')) {
                        .some => |semi| .{ .some = .{
                            .value = some.value,
                            .tail = semi.tail,
                        } },
                        .none => |none| .{ .none = none },
                    };
                },
                .none => |none| return .{ .none = none },
            }
        }
    }.expression_semicolon, .args = .{} });
}

pub fn wrapperChoice(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(u8) {
    return Combinator.choice(input, allocator, .{
        .{ .parser = Char.symbol, .args = .{'_'} },
        .{ .parser = Char.alphaNum, .args = .{} },
    });
}

pub fn choice_u8(input: Input, allocator: std.mem.Allocator, args: anytype) anyerror!Parser.Result(u8) {
    return Combinator.choice(input, allocator, args);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const input = Input.init("____a", null);
    const r = try Combinator.many(input, allocator, .{
        .parser = Combinator.choice,
        .args = .{.{
            .{ .parser = Char.symbol, .args = .{'_'} },
            .{ .parser = Char.alphaNum, .args = .{} },
        }},
    });
    switch (r) {
        .some => |c| {
            std.log.info("{}", .{c});
            allocator.free(c.value);
            //for (c.value) |expr| {
            //    std.log.info("{}", .{expr});
            //    expr.deinit(allocator);
            //    allocator.destroy(expr);
            //}
            //allocator.free(c.value);
        },
        .none => |message| std.log.err("{s}", .{message}),
    }
}
