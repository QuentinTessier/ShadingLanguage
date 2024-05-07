const std = @import("std");
pub const LazyType = @import("Type.zig").LazyType;

const Expression = @import("Expression.zig").Expression;

pub const FunctionArgument = struct {
    name: []u8,
    type: LazyType,

    pub fn deinit(self: FunctionArgument, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.type.deinit(allocator);
    }
};

pub const Stage = enum(u8) {
    Vertex,
    Fragment,
};

pub const Modifier = union(enum(u32)) {
    stage: Stage,
    location: u32,
    binding: u32,
};

// "fn" <modifiers> <identifier> : (<arguments>) -> <type> <block>
pub const EntryDefinition = struct {
    name: []u8,
    modifiers: ?[]Modifier,
    arguments: []FunctionArgument,
    return_type: LazyType,
    body: BlockStatement = &.{},

    pub fn deinit(self: EntryDefinition, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.modifiers) |mods| {
            allocator.free(mods);
        }
        if (self.arguments.len > 0) {
            for (self.arguments) |arg| {
                arg.deinit(allocator);
            }
            allocator.free(self.arguments);
        }
        self.return_type.deinit(allocator);
        for (self.body) |stmt| {
            stmt.deinit(allocator);
            allocator.destroy(stmt);
        }
        allocator.free(self.body);
    }
};

// "fn" <identifier> : (<arguments>) -> <type> <block>
pub const FunctionDefinition = struct {
    name: []u8 = undefined,
    arguments: []FunctionArgument = &.{},
    return_type: LazyType = undefined,
    body: BlockStatement = &.{},

    pub fn deinit(self: FunctionDefinition, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.arguments.len > 0) {
            for (self.arguments) |arg| {
                arg.deinit(allocator);
            }
            allocator.free(self.arguments);
        }
        self.return_type.deinit(allocator);
        for (self.body) |stmt| {
            stmt.deinit(allocator);
            allocator.destroy(stmt);
        }
        allocator.free(self.body);
    }
};

// "pub" <statement>
pub const PublicSymbol = *Statement;

// In this case, we exclude parsers <function> and <entry>
// '{' <statement>* '}'
pub const BlockStatement = []*Statement;

// <expression> ';'
pub const ExpressionStatement = *Expression;

// "const" <identifier> '=' <statement>
pub const ConstantStatement = struct {
    name: []const u8,
    type: LazyType,
    value: *Statement,

    pub fn deinit(self: ConstantStatement, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.type.deinit(allocator);
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }
};

// "var" <identifier> '=' <statement>
pub const VariableStatement = struct {
    name: []const u8,
    type: LazyType,
    value: *Statement,

    pub fn deinit(self: VariableStatement, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.type.deinit(allocator);
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }
};

// <public> | <entry> | <function> | <block> | <expressionStmt> | <constant> | <variable>
pub const Statement = union(enum(u32)) {
    public: PublicSymbol,
    entry: EntryDefinition,
    function: FunctionDefinition,
    block: BlockStatement,
    expression: ExpressionStatement,
    constant: ConstantStatement,
    variable: VariableStatement,

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .entry => |entry| entry.deinit(allocator),
            .function => |fnc| fnc.deinit(allocator),
            .expression => |expr| {
                expr.deinit(allocator);
                allocator.destroy(expr);
            },
            .block => |stmts| {
                for (stmts) |stmt| {
                    stmt.deinit(allocator);
                    allocator.destroy(stmt);
                }
                allocator.free(stmts);
            },
            .public => |p| {
                p.deinit(allocator);
                allocator.destroy(p);
            },
            .constant => |c| {
                c.deinit(allocator);
            },
            .variable => |v| {
                v.deinit(allocator);
            },
        }
    }
};
