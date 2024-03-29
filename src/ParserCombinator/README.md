This parser combinator library follows Haskell's combinator lib Parsec.

```zig
// All parsers follow the same definition:
pub fn my_parser(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(ResultType);

// Optionally parser can also declare custom arguments:
pub fn my_parser_with_args(input: Input, allocator: std.mem.Allocator, arg1: u32) anyerror!Parser.Result(ResultType);

// One of kind of argument a parser can take has argument is another Parser, comptime tuples are use for this:
pub fn my_parser_with_parser_arg(input: Input, allocator: std.mem.Allocator, parser: anytype) anyerror!Parser.Result(ResultType);

my_parser_with_parser_arg(input, allocator, .{ .parser = my_parser_with_args, .args = .{ @as(u32, 1) } });

// Parsers returns a Parser.Result(comptime T: type), this type is a union:
union(enum(u32)) {
    some: struct {
        value: T, // The parsed value
        tail: Input, // Remaining input stream
    },
    .none : []const u8, // An error message (currently kind of static and not very practical. Will be improved when the need is felt)
}

// You can check if a parser failed or succeed with the following syntax:
switch(try my_parser(input, allocator)) {
    .some => |some| {
        std.log.info("Parsed data: {}, Tail: {}", .{some.value, some.tail});
    },
    .none => |none| {
        std.log.info("Failed to parse with message: {s}", .{none});
    }
}
```



Basic parsers:

| Parser                | Description                                                                                 |
| --------------------- | ------------------------------------------------------------------------------------------- |
| symbol(u8)            | Try to match the given char                                                                 |
| anySymbol()           | Try to match any char                                                                       |
| range(a: u8, b: u8)   | Try to match any char between a and b                                                       |
| oneOf([]u8)           | Try to match any char in the given list                                                     |
| noneOf([]u8)          | Try to match any char outside of the given list                                             |
| digit()               | Try to match '0', '1', '2', '3', '4', '5', '6', '7', '8' and '9'                            |
| lowercase()           | Try to match a lowercase letter                                                             |
| uppercase()           | Try to match a uppercase letter                                                             |
| whitespace()          | Try to match '\t', '\n', '\r' and ' '                                                       |
| alpha()               | Try to match a lowercase/uppercase letter                                                   |
| alphaNum()            | Try to match a lowercase/uppercase letter or a digit                                        |
| octDigit()            | Try to match '0', '1', '2', '3', '4', '5', '6' and '7'                                      |
| hexDigit()            | Try to match '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' |
| satisfy(fn (u8) bool) | Try to match a char that satisfy the given predicate                                        |
| keyword([]const u8)   | Try to match the given string                                                               |

The notation:
A uppercase letter represent a type
Parser(T) is a parser returning type T


Combinators parsers:
| Parser                                       | Description                                                                                |
| -------------------------------------------- | ------------------------------------------------------------------------------------------ |
| many(p: Parser(T)) []T                       | Try to parse 0 or more T using the given parser 'p'                                        |
| many1(p: Parser(T)) []T                      | Try to parse at least one or more T using the given parser 'p'                             |
| choice(ps: []Parser(T)) T                    | Loops over the given parsers and return the first successful one or nothing                |
| option(p: Parser(T), fallback: T)            | Try to parse using the given parser 'p', if it fails gives the 'fallback' value            |
| optionMaybe(p: Parser(T)) ?T                 | Try to parse using the given parser 'p', if it fails return nulls                          |
| count(p: Parser(T), n: usize)                | Try to parse 'n' times using parser 'p'                                                    |
| sepBy(value: Parser(T), sep: Parser(T)) []T  | Try to parse 0 or more T using the given parser 'value' seperated by parser 'sep'          |
| sepBy1(value: Parser(T), sep: Parser(T)) []T | Try to parse at least 1 or more T using the given parser 'value' seperated by parser 'sep' |
| chainl1(Parser(T), Parser(fn (T, T) T)) T    |                                                                                            |