This parser combinator library follows Haskell's combinator lib Parsec.

```
// All parsers follow the same definition:
pub fn my_parser(input: Input, allocator: std.mem.Allocator) anyerror!Parser.Result(ResultType);

// Optionally parser can also declare custom arguments:
pub fn my_parser_with_args(input: Input, allocator: std.mem.Allocator, arg1: u32) anyerror!Parser.Result(ResultType);

// One of kind of argument a parser can take has argument is another Parser, comptime tuples are use for this:
pub fn my_parser_with_parser_arg(input: Input, allocator: std.mem.Allocator, parser: anytype) anyerror!Parser.Result(ResultType);

my_parser_with_parser_arg(input, allocator, .{ .parser = my_parser_with_args, .args = .{ @as(u32, 1) } });

```



Here are the basic parsers found in the library:

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