# Your EBNF for your Rust in Rust language

Try to capture the syntax for your RNR language by means of EBNF rules, see e.g. [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form.).

You don't need to worry about white spaces, we can assume them to be suppressed.

# Data types
```ebnf
digit excluding zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
digit = "0" | digit excluding zero ;
type = "i32" | "bool" | "()";
integer = [ "-" ], {digit};
boolean = "false" | "true";
```

# Expressions
```ebnf
literal = integer | boolean;
op = "-" | "+" | "*" | "/" | "&&" | "||" | "<" | ">" | "==" |;
binop = expr, op, expr;
par = "(", expr, ")";
if_then_else = "if", expr, block, ["else", block];
expr = literal | binop | par | if_then_else;
Call = Ident, "(", [Arguments], ")";
```

# Statements
```ebnf
let = "let", expr, ":", type, "=", expr;
while = "while", expr, block;
assign = expr, "=", expr;
statement = (let | assign | while | expr);
Fn = FnDeclaration;
```

# Block
```ebnf
block = "{", {statement, ";"}, "}";
```

# Functions
```ebnf
Parameter = Ident, ":", Type;
Parameters = {Parameter};
FnDeclaration = "fn", Ident, "(", [Parameters], ")", ["->", Type], Block;
Arguments = {Expr};
```