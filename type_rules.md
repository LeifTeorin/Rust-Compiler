# Type rules for your Rust in Rust language

See slides Lecture 6 for further details.

Hint: Use inline latex math for GitLab.

# Binary Operators
- $`fn \: \text{Sub}(<a_1:i32>, <a_2:i32>) \to i32`$
- $`fn \: \text{Add}(<a_1:i32>, <a_2:i32>) \to i32`$
- $`fn \: \text{Mul}(<a_1:i32>, <a_2:i32>) \to i32`$
- $`fn \: \text{Div}(<a_1:i32>, <a_2:i32>) \to i32`$
- $`fn \: \text{And}(<a_1:bool>, <a_2:bool>) \to bool`$
- $`fn \: \text{Or}(<a_1:bool>, <a_2:bool>) \to bool`$
- $`fn \: \text{Eq}(<a_1:i32>, <a_2:i32>) \to bool`$
- $`fn \: \text{Gt}(<a_1:i32>, <a_2:i32>) \to bool`$
- $`fn \: \text{Lt}(<a_1:i32>, <a_2:i32>) \to bool`$

# Unary Operators
- $`fn \: \text{Not}(<a_1:bool>) \to bool`$
- $`fn \: \text{Add}(<a_1:i32>, <a_2:i32>) \to i32`$
- $`fn \: \text{Mul}(<a_1:i32>, <a_2:i32>) \to i32`$

# Statements
- $`fn \: \text{Let}(<a_1:Expr>, <a_2:Expr>, <a_3:Expr>) → Unit`$
- $`fn \: \text{Assign} (<a_1:Expr>, <a_2:Expr>) → Unit`$
- $`fn \: \text{Expr} (<a_1:Expr>) → Type$
- $`fn \: \text{While} (<a_1:Expr>, <a_2:Block>) → Type`$
- $`fn \: \text{Fn} (<a_1:Expr>, <a_2:Parameter>, <a_3:Type>, <a_4:Block>) → Type`$

# Expressions
- $`fn \: \text{Ident} (<a_1:String>) → Type`$
- $`fn \: \text{BinOp} (<a_1:Expr>, <a_2:Expr>, <a_3:Expr>) → Type`$
- $`fn \: \text{IfThenOptElse} (<a_1:Expr>, <a_2:Block>, <a_3:Block>) → Type`$
- $`fn \: \text{Par} (<a_1:Expr>) → Type`$
- $`fn \: \text{Call} (<a_1:String>, <a_2:Arguments>) → Type`$
