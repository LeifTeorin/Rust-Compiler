use crate::ast::{Block, Expr, FnDeclaration, Literal, Op, Prog, Statement, UnOp};
use crate::climb::climb;
use crate::common::Eval;
use crate::env::{Env, Ref};
use crate::error::Error;
use crate::intrinsics::vm_println;

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Lit(Literal),
    Ref(Ref),
    UnInit,
    Mut(Box<Val>),
}

// Helpers for Val
// Alternatively implement the TryFrom trait
impl Val {
    pub fn get_bool(&self) -> Result<bool, Error> {
        match self {
            Val::Lit(Literal::Bool(b)) => Ok(*b),
            _ => Err(format!("cannot get Bool from {:?}", self)),
        }
    }

    pub fn get_int(&self) -> Result<i32, Error> {
        match self {
            Val::Lit(Literal::Int(i)) => Ok(*i),
            Val::Mut(b) => (*b).get_int(),
            _ => Err(format!("cannot get integer from {:?}", self)),
        }
    }

    pub fn get_string(&self) -> Result<Literal, Error> {
        match self {
            Val::Lit(s) => Ok(s.clone()),
            _ => Err(format!("cannot get string from {:?}", self)),
        }
    }
}

// Helper for Op
impl Op {
    // Evaluate operator to literal
    pub fn eval(&self, left: Val, right: Val) -> Result<Val, Error> {
        use Literal::{Bool, Int};
        match self {
            Op::Add => Ok(Val::Lit(Int(left.get_int()? + right.get_int()?))),
            Op::Sub => Ok(Val::Lit(Int(left.get_int()? - right.get_int()?))),
            Op::Mul => Ok(Val::Lit(Int(left.get_int()? * right.get_int()?))),
            Op::Div => Ok(Val::Lit(Int(left.get_int()? / right.get_int()?))),
            Op::And => Ok(Val::Lit(Bool(left.get_bool()? && right.get_bool()?))),
            Op::Or => Ok(Val::Lit(Bool(left.get_bool()? || right.get_bool()?))),
            Op::Eq => Ok(Val::Lit(Bool(left == right))), // overloading
            Op::Lt => Ok(Val::Lit(Bool(left.get_int()? < right.get_int()?))),
            Op::Gt => Ok(Val::Lit(Bool(left.get_int()? > right.get_int()?))),
        }
    }
}

fn get_val_from_mut(input: Val) -> Val {
    match input {
        Val::Mut(b) => *b,
        _ => input
    }
}

impl Eval<Val> for Expr {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        match self {
            Expr::Ident(id) => 
            match env.v.get(&id){
                Some(t) => Ok((get_val_from_mut(t), env.v.get_ref(&id))),
                None => Err("Variable not found".to_string()),
            },
            Expr::Lit(l) => Ok((Val::Lit(l.clone()), None)),
            Expr::BinOp(op, left, right) => Ok((op.eval(left.eval(env)?.0, right.eval(env)?.0)?, None)),
            Expr::Par(e) => e.eval(env),
            Expr::IfThenElse(c, t, e) => match c.eval(env)?.0.get_bool()? {
                true => (t).eval(env),
                false => match e {
                    Some(e) => e.eval(env),
                    None => Ok((Val::UnInit, None)),
                },
            },
            Expr::Block(bl) => bl.eval(env),
            Expr::Call(id, params) => {
                if !(env.f.0.contains_key(id)){
                    return Err("This function has not been declared anywhere".to_string());
                }
                let tempenv = env.clone();
                let func = tempenv.f.0.get(id).unwrap();
                if func.0.id == "println!"{
                    let mut arg_vec: Vec<Literal> = Vec::new();
                    for arg in params.0.iter() {
                        arg_vec.push(arg.eval(env)?.0.get_string()?);
                    }
                    Ok((Val::Lit(func.1.unwrap()(arg_vec)), None))
                }else{
                    env.v.push_scope();
                    let mut i = 0;
                    for arg in params.0.clone(){
                        let arg_id = func.0.parameters.0[i].id.clone();
                        let arg_val = arg.eval(env)?.0;
                        env.v.alloc(&arg_id, arg_val.clone());
                        i= i+1;
                    }
                    let bl: Block = func.0.body.clone();
                    let retval = bl.eval(env);
                    env.v.pop_scope();
                    retval
                }
            },
            Expr::UnOp(uop, e) => uop.eval(*e.clone(), env),
        }
    }
}

impl Eval<Val> for Block {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        // let mut env = env.clone();
        env.v.push_scope();
        let mut return_val = Val::Lit(Literal::Unit);
        for be in &self.statements {
            println!("be {:?}", be);
            match be {
                Statement::Let(m, id, _, e) => {
                    // the right hand side, in the "old" env
                    let l: Val;
                    match (e){
                        Some(e) => l = e.eval(env)?.0,
                        None => l = Val::UnInit
                    }
                    // the left hand side, for now just accept an ident
                    if m.0{
                        env.v.alloc(id, Val::Mut(Box::new(l)));
                    }else{
                        env.v.alloc(id, l);
                    }
                },
                Statement::Assign(id, e) => {
                    
                    let id_val = id.eval(env)?;
                    let ex = e.eval(env)?;
                    
                    let m = id_val.1;
                    
                    if m.is_some(){
                        match env.v.de_ref(m.unwrap()) {
                            Val::Mut(_) => env.v.set_ref(id_val.1.unwrap(), Val::Mut(Box::new(ex.0))),
                            Val::Ref(re) => env.v.set_ref(re, ex.0),
                            Val::UnInit => env.v.set_ref(id_val.1.unwrap(), ex.0),
                            Val::Lit(_) => return Err("That variable is immutable and cannot be assigned a value".to_string()),
                        }
                    }
                },
                Statement::Expr(e) => {
                    return_val = e.eval(env)?.0;
                },

                Statement::While(c, block) => {
                    while c.eval(env)?.0.get_bool()? {
                        block.eval(env)?;
                    }
                },
                Statement::Fn(fndecl) => {
                    fndecl.eval(env)?;
                },
            }
        }
        env.v.pop_scope();
        match self.semi {
            true => Ok((Val::Lit(Literal::Unit), None)),
            false => Ok((return_val, None)),
        }
    }
}

impl Eval<Val> for FnDeclaration {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        let res = env.f.add_functions_unique(vec![self.clone()]);
        Ok((Val::Lit(Literal::Unit), None))
    }
}

impl  UnOp {
    fn eval(&self, expr: Expr, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        use Literal::Bool;
        match self {
            UnOp::Bang => Ok((Val::Lit(Bool(!expr.eval(env)?.0.get_bool()?)), None) ),
            UnOp::Ref => {
                let v = expr.eval(env)?;

                let r = if v.1.is_none(){
                    let new_ref = env.v.stack_val(v.0.clone());
                    env.v.set_ref(new_ref, v.0);
                    new_ref
                } else {
                    v.1.unwrap()
                };

                Ok( (Val::Ref(r.clone()), Some(r)))
            },
            UnOp::DeRef => {
                let v = expr.eval(env)?;
                let v = v.0;
                match v {
                    Val::Ref(r) => Ok((env.v.de_ref(r.clone()), Some(r))),
                    Val::Mut(m) => match *m {
                        Val::Ref(r) => Ok((env.v.de_ref(r.clone()), Some(r))),
                        _ => Err("Var is not a reference!".to_string())
                    }
                    _ => Err("Var is not a reference!".to_string())
                }
            },
            UnOp::Mut => Ok((Val::Mut(Box::new(expr.eval(env)?.0)), None)),
        }
    }
}

impl Eval<Val> for Prog {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        //env.f.add_functions_unique(self.0.clone());
        let mut mainfunc: Option<FnDeclaration> = None;
        let (print, intrinsic) = vm_println();
        env.f.0.insert(print.id.clone(), (print, Some(intrinsic)));
        for func in self.0.clone(){
            if func.id == "main"{
                if func.parameters.0.is_empty(){
                    mainfunc = Some(func.clone());
                }else{
                    return Err("The main function should not have arguments".to_string())
                }
            }
            func.eval(env)?;
        }
        if mainfunc.is_none(){
            Err("Warning, function 'main' not found".to_string())
        }else{
            mainfunc.unwrap().body.eval(env)
        }

    }
}

#[cfg(test)]
mod tests {
    use super::Val;
    use crate::ast::Literal;
    use crate::ast::{Block, Prog};
    use crate::common::parse_test;

    #[test]
    fn test_block_let() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a: i32 = 1;
        let b: i32 = 2;

        a + b
    }",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 3);
    }

    #[test]
    fn test_block_let_shadow() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a: i32 = 1;
        let b: i32 = 2;
        let a: i32 = 3;
        let b: i32 = 4;

        a + b
    }",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 7);
    }

    #[test]
    fn test_block_assign() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a: i32 = 1;
        a = a + 2;
        a
    }",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 3);
    }

    #[test]
    fn test_block_assign_fail() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a: i32 = 1;
        a = a + 2;
        a
    }",
        );
        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_expr_if_then_else() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a: i32 = 1;
        a = if a > 0 { a + 1 } else { a - 2 };
        a
    }",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_expr_if_then_else2() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a: i32 = 1;
        a = if a < 0 { a + 1 } else { a - 2 };
        a
    }",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), -1);
    }

    #[test]
    fn test_ref_deref() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 1;
        let b = &a;
        *b
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }

    #[test]
    fn test_ref_deref_indirect() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 1;
        let b = &a;
        let c = b;
        *c
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }

    #[test]
    fn test_deref_assign() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a = 1;
        let b = &a;
        *b = 7;
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 7);
    }

    #[test]
    fn test_while() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a = 2;
        let mut b = 0;
        while a > 0 {
            a = a - 1;
            b = b + 1;
        }
        b
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_while_ref() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a = 2;
        let b = 0;
        let c = &mut b;
        while a > 0 {
            a = a - 1;
            *c = (*c) + 1;
        }
        *c
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_while_ref2() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 2;
        let b = 0;
        let c = &mut b;
        let d = &mut a;
        
        while (*d) > 0 {
            *d = (*d) - 1;
            *c = (*c) + 1;
        }
        *c
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_bool() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = true && false;
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_bool().unwrap(), false);
    }

    #[test]
    fn test_bool_bang() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = true && !false;
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_bool().unwrap(), true);
    }

    #[test]
    fn test_bool_bang2() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = (!true) && false;
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_bool().unwrap(), false);
    }

    #[test]
    fn test_local_block() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a = 1;
        { 
            let b = &a;
            *b = 2;
        };
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_local_block_assign() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a = 6;
        let b = { 
            let b = &a;
            *b = (*b) + 1;
            *b
        };
        b
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 7);
    }

    #[test]
    fn test_prog() {
        let v = parse_test::<Prog, Val>(
            "
            fn main() {
                let a = 1;
                a
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }

    #[test]
    fn test_local_fn() {
        let v = parse_test::<Prog, Val>(
            "
            fn main() {
                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }
                let a = f(1, 2);
                println!(\"a = {} and another a = {}\", a, a);
            }
            ",
        );

        assert_eq!(v.unwrap(), Val::Lit(Literal::Unit));
    }

    #[test]
    fn test_check_if_then_else_shadowing() {
        let v = parse_test::<Block, Val>(
            "
        {
            let a: i32 = 1 + 2; // a == 3
            let mut a: i32 = 2 + a; // a == 5
            if true {
                a = a - 1;      // outer a == 4
                let mut a: i32 = 0; // inner a == 0
                a = a + 1       // inner a == 1
            } else {
                a = a - 1
            };
            a   // a == 4
        }
        ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 4);
    }
    #[test]
    fn test_ref() {
        let v = parse_test::<Block, Val>(
            "
        {
            let a = &1;
            *a
        }
        ",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }
    #[test]
    fn test_ref_2() {
        let v = parse_test::<Block, Val>(
            "
        {
            let a = &1;
            let b = &a;
            **b
        }
        ",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }
    #[test]
    fn test_mut() {
        let v = parse_test::<Block, Val>(
            "
        {
            let a = 1;
            a = a + 3;
            a
        }
        ",
        );
        assert_eq!(v.is_err(), true);
    }
    #[test]
    fn test_mut_ref() {
        let v = parse_test::<Block, Val>(
            "
        {
            let a = 1;
            let b = &a;
            *b = 5;
            a
        }
        ",
        );
        assert_eq!(v.is_err(), true);
    }
    #[test]
    fn test_ref_3_scope() {
        let v = parse_test::<Block, Val>(
            "
        {
            let b;
            {
                let a = 1;
                b = &a;
                let c = *b + 2;
            }
            *b
        }
        ",
        );
        assert_eq!(v.is_err(), true);
    }
    #[test]
    fn test_ref_func() {
        let v = parse_test::<Block, Val>(
            "
        {
            let b = &3;
            fn test(a: &i32) {
                *a = 6;
            }
            test(b);
        }
        ",
        );
        assert_eq!(v.is_err(), true);
    }
}
