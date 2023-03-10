use crate::ast::{Block, Expr, FnDeclaration, Literal, Op, Prog, Type, Statement, UnOp};
use crate::common::Eval;
use crate::env::{Env, Ref};
use crate::error::Error;

use std::convert::{From, Into};
use std::fmt::Debug;

// type check
#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Lit(Type),
    Ref(Ref),
    Mut(Box<Ty>),
}

// Helpers for Ty
impl From<&Literal> for Ty {
    fn from(t: &Literal) -> Self {
        Ty::Lit(match *t {
            Literal::Bool(_) => Type::Bool,
            Literal::Int(_) => Type::I32,
            Literal::String(_) => Type::String,
            Literal::Unit => Type::Unit,
        })
    }
}

#[allow(dead_code)]
fn op_type(op: Op) -> (Ty, Ty, Ty) {
    match op {
        Op::Add => (Ty::Lit(Type::I32), Ty::Lit(Type::I32), Ty::Lit(Type::I32)),
        Op::Sub => (Ty::Lit(Type::I32), Ty::Lit(Type::I32), Ty::Lit(Type::I32)),
        Op::Mul => (Ty::Lit(Type::I32), Ty::Lit(Type::I32), Ty::Lit(Type::I32)),
        Op::Div => (Ty::Lit(Type::I32), Ty::Lit(Type::I32), Ty::Lit(Type::I32)),
        Op::And => (Ty::Lit(Type::Bool), Ty::Lit(Type::Bool), Ty::Lit(Type::Bool)),
        Op::Or => (Ty::Lit(Type::Bool), Ty::Lit(Type::Bool), Ty::Lit(Type::Bool)),
        Op::Lt => (Ty::Lit(Type::I32), Ty::Lit(Type::I32), Ty::Lit(Type::Bool)),
        Op::Gt => (Ty::Lit(Type::I32), Ty::Lit(Type::I32), Ty::Lit(Type::Bool)),
        Op::Eq => (Ty::Lit(Type::I32), Ty::Lit(Type::I32), Ty::Lit(Type::Bool)),
        _ => todo!(),
    }
}

// Helper for Op
impl Op {
    // Evaluate operator to literal
    fn unify(&self, left: Ty, right: Ty) -> Result<(Ty, Option<Ref>), Error> {
        match self {
            Op::Add => unify(left, right, Ty::Lit(Type::I32)),
            Op::Sub => unify(left, right, Ty::Lit(Type::I32)),
            Op::Mul => unify(left, right, Ty::Lit(Type::I32)),
            Op::Div => unify(left, right, Ty::Lit(Type::I32)),
            Op::And => unify(left, right, Ty::Lit(Type::Bool)),
            Op::Or => unify(left, right, Ty::Lit(Type::Bool)),
            Op::Eq => unify(left, right, Ty::Lit(Type::Bool)),
            Op::Lt => unify(left, right, Ty::Lit(Type::Bool)),
            Op::Gt => unify(left, right, Ty::Lit(Type::Bool)),

            _ => todo!(),
        }
        /* let (lefttyp, righttyp, restyp) = op_type(*self);
        match(left == lefttyp, right == righttyp){
            (true, true) => Ok((restyp, None)),
            (false, true) => Err(format!("Wrong left argument in Op: {}", self)),
            (true, false) => Err(format!("Wrong right argument in Op: {}", self)),
            _ => Err(format!("Wrong arguments in Op: {}", self)),
        } */
    }
}

// General unification
fn unify(expected: Ty, got: Ty, result: Ty) -> Result<(Ty, Option<Ref>), Error> {
    match expected == got {
        true => Ok((result.into(), None)),
        _ => Err(format!(
            "Cannot unify types, expected {:?} got {:?}",
            expected, got
        )),
    }
}

fn get_type_from_mut(input: Ty) -> Ty {
    match input {
        Ty::Mut(b) => *b,
        _ => input
    }
}


impl Eval<Ty> for Expr {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        match self {
            Expr::Ident(id) => match env.v.get(&id) {
                Some(t) => {
                    Ok((get_type_from_mut(t), env.v.get_ref(id)))
                },
                None => Err("variable not found".to_string()),
            },
            Expr::Lit(Literal::Int(_)) => Ok((Ty::Lit(Type::I32), None)),
            Expr::Lit(Literal::Bool(_)) => Ok((Ty::Lit(Type::Bool), None)),
            Expr::Lit(Literal::Unit) => Ok((Ty::Lit(Type::Unit), None)),
            Expr::Lit(Literal::String(_)) => Ok((Ty::Lit(Type::String), None)),
    
            #[allow(unused_variables)]
            Expr::BinOp(op, l, r) => {
                let l_type = l.eval(env)?;
                let r_type = r.eval(env)?;
                let optyp = op.unify(l_type.0, r_type.0)?;
                Ok(optyp)
            }
    
            #[allow(unused_variables)]
            Expr::Par(expr) => expr.eval(env),
    
            #[allow(unused_variables)]
            Expr::IfThenElse(cond, t, e) => {
                let cond_type = cond.eval(env)?;
                let do_type = t.eval(env)?;
                if unify(cond_type.0, Ty::Lit(Type::Bool), Ty::Lit(Type::Bool)).is_err() {
                    return Err("Condition needs to be a boolean".to_string())
                }
                if e.is_none(){
                    Ok(do_type)
                } else {
                    let else_type = e.as_ref().unwrap().eval(env)?;
                    unify(do_type.0.clone(), else_type.0.clone(), else_type.0.clone())?;
                    Ok(do_type)
                }
            },
            Expr::Call(id, args) => 
            if env.f.0.contains_key(id){
                let f = env.f.0.get(id).unwrap().clone();

                let mut i = 0;
                for param in f.0.parameters.0.clone() {
                    let a = args.0.get(i).clone();
                    let arg;
                    if a != None{
                        arg = a.unwrap();
                    }else{
                        break;
                    }
                    let arg = arg.eval(env)?.0;
                    if Ty::Lit(param.ty.clone()) != arg.clone() {
                        //Throw error since arg and param types don't match
                        return unify(Ty::Lit(param.ty), arg.clone(), arg)
                    }
                    i = i+1;
                }

                if f.0.ty.is_some(){
                    Ok((Ty::Lit(f.0.ty.unwrap()), None))
                }else{
                    Ok((Ty::Lit(Type::Unit), None))
                }
                
            }else{
                return Err("This function is not declared".to_string())
            },
            Expr::Block(b) => b.eval(env),
            Expr::UnOp(uop, e) => {
                let expr = (*e.clone()).eval(env)?;
                match uop {
                    UnOp::Ref => match expr.0.clone(){
                        Ty::Lit(type_) => {
                            let r = if expr.1.is_none(){
                                let new_ref = env.v.stack_val(expr.0.clone());
                                env.v.set_ref(new_ref, expr.0);
                                new_ref
                            }else{
                                expr.1.unwrap()
                            };
                            Ok((Ty::Lit(Type::Ref(Box::new(type_))), Some(r)))
                        },
                        _ => Err(format!("Expected Literal but got {:?}", expr.0))
                    },
                    UnOp::DeRef => match expr.0 {
                        Ty::Lit(Type::Ref(e)) => Ok((Ty::Lit(*e), None)),
                        Ty::Ref(r) => Ok((env.v.de_ref(r.clone()), Some(r))),
                        _ => Err(format!("Expected Literal but got {:?}", expr.0))
                    },
                    UnOp::Bang => unify(expr.0.clone(), Ty::Lit(Type::Bool), Ty::Lit(Type::Bool)),
                    UnOp::Mut => Ok((Ty::Mut(Box::new(expr.0)), None)),
                }
            },
        }
    }
}

impl Eval<Ty> for Statement{
    #[allow(unused_variables)]
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        #[allow(unreachable_code)]
        Ok(match self {
            #[allow(unused_variables)]
            Statement::Let(m, id, t, e) => {
                // let a: i32 = 5 + 2
                // for now just accept an ident
                let e_val: Ty;
                if e.is_some(){
                    e_val = e.as_ref().unwrap().eval(env)?.0;
                }else{
                    e_val = Ty::Lit(Type::Unit);
                }
                match (e, t) {
                    (Some(e), Some(t)) => {
                        if unify(e_val.clone(),Ty::Lit((*t).clone()), Ty::Lit((*t).clone())).is_err() {
                            return Err("Missmatching types in let-statement".to_string())
                        }
                        match e_val {
                            Ty::Mut(_) => {env.v.alloc(&id, e_val.clone());},
                            _ => {
                                if m.0 {
                                    env.v.alloc(&id, Ty::Mut(Box::new(e_val.clone())));
                                }else{
                                    env.v.alloc(&id, e_val.clone());
                                }
                            },
                        }
                        
                    },
                    (None, Some(t)) => {
                        if m.0 {
                            env.v.alloc(&id, Ty::Mut(Box::new(Ty::Lit((t).clone()))));
                        }else{
                            env.v.alloc(&id, Ty::Lit((*t).clone()));
                        }
                        
                    },
                    (Some(e), None) => {
                        match e_val {
                            Ty::Mut(_) => {env.v.alloc(&id, e_val.clone());},
                            _ => {
                                if m.0 {
                                    env.v.alloc(&id, Ty::Mut(Box::new(e_val.clone())));
                                }else{
                                    env.v.alloc(&id, e_val.clone());
                                }
                            },
                        }
                        
                    },
                    (None, None) => {
                        env.v.alloc(&id, Ty::Lit(Type::Unit));
                    },
                }
                (Ty::Lit(Type::Unit), None)
            }
            #[allow(unused_variables)]
            Statement::Expr(e) => {
                // the type of an Expr is returned
                e.eval(env)?
            }
            #[allow(unused_variables)]
            Statement::Assign(id, e) => {
                let id_typ = id.eval(env)?.0;
                let m = id.eval(env)?.1;
                let ty: Option<Ty> = env.v.get(&id.to_string());
                if ty.is_some(){
                    println!("{} of type: idonno", &id.to_string());
                }
                
                if m.is_some() {
                    match env.v.de_ref(m.unwrap()) {
                        Ty::Mut(_) => {},
                        //Not allowed in rust

                        Ty::Lit(Type::Ref(_)) => return Err("Can't assign to Reference".to_string()),
                        _ => return Err("Can't assign to none mutable".to_string())
                    }
                }
                
                let e_type = e.eval(env)?;
                match id_typ {
                    Ty::Lit(Type::Unit) => { // if it is a unit-type we can change it's type in our environment
                        match id {
                            Expr::Ident(key) => {env.v.alloc(&key, e_type.0);},
                            _ => unreachable!()
                        }
                    },
                    _ => {
                        let err = unify(id_typ, e_type.clone().0, e_type.0);
                        if err.is_err(){
                            return err
                        }
                    },
                }
                (Ty::Lit(Type::Unit), None)
            }
            #[allow(unused_variables)]
            Statement::While(e, b) => {
                let cond_type = e.eval(env)?;
                let do_type = b.eval(env);
                if unify(cond_type.0, Ty::Lit(Type::Bool), Ty::Lit(Type::Bool)).is_ok(){
                    do_type?
                } else {
                    return Err("Your condition needs to be a boolean".to_string())
                }
            },
            Statement::Fn(decl) => {
                decl.eval(env)?
            },
        })
    }
}

impl Eval<Ty> for Block {
    
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        env.v.push_scope();
        #[allow(unused_variables)]
        let mut return_ty = (Ty::Lit(Type::Unit), None);
        for stmt in &self.statements {
            // update the return type for each iteration
            return_ty = stmt.eval(env)?;
        }
        env.v.pop_scope();
        if self.semi{
            Ok((Ty::Lit(Type::Unit), None))
        }else{
            Ok(return_ty)
        }
    }
}

impl Eval<Ty> for FnDeclaration {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        let res = env.f.add_functions_unique(vec![self.clone()]);
        if self.ty.is_none(){
            Ok((Ty::Lit(Type::Unit), None))
        } else {
            Ok((Ty::Lit(self.ty.clone().unwrap()), None))
        }
    }
}

impl Eval<Ty> for Prog {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        for func in self.0.clone(){
            func.eval(env)?;
        }
        match env.f.0.get("main") {
            Some(_f) => return Ok((Ty::Lit(Type::Unit), None)),
            None => Err("Warning, function 'main' not found")?,
        }

    }
}

#[cfg(test)]
mod tests {
    use super::Ty;
    use crate::ast::{Block, Prog, Type};
    use crate::common::parse_test;

    #[test]
    fn test_block_let() {
        let v = parse_test::<Block, Ty>(
            "
    {
        let a: i32 = 1;
        let b: i32 = 2;

        a + b
    }",
        );
        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_block_let_shadow() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a: i32 = 1;
            let b: i32 = 2;
            let a: i32 = 3;
            let b: i32 = 4;

            a + b
        }",
        );
        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_block_assign() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let mut a: i32 = 1;
            a = 1 + 2;
            a
        }",
        );
        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_expr_if_then_else() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let mut a: i32 = 1;
            a = if a > 0 { a + 1 } else { a - 2 };
            a
        }",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_expr_if_then_else_bool() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let mut a: bool = false;
            a = if a || false { a || false } else { a && true };
            a
        }",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::Bool));
    }

    #[test]
    fn test_ref_deref() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a: i32 = 1;
            let b: &i32 = &a;
            *b
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_ref_deref_err() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a: i32 = 1;
            let b: &bool = &a;
            *b
        }
        ",
        );

        assert_eq!(v.is_err(), true);
    }

    
    #[test]
    fn test_non_mut_err() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a: i32 = 1;
            a = 2;
        }
        ",
        );

        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_ref_deref_indirect() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            let b = &a;
            let c = b;
            *c
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_ref_deref_indirect2() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            let b = &a;
            let c = &b;
            **c
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_deref_assign_err() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            let b = &a;
            *b = false;
            a
        }
        ",
        );

        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_deref_assign() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            let b = &a;
            *b = 7;
            a
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_while_err() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let mut a = 2;
            let mut b = false;
            while a > 0 {
                a = a - 1;
                b = b + 1;
            }
            b
        }
        ",
        );

        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_while() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let mut a = 2;
            let mut b = 1;
            while a > 0 {
                a = a - 1;
                b = b + 1;
            }
            b
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }
    #[test]
    fn test_while_ref() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let mut a = 2;
            let b = 0;
            let c = &b;
            while a > 0 {
                a = a - 1;
                *c = (*c) + 1;
            }
            *c
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_while_ref2() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 2;
            let b = 0;
            let c = &b;
            let d = &a;

            while (*d) > 0 {
                *d = (*d) - 1;
                // not sure if this is even allowed in Rust
                *&*c = (*c) + 1;
            }
            *c
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_bool() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = true && false;
            a
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::Bool));
    }

    #[test]
    fn test_bool_bang() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = true && !false;
            a
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::Bool));
    }

    #[test]
    fn test_bool_bang2() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = (!true) && false;
            a
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::Bool));
    }

    #[test]
    fn test_local_block() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            {
                let b = &a;
                *b = 2;
            };
            a
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_local_block_assign() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 6;
            let b = {
                let b : &i32 = &a;
                *b = (*b) + 1;
                *b
            };
            b
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_prog_fn_sig() {
        let v = parse_test::<Prog, Ty>(
            "
        fn a(i: i32, bo: bool) -> i32 {
            let q = 0;
            fn b(j: i32) -> i32 {
                a(j, c())
            }

            fn c() -> bool {
                false
            }

            b(1 + i);
            a(i, bo)
        }
        ",
        );
        println!("v {:?}", v);
        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_prog_fn_defined_twice() {
        let v = parse_test::<Prog, Ty>(
            "
        fn a() {
        }

        fn b() {
            fn b() {

            }

        }
        ",
        );
        println!("v {:?}", v);
        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_prog() {
        let v = parse_test::<Prog, Ty>(
            "
        fn main() {
            let a = 1;
            a;
        }
        ",
        );
        println!("v {:?}", v);
        assert_eq!(v.is_ok(), true);
    }

    #[test]
    fn test_local_fn() {
        let v = parse_test::<Prog, Ty>(
            "
        fn main() {
            fn f(i: i32, j: i32) -> i32 {
                i + j
            }
            let a = f(1, 2);
            // println!(\"a = {} and another a = {}\", a, a);
        }
        ",
        );
        println!("v {:?}", v);
        assert_eq!(v.is_ok(), true);
    }

    #[test]
    fn test_check_if_then_else_shadowing() {
        let v = parse_test::<Block, Ty>(
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

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_ref() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = &1;
            *a
        }
        ",
        );
        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_mut() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            a = 4;
            a
        }
        ",
        );
        assert_eq!(v.is_err(), true);
    }
    #[test]
    fn test_prog_unit() {
    let v = parse_test::<Prog, Ty>(
        "
          fn main(){
            fn f(i: i32, j: i32) -> i32 {
                i + j
                
            }
            let mut a = f(1, 2);
            a = true;
            a = 2
            // println!(\"a = {} and another a = {}\", a, a);
            }
        ",
    );
    assert_eq!(v.unwrap(), Ty::Lit(Type::Unit));
}

}
