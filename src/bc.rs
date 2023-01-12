use crate::ast::{Block, Expr, FnDeclaration, Prog, Statement, Literal};
use crate::common::Eval;
use crate::env::{Env, Ref};
use crate::error::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Loan {
    Unique(Ref),
    Shared(Ref),
}

type Stack = Vec<Loan>;
#[derive(Debug, Clone, PartialEq)]
pub struct Loans(Stack);

impl Loans {
    //
}

// Borrow check
#[derive(Debug, Clone, PartialEq)]
pub enum Bc {
    Lit(Loans),
    Ref(bool, Ref),
}

impl Eval<Bc> for Expr {
    fn eval(&self, env: &mut Env<Bc>) -> Result<(Bc, Option<Ref>), Error> {
        match self {
            Expr::Ident(id) => match env.v.get(id){
                Some(t) => Ok((t, env.v.get_ref(id))),
                None => Err("Variable not found".to_string()),
            },
            Expr::Lit(l) => todo!(),
            Expr::BinOp(op, e, e2) => todo!(),
            Expr::Par(e) => todo!(),
            Expr::Block(bl) => todo!(),
            Expr::IfThenElse(e, then_bl, else_bl) => todo!(),
            Expr::UnOp(uop, e) => todo!(),
            Expr::Call(id, args) => todo!(),
        }
        //todo!("not implemented {:?}", self)
    }
}

impl Eval<Bc> for Block {
    fn eval(&self, env: &mut Env<Bc>) -> Result<(Bc, Option<Ref>), Error> {
        env.v.push_scope();
        let mut return_val: Bc;
        for be in &self.statements {
            match be {
                Statement::Let(m, id, ty, ex) => todo!(),
                Statement::Assign(left, right) => todo!(),
                Statement::Expr(e) => todo!(),
                Statement::While(e, do_block) => todo!(),
                Statement::Fn(decl) => todo!(),
            }
        }
        env.v.pop_scope();
        todo!("not implemented {:?}", self)
    }
}

impl Eval<Bc> for FnDeclaration {
    fn eval(&self, env: &mut Env<Bc>) -> Result<(Bc, Option<Ref>), Error> {
        todo!("not implemented {:?}", self)
    }
}

impl Eval<Bc> for Prog {
    fn eval(&self, env: &mut Env<Bc>) -> Result<(Bc, Option<Ref>), Error> {
        todo!("not implemented {:?}", self)
    }
}

#[cfg(test)]
mod tests {
    use super::Bc;
    use crate::ast::{Block, Prog, Literal};
    use crate::common::parse_test;

    // Tests for the Bc specific handling
    // of Loans etc.
    #[test]
    fn loan() {}

    // Tests for borrow checking of Block
    #[test]
    fn test_block_let() {
        let _v = parse_test::<Block, Bc>(
            "
        {
            let a: i32 = 1;
            let b: i32 = 2;

            a + b
        }",
        );
        assert_eq!(_v.is_ok(), true)
    }

    // Come up with your own set of tests for Block
    #[test]
    fn test_block_let_2() {
        let _v = parse_test::<Block, Bc>(
            "
        {
            let a: i32 = 1;
            let b = a;

            a
        }",
        );
        assert_eq!(_v.is_err(), true);
        // Suitable assertion
    }

    // Tests for borrow checking of Prog
    #[test]
    fn test_prog_fn_sig() {
        let _v = parse_test::<Prog, Bc>(
            "
        fn main() {
            let mut a = 0;
            let b = &mut a;
            let c = &a;
            *b = 4;
            let d = *c; // <- error here, with stacked borrows
        }
            ",
        );
        // suitable assertion
    }

    // Come up with your own set of test for Prog
}
