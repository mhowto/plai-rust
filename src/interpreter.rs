use ty::Expression;
use std::collections::HashMap;

type Location = u64;

#[derive(Debug,PartialEq)]
pub struct Binding {
    name: String,
    loc: Location,
}

#[derive(Debug,PartialEq)]
pub enum Env {
    mt_env,

    // 为什么用Box，不用&Env
    // 因为Env对rest有所有权，用&Env只是借用，并没有所有权
    extend_env{with: Binding, rest: Box<Env>} 
}

#[derive(Debug,PartialEq)]
pub enum Value {
    NilV,
    NumV(isize),
    BoolV(bool),
    // ClosV{arg: String, body: Expression, env: Env},
    BoxV(Location),
}

impl Value {
    pub fn to_string(&self) -> String {
        match *self {
            Value::NilV => String::from("Nil"),
            Value::NumV(n) => n.to_string(),
            Value::BoolV(b) => b.to_string(),
            // Value::ClosV{ref arg, ref body, ref env} => String::from("Unknown"),
            Value::BoxV(_) => String::from("Unknown")
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match *self {
            Value::NilV => Value::NilV,
            Value::NumV(n) => Value::NumV(n),
            Value::BoolV(b) => Value::BoolV(b),
            Value::BoxV(a) => Value::BoxV(a),
            // Value::ClosV{ref arg, ref body, ref env} => Value::ClosV
        }

    }
}

pub struct Storage {
    location: Location,
    val: Value,
}

//type Store = HashMap<Location, &mut Value>;

pub enum Store {
    mt_store,
    override_store{cell: Storage, rest: Box<Store>}
}

pub struct IResult {
    pub val: Value,
    pub sto: Store,
}

fn num_op<F: Fn(isize, isize) -> isize>(op: F, left: &Value, right: &Value) -> Value {
    match (left, right) {
        (&Value::NumV(l), &Value::NumV(r)) => Value::NumV(op(l, r)),
        _ => panic!("type error")
    }
}

/*
fn num_plus(left: &Value, right: &Value) -> Option<Value> {
    num_op(|x, y| x+y, left, right)
}

fn num_minus(left: &Value, right: &Value) -> Option<Value> {
    num_op(|x, y| x-y, left, right)
}

fn num_mult(left: &Value, right: &Value) -> Option<Value> {
    num_op(|x, y| x*y, left, right)
}
*/

fn lookup(what: &String, in_env: &Env) -> Location {
    match in_env {
        &Env::mt_env => panic!("lookup: unbound identifier"),
        &Env::extend_env{ref with, ref rest} => 
            if what.eq(&with.name) {
                with.loc
            } else {
                lookup(what, rest.as_ref())
            }
    }
}

fn fetch(what: Location, in_store: &Store) -> Value {
    match in_store {
        &Store::mt_store => panic!("fetch: location not found"),
        &Store::override_store{ref cell, ref rest} =>
            if cell.location == what {
                cell.val.clone()
            } else {
                fetch(what, rest.as_ref())
            }
    }
}

fn interp(expr: &Expression, env: &Env, sto: &mut Store) -> Value {
    match expr {
        &Expression::Nil     => Value::NilV,
        &Expression::True    => Value::BoolV(true),
        &Expression::False   => Value::BoolV(false),
        &Expression::Num(n)  => Value::NumV(n),
        &Expression::Uminus(ref v) => {
            let r = interp(v.as_ref(), env, sto);
            match r {
                Value::NumV(val) => Value::NumV(-val),
                _ => panic!("type error: num expected")
            }
        },
        &Expression::Plus(ref left, ref right) => {
            let r1 = interp(left.as_ref(), env, sto);
            let r2 = interp(right.as_ref(), env, sto);
            num_op(|x, y| x+y, &r1, &r2)
        },
        &Expression::Bminus(ref left, ref right) => {
            let r1 = interp(left.as_ref(), env, sto);
            let r2 = interp(right.as_ref(), env, sto);
            num_op(|x, y| x-y, &r1, &r2)
        },
        &Expression::Mult(ref left, ref right) => {
            let r1 = interp(left.as_ref(), env, sto);
            let r2 = interp(right.as_ref(), env, sto);
            num_op(|x, y| x*y, &r1, &r2)
        },
        &Expression::True => {
            Value::BoolV(true)
        },
        &Expression::False => {
            Value::BoolV(false)
        },
        &Expression::If{ref test, ref expr_if, ref else_expr} => {
            let test_value = interp(test.as_ref(), env, sto);
            if test_value == Value::BoolV(true) {
                interp(expr_if.as_ref(), env, sto)
            } else if let &Option::Some(ref else_expr1) = else_expr {
                interp(else_expr1, env, sto)
            } else {
                panic!("type error: expected else expression");
            }
        },
        &Expression::ID(ref id) => fetch(lookup(id, env), sto),
        _ => Value::NilV,
    }
}

pub fn interpret(expr: &Expression) -> Value {
    interp(expr, &mut Env::mt_env, &mut Store::mt_store)
}