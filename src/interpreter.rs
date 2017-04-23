use ty::Expression;

type Location = u64;

#[derive(Debug,PartialEq)]
pub struct Binding {
    name: String,
    val: Location,
}

#[derive(Debug,PartialEq)]
pub enum Env {
    mt_env,
    extend_env{with: Binding, rest: Box<Env>}
}

#[derive(Debug,PartialEq)]
pub enum Value {
    NilV,
    NumV(isize),
    BoolV(bool),
    ClosV{arg: String, body: Expression, env: Env},
    BoxV(Location),
}

impl Value {
    pub fn to_string(&self) -> String {
        match *self {
            Value::NilV => String::from("Nil"),
            Value::NumV(n) => n.to_string(),
            Value::BoolV(b) => b.to_string(),
            Value::ClosV{ ref arg, ref body, ref env } => String::from("Unknown"),
            Value::BoxV(_) => String::from("Unknown")
        }
    }
}

pub struct Storage {
    location: Location,
    val: Value,
}

pub enum Store {
    mt_store,
    override_store{with: Storage, rest: Box<Store>}
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

fn interp(expr: &Expression, env: &mut Env, sto: &mut Store) -> Value {
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
        _ => Value::NilV,
    }
}

pub fn interpret(expr: &Expression) -> Value {
    interp(expr, &mut Env::mt_env, &mut Store::mt_store)
}