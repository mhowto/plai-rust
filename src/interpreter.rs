use ty::Expression;
use std::collections::HashMap;

type Location = u64;

fn counter() -> Box<FnMut() -> u64> {
    let mut c = 0;
    Box::new(move || {
        c += 1;
        c
    })
}

// static mut new_loc: Box<FnMut() -> isize> = counter();


#[derive(Debug,PartialEq,Clone)]
pub struct Binding {
    name: String,
    loc: Location,
}

#[derive(Debug,PartialEq,Clone)]
pub enum Env {
    MtEnv,

    // 为什么用Box，不用&Env
    // 因为Env对rest有所有权，用&Env只是借用，并没有所有权
    ExtendEnv{with: Binding, rest: Box<Env>} 
}

#[derive(Debug,PartialEq,Clone)]
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
            Value::ClosV{arg: _, body: _, env: _} => String::from("ClosV"),
            Value::BoxV(loc) => format!("Box{{ Location: {} }}", loc)
        }
    }
}

/*
pub struct Storage {
    location: Location,
    val: Value,
}
*/

pub type Store = HashMap<Location, Value>;

/*
pub enum Store {
    mt_store,
    override_store{cell: Storage, rest: Box<Store>}
}
*/

/*
pub struct IResult {
    pub val: Value,
    pub sto: Store,
}
*/

fn num_op<F: Fn(isize, isize) -> isize>(op: F, left: &Value, right: &Value) -> Value {
    match (left, right) {
        (&Value::NumV(l), &Value::NumV(r)) => Value::NumV(op(l, r)),
        _ => panic!("type error")
    }
}

fn lookup(what: &String, in_env: &Env) -> Location {
    match in_env {
        &Env::MtEnv => panic!("lookup: unbound identifier"),
        &Env::ExtendEnv{ref with, ref rest} => 
            if what.eq(&with.name) {
                with.loc
            } else {
                lookup(what, rest.as_ref())
            }
    }
}

fn fetch(what: Location, sto: &Store) -> Value {
    match sto.get(&what) {
        Some(val) => val.clone(),
        None => panic!("fetch not found")
    }
}

fn interp(new_loc: &mut Box<FnMut() -> u64>, expr: &Expression, env: &Env, sto: &mut Store) -> Value {
    match expr {
        &Expression::Nil     => Value::NilV,
        &Expression::True    => Value::BoolV(true),
        &Expression::False   => Value::BoolV(false),
        &Expression::Num(n)  => Value::NumV(n),
        &Expression::Uminus(ref v) => {
            let r = interp(new_loc, v.as_ref(), env, sto);
            match r {
                Value::NumV(val) => Value::NumV(-val),
                _ => panic!("type error: num expected")
            }
        },
        &Expression::Plus(ref left, ref right) => {
            let r1 = interp(new_loc, left.as_ref(), env, sto);
            let r2 = interp(new_loc, right.as_ref(), env, sto);
            num_op(|x, y| x+y, &r1, &r2)
        },
        &Expression::Bminus(ref left, ref right) => {
            let r1 = interp(new_loc, left.as_ref(), env, sto);
            let r2 = interp(new_loc, right.as_ref(), env, sto);
            num_op(|x, y| x-y, &r1, &r2)
        },
        &Expression::Mult(ref left, ref right) => {
            let r1 = interp(new_loc, left.as_ref(), env, sto);
            let r2 = interp(new_loc, right.as_ref(), env, sto);
            num_op(|x, y| x*y, &r1, &r2)
        },
        &Expression::If{ref test, ref expr_if, ref else_expr} => {
            let test_value = interp(new_loc, test.as_ref(), env, sto);
            if test_value == Value::BoolV(true) {
                interp(new_loc, expr_if.as_ref(), env, sto)
            } else if let &Option::Some(ref else_expr1) = else_expr {
                interp(new_loc, else_expr1, env, sto)
            } else {
                panic!("type error: expected else expression");
            }
        },
        &Expression::ID(ref id) => fetch(lookup(id, env), sto),
        &Expression::Lambda{ref arg, ref body} => Value::ClosV{
            arg: arg.clone(),
            body: body.as_ref().clone(),
            env: env.clone(),
            },
        &Expression::App{ref func, ref arg} =>  {
            if let Value::ClosV{arg: ref clos_arg, body: ref clos_body, env: ref clos_env} = interp(new_loc, func,  env, sto) {
                let arg_val =interp(new_loc, arg, env, sto);
                let nloc = new_loc();
                sto.insert(nloc, arg_val);
                interp(
                    new_loc,
                    clos_body,
                    &Env::ExtendEnv{
                        with: Binding{name: clos_arg.clone(), loc: nloc},
                        rest: Box::new(clos_env.clone())
                        },
                    sto)
            } else {
                panic!("interpretation of lambda must be closure");
            }
        },
        &Expression::Box_(ref _box) => {
            let val = interp(new_loc, _box.as_ref(), env, sto);
            let nloc = new_loc();
            sto.insert(nloc, val.clone());
            Value::BoxV(nloc)
        },
        &Expression::Unbox_(ref body) => {
            if let Value::BoxV(loc) = interp(new_loc, body.as_ref(), env, sto) {
                fetch(loc, sto)
            } else {
                panic!("interpretation of Unbox's body must be a Box");
            }
        },
        &Expression::Setbox_(ref _box, ref _val) => {
            if let Value::BoxV(loc) = interp(new_loc, _box.as_ref(), env, sto) {
                let val = interp(new_loc, _val, env, sto);
                sto.insert(loc, val);
                Value::NilV
            } else {
                panic!("interpretation of Setbox's box must be a Box");
            }
        },
        _ => Value::NilV,
    }
}

pub fn interpret(expr: &Expression) -> Value {
    let mut store = Store::new();
    let mut new_loc = counter();
    interp(&mut new_loc, expr, &mut Env::MtEnv, &mut store)
}