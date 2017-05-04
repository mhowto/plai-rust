use ty::Expression;
use std::collections::{HashMap};
use std::vec::Vec;

type Location = u64;

pub fn counter() -> Box<FnMut() -> u64> {
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

pub type Env = Vec<Binding>;

/*
#[derive(Debug,PartialEq,Clone)]
pub enum Env {
    MtEnv,

    // 为什么用Box，不用&Env
    // 因为Env对rest有所有权，用&Env只是借用，并没有所有权
    ExtendEnv{with: Binding, rest: Box<Env>} 
}
*/

#[derive(Debug,PartialEq,Clone)]
pub enum Value {
    NilV,
    NumV(isize),
    BoolV(bool),
    ClosV{arg: String, body: Expression, env: Env},
    BoxV(Location),
    ListV(Vec<Value>),
    ObjV{ns: Vec<String>, vs: Vec<Value>},
}

/*
impl Value {
    pub fn to_string(&self) -> String {
        match *self {
            Value::NilV => String::from("Nil"),
            Value::NumV(n) => n.to_string(),
            Value::BoolV(b) => b.to_string(),
            Value::ClosV{arg: _, body: _, env: _} => String::from("ClosV"),
            Value::BoxV(loc) => format!("Box{{ Location: {} }}", loc),
            Value::ListV(ref vec) => format!("List"),
        }
    }
}
*/

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
    let mut iter = in_env.iter().rev();
    while let Some(ref binding) = iter.next() {
        if what.eq(&binding.name) {
            return binding.loc
        }
    }
    panic!(format!("lookup: unbound identifier '{}'", what));
}

fn fetch(what: Location, sto: &Store) -> Value {
    match sto.get(&what) {
        Some(val) => val.clone(),
        None => panic!("fetch not found")
    }
}

// lookup_msg: symbol * Value -> Value
// where the second argument is expected to be a objV
fn lookup_msg(method: String, obj: Value) -> Value {
    if let Value::ObjV{ref ns, ref vs}  =  obj {
        for x in 0..ns.len() {
            if method == ns[x] {
                return vs[x].clone()
            }
        }
    } else {
        panic!("The second argument must be a objV");
    }
    panic!("lookup_msg: method not found");
}

fn interp(new_loc: &mut Box<FnMut() -> u64>, expr: &Expression, env: &mut Env, sto: &mut Store) -> Value {
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
        &Expression::Equal(ref left, ref right) => {
            let r1 = interp(new_loc, left.as_ref(), env, sto);
            let r2 = interp(new_loc, right.as_ref(), env, sto);
            if r1 == r2 {
                Value::BoolV(true)
            } else {
                Value::BoolV(false)
            }
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
        &Expression::App{ref func, ref arg} => {
            let mut val = interp(new_loc, func, env, sto);
            if let Value::BoxV(loc) = val {
                val = fetch(loc, sto);
            }

            if let Value::ClosV { arg: ref clos_arg, body: ref clos_body, env: ref clos_env } = val {
                let arg_val = interp(new_loc, arg, env, sto);
                let nloc = new_loc();
                sto.insert(nloc, arg_val);
                let mut nenv = clos_env.clone();
                nenv.push(Binding { name: clos_arg.clone(), loc: nloc });
                interp(
                    new_loc,
                    clos_body,
                    &mut nenv,
                    sto)
            } else {
                panic!("interpretation of lambda must be closure")
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
        &Expression::Seq(ref s1, ref s2) => {
            interp(new_loc, s1.as_ref(), env, sto);
            interp(new_loc, s2.as_ref(), env, sto)
        },
        &Expression::Let{ref what_, ref to_, ref in_} => {
            let val = interp(new_loc, to_.as_ref(), env, sto);
            let nloc = new_loc();
            sto.insert(nloc, val);
            let mut nenv = env.clone();
            nenv.push(Binding{name: what_.clone(), loc: nloc});
            interp(new_loc, in_.as_ref(), &mut nenv, sto)
        },
        &Expression::List(ref list) => {
            let mut vec = Vec::new();
            for x in list {
                vec.push(interp(new_loc, x, env, sto));
            }
            Value::ListV(vec)
        },
        &Expression::Object{ref ns, ref vs} => {
            if let Value::ListV(vec) = interp(new_loc, vs, env, sto) {
                Value::ObjV{ns: ns.clone(), vs: vec}
            } else {
                panic!("interpretation of vs in object must be ListV");
            }
        },
        &Expression::Msg{ref obj, ref method} => lookup_msg(
            method.clone(),
            interp(new_loc, obj, env, sto)),
        &Expression::Define(ref name, ref val) => {
            let new_val = interp(new_loc, val, env, sto);
            let nloc = new_loc();
            env.push(Binding {name: name.clone(), loc: nloc});
            sto.insert(nloc, new_val);
            Value::NilV
        },
    }
}

pub fn interpret(expr: &Expression) -> Value {
    let mut store = Store::new();
    let mut new_loc = counter();
    let mut env = Vec::new();
    interp(&mut new_loc, expr, &mut env, &mut store)
}

use parser::expression;
use nom::{IResult};

pub fn execute(source_code: &[u8]) -> Value {
    let mut rest = source_code;
    let mut store = Store::new();
    let mut new_loc = counter();

    let mut env = Vec::new();
    loop {
        match expression(rest) {
            IResult::Done(_rest, ref expr) => {
                let val = interp(&mut new_loc, expr, &mut env, &mut store);
                if _rest.len() == 0 {
                    return val;
                } else {
                    rest = _rest;
                }
            },
            _ => {panic!("throws IResult::Error: parse panic");}
        }
    }
    unreachable!();
}