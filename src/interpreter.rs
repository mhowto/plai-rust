use ty::Expression;

type Location = u64;

pub struct Binding {
    name: String,
    val: Location,
}

pub enum Env {
    mt_env,
    extend_env{with: Binding, rest: Box<Env>}
}

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

fn interp(expr: &Expression, env: &mut Env, sto: &mut Store) -> Value {
    match *expr {
        Expression::Nil     => Value::NilV,
        Expression::True    => Value::BoolV(true),
        Expression::False   => Value::BoolV(false),
        Expression::Num(n)  => Value::NumV(n),
        _                   => Value::NilV,
    }
}

pub fn interpret(expr: &Expression) -> Value {
    interp(expr, &mut Env::mt_env, &mut Store::mt_store)
}