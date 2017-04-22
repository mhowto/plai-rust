use parser::Expression;

type Location = u64;

struct Binding {
    name: String,
    val: Location,
}

enum Env {
    mt_env,
    extend_env{with: Binding, rest: Box<Env>}
}

enum Value {
    NumV(isize),
    BoolV(bool),
    ClosV{arg: String, body: Expression, env: Env},
    BoxV(Location),
}

impl Value {
    fn to_string(&self) -> String {
        match *self {
            Value::NumV(n) => n.to_string(),
            Value::BoolV(b) => b.to_string(),
            Value::ClosV { ref arg, ref body, ref env } => String::from("Unknown"),
            Value::BoxV(_) => String::from("Unknown")
        }
    }
}

struct Storage {
    location: Location,
    val: Value,
}

enum Store {
    mt_store,
    override_store{with: Storage, rest: Box<Store>}
}

struct IResult {
    val: Value,
    sto: Store,
}

fn interp(expr: Expression, env: Env, sto: Store) -> IResult {
    IResult {val: Value::NumV(3), sto: Store::mt_store}
}

fn interpret(expr: Expression) -> IResult {
    interp(expr, Env::mt_env, Store::mt_store)
}

#[cfg(test)]
mod tests {
    use parser::expression;
    use super::interpret;
    use nom::IResult;

    #[test]
    fn interp_plus() {
        if let IResult::Done(_, expr) = expression(b"(+ 1 2)") {
            let result = interpret(expr);
            assert_eq!(result.val.to_string(), String::from("3"));
        } else {
            assert!(false);
        }
        // let expr = expression(b"(+ 1 2)");
        // assert_eq!(expr.is_done(), true);
        // let result = interpret(expr.unwrap());
        // assert_eq!(result.val.to_string(), String::from("3"));
    }
}