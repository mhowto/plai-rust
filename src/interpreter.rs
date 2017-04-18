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

struct Storage {
    location: Location,
    val: Value,
}

enum Store {
    mt_store,
    override_store{with: Storage, rest: Box<Store>}
}

struct PResult {
    val: Value,
    sto: Store,
}

fn interp(expr: Expression, env: Env, sto: Store) -> PResult {
    PResult {val: Value::NumV(1), sto: Store::mt_store}
}