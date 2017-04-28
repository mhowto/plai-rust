extern crate plai_rust;
extern crate nom;

use plai_rust::parser::expression;
use plai_rust::ty::Expression;
use plai_rust::interpreter::{interpret, Value, Env};

use nom::IResult;

#[test]
fn test_plus() {
    let plus_raw_string = b"(+ 1 2)";
    let plus_expr = Expression::Plus(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)));

    if let IResult::Done(_, expr) = expression(plus_raw_string) {
        assert_eq!(expr, plus_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(3));
        assert_eq!(result.to_string(), String::from("3"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_uminus() {
    let minus_raw_string = b"(- 1)";
    let minus_expr = Expression::Uminus(Box::new(Expression::Num(1)));

    if let IResult::Done(_, expr) = expression(minus_raw_string) {
        assert_eq!(expr, minus_expr);
        
        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(-1));
        assert_eq!(result.to_string(), String::from("-1"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_uminus2() {
    let uminus_raw_string = b"(- (+ 1 2))";
    let uminus_expr = Expression::Uminus(Box::new(Expression::Plus(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))));

    if let IResult::Done(_, expr) = expression(uminus_raw_string) {
        assert_eq!(expr, uminus_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(-3));
        assert_eq!(result.to_string(), String::from("-3"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_bminus() {
    let bminus_raw_string = b"(- 1 2)";
    let bminus_expr = Expression::Bminus(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)));

    if let IResult::Done(_, expr) = expression(bminus_raw_string) {
        assert_eq!(expr, bminus_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(-1));
        assert_eq!(result.to_string(), String::from("-1"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_mult() {
    let mult_raw_string = b"(* 1 2)";
    let mult_expr = Expression::Mult(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)));

    if let IResult::Done(_, expr) = expression(mult_raw_string) {
        assert_eq!(expr, mult_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(2));
        assert_eq!(result.to_string(), String::from("2"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_bool() {
    let true_raw_string = b"true";
    let true_expr = Expression::True;
    let false_raw_string = b"false";
    let false_expr = Expression::False;

    if let IResult::Done(_, expr) = expression(true_raw_string) {
        assert_eq!(expr, true_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::BoolV(true));
        assert_eq!(result.to_string(), String::from("true"));
    } else {
        assert!(false);
    }

    if let IResult::Done(_, expr) = expression(false_raw_string) {
        assert_eq!(expr, false_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::BoolV(false));
        assert_eq!(result.to_string(), String::from("false"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_if_true() {
    let if_raw_string = b"(if true 3 4)";
    let if_expr = Expression::If{
        test: Box::new(Expression::True),
        expr_if: Box::new(Expression::Num(3)),
        else_expr: Some(Box::new(Expression::Num(4)))
    };

    if let IResult::Done(_, expr) = expression(if_raw_string) {
        assert_eq!(expr, if_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(3));
        assert_eq!(result.to_string(), String::from("3"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_if_false() {
    let if_raw_string = b"(if false 3 4)";
    let if_expr = Expression::If{
        test: Box::new(Expression::False),
        expr_if: Box::new(Expression::Num(3)),
        else_expr: Some(Box::new(Expression::Num(4)))
    };

    if let IResult::Done(_, expr) = expression(if_raw_string) {
        assert_eq!(expr, if_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(4));
        assert_eq!(result.to_string(), String::from("4"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_lambda() {
    let lambda_raw_string = b"(lambda x (+ x 1))";
    let lambda_expr = Expression::Lambda{
        arg: String::from("x"),
        body: Box::new(
            Expression::Plus(
                Box::new(Expression::ID(String::from("x"))),
                Box::new(Expression::Num(1))))
    };

    if let IResult::Done(_, expr) = expression(lambda_raw_string) {
        assert_eq!(expr, lambda_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::ClosV{
            arg: String::from("x"),
            body: Expression::Plus(Box::new(Expression::ID(String::from("x"))), Box::new(Expression::Num(1))),
            env: Env::MtEnv});
        assert_eq!(result.to_string(), String::from("ClosV"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_app() {
    let app_raw_string = b"(app (lambda x (+ x 1)) 2)";
    let app_expr = Expression::App{
        func: Box::new(Expression::Lambda{
            arg: String::from("x"),
            body: Box::new(Expression::Plus(
                Box::new(Expression::ID(String::from("x"))),
                Box::new(Expression::Num(1))))
        }),
        arg: Box::new(Expression::Num(2))
    };

    if let IResult::Done(_, expr) = expression(app_raw_string) {
        assert_eq!(expr, app_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(3));
        assert_eq!(result.to_string(), String::from("3"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_box() {
    let box_raw_string = b"(box 3)";
    let box_expr = Expression::Box_(Box::new(Expression::Num(3)));

    if let IResult::Done(_, expr) = expression(box_raw_string) {
        assert_eq!(expr, box_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::BoxV(1));
        assert_eq!(result.to_string(), String::from("Box{ Location: 1 }"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_unbox() {
    let unbox_raw_string = b"(unbox (box 3))";
    let unbox_expr = Expression::Unbox_(Box::new(Expression::Box_(Box::new(Expression::Num(3)))));

    if let IResult::Done(_, expr) = expression(unbox_raw_string) {
        assert_eq!(expr, unbox_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(3));
        assert_eq!(result.to_string(), String::from("3"));
    } else {
        assert!(false);
    }
}

#[test]
fn test_setbox() {
    let setbox_raw_string = b"(setbox (box 3) 4)";
    let setbox_expr = Expression::Setbox_(
        Box::new(Expression::Box_(Box::new(Expression::Num(3)))),
        Box::new(Expression::Num(4)));

    if let IResult::Done(_, expr) = expression(setbox_raw_string) {
        assert_eq!(expr, setbox_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NilV);
    } else {
        assert!(false);
    }
}

#[test]
fn test_seq() {
    let seq_raw_string = b"(seq 2 3)";
    let seq_expr = Expression::Seq(
        Box::new(Expression::Num(2)),
        Box::new(Expression::Num(3)));

    if let IResult::Done(_, expr) = expression(seq_raw_string) {
        assert_eq!(expr, seq_expr);
    } else {
        assert!(false);
    }
}

#[test]
fn test_let() {
    let let_raw_string = b"(let a 4 (* 3 a))";
    let let_expr = Expression::Let{
        what_: String::from("a"),
        to_: Box::new(Expression::Num(4)),
        in_: Box::new(Expression::Mult(
            Box::new(Expression::Num(3)),
            Box::new(Expression::ID(String::from("a")))))};

    if let IResult::Done(_, expr) = expression(let_raw_string) {
        assert_eq!(expr, let_expr);
    } else {
        assert!(false);
    }
}

/*
(+ (let ([b (box 0)])
     1)
   b)

should error
*/

/*
(let ([a (box 1)])
  (let ([f (lambda (x) (+ x (unbox a)))])
    (begin
      (set-box! a 2)
      (f 10))))

12
*/