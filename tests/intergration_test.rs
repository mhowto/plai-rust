extern crate plai_rust;
extern crate nom;

use plai_rust::parser::expression;
use plai_rust::ty::Expression;
use plai_rust::interpreter::{interpret, Value};

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
        
        // let result = interpret(expr);
        // assert_eq!(result.val.to_string(), String::from);
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
    } else {
        assert!(false);
    }
}

#[test]
fn test_if() {
    let if_raw_string = b"(if (+ 1 2) 3 4)";
    let if_expr = Expression::If{
        test: Box::new(Expression::Plus(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))),
        expr_if: Box::new(Expression::Num(3)),
        else_expr: Some(Box::new(Expression::Num(4)))
    };

    if let IResult::Done(_, expr) = expression(if_raw_string) {
        assert_eq!(expr, if_expr);
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