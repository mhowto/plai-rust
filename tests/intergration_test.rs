extern crate plai_rust;
extern crate nom;

use plai_rust::parser::{expression, expr_list_id};
use plai_rust::ty::Expression;
use plai_rust::interpreter::{interpret, Value, execute};

use nom::{IResult};

#[test]
fn test_plus() {
    let plus_raw_string = b"(+ 1 2)";
    let plus_expr = Expression::Plus(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)));

    if let IResult::Done(_, expr) = expression(plus_raw_string) {
        assert_eq!(expr, plus_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(3));
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
    } else {
        assert!(false);
    }

    if let IResult::Done(_, expr) = expression(false_raw_string) {
        assert_eq!(expr, false_expr);

        let result = interpret(&expr);
        assert_eq!(result, Value::BoolV(false));
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
            env: Vec::new()});
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

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(3));
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

        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(12));
    } else {
        assert!(false);
    }
}

#[test]
#[should_panic(expected = "lookup: unbound identifier 'b'")]
fn test_lexical_scope_1() {
    let raw_string = b"(+ (let b
                               (box 0)
                               1)
                           b))";
    if let IResult::Done(_, expr) = expression(raw_string) {
        interpret(&expr);
    } else {
        assert!(false);
    }
}

#[test]
fn test_lexical_scope_2() {
    let raw_string = b"
    (let a
         (box 1)
         (let f
              (lambda x
                      (+ x
                         (unbox a)))
              (seq (setbox a 2)
                   (app f 10))))";

    if let IResult::Done(_, expr) = expression(raw_string) {
        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(12));
    } else {
        assert!(false);
    }
}

#[test]
#[should_panic(expected = "lookup: unbound identifier 'fact'")]
fn test_recursive_function_panic() {
    let raw_string = b"
    (let fact
         (lambda n
                 (if (= n 0)
                     1
                     (* n
                        (app fact (- n 1))
                     )
                 )
         )
         (app fact 10)
    )";

    if let IResult::Done(_, expr) = expression(raw_string) {
        interpret(&expr);
    } else {
        assert!(false);
    }
}

#[test]
fn test_recursive_function_fact() {
    let raw_string = b"
    (let fact 
         (box 3)
         (seq (setbox fact
                      (lambda n
                              (if (= n 0)
                                  1
                                  (* n (app fact (- n 1)))
                              )
                      )
              )
              (app fact 10)))";

    if let IResult::Done(_, expr) = expression(raw_string) {
        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(3628800))
    } else {
        assert!(false);
    }
}

#[test]
fn test_cyclic_data() {
    let raw_string = b"
    (let b
         (box 3)
         (seq (setbox b b)
              (unbox (unbox b))))";
    
    if let IResult::Done(_, expr) = expression(raw_string) {
        let result = interpret(&expr);
        assert_eq!(result, Value::BoxV(1));
    } else {
        assert!(false);
    }
}

#[test]
fn test_list() {
    let raw_string = b"(list 1 2)";

    if let IResult::Done(_, expr) = expression(raw_string) {
        let result = interpret(&expr);
        assert_eq!(result, Value::ListV([Value::NumV(1), Value::NumV(2)].to_vec()));

    } else {
        assert!(false);
    }
}

#[test]
fn test_list_id() {
    let raw_string = b"(list add1 sub1 mult1)";

    if let IResult::Done(_, expr) = expr_list_id(raw_string) {
        assert_eq!(expr.len(), 3);
        assert_eq!(expr[0], String::from("add1"));
        assert_eq!(expr[1], String::from("sub1"));
        assert_eq!(expr[2], String::from("mult1"));

        // let result = interpret(&expr);
        // assert_eq!(result, Value::ListV([Value:: Value::ID("a"), Value::ID("b"), Value::ID("c")].to_vec()));
    } else {
        assert!(false);
    }
}

#[test]
fn test_list_lambda() {
    let raw_string = b"
           (list (lambda x (+ x 1))
                 (lambda x (+ x -1)))";

    if let IResult::Done(_, _) = expression(raw_string) {
    } else {
        assert!(false);
    }
}

#[test]
fn test_object_parse() {
    let raw_string = b"
        (obj 
            (list add1 sub1)
            (list (lambda x (+ x 1))
                  (lambda x (+ x -1))))";
    if let IResult::Done(_, _) = expression(raw_string) {
    } else {
        assert!(false);
    }
}

#[test]
fn test_object() {
    let raw_string = b"
    (let o
         (obj (list add1 sub1)
              (list (lambda x (+ x 1))
                    (lambda x (+ x -1))))
         (msg o add1 3))";

    if let IResult::Done(_, expr) = expression(raw_string) {
        let result = interpret(&expr);
        assert_eq!(result, Value::NumV(4));
    } else {
        assert!(false);
    }
}

#[test]
#[should_panic(expected = "throws IResult::Error: parse panic")]
fn test_execute_parse_error() {
    let raw_string = b"((+ 1 2)";
    execute(raw_string);
}

#[test]
fn test_execute_object() {
    let raw_string = b"
    (let o
         (obj (list add1 sub1)
              (list (lambda x (+ x 1))
                    (lambda x (+ x -1))))
         (msg o add1 3))";

    let result = execute(raw_string);
    assert_eq!(result, Value::NumV(4));
}

#[test]
fn test_execute_define_simple() {
    let raw_string = b"
    (define add
      (lambda x (+ x 1)))
    (app add 3)
    ";

    let result = execute(raw_string);
    assert_eq!(result, Value::NumV(4));
}

#[test]
fn test_objects_as_named_collections() {
    let raw_string_1 = b"
    (define o1
      (lambda m
        (if (= m 0) (lambda x (+ x 1))
                    (lambda x (- x 1)))))
    (app (app o1 1) 5)";

    let result_1 = execute(raw_string_1);
    assert_eq!(result_1, Value::NumV(4));

    let raw_string_2 = b"
    (define o1
      (lambda m
        (if (= m 0) (lambda x (+ x 1))
                    (lambda x (- x 1)))))
    (app (app o1 0) 5)";

    let result_2 = execute(raw_string_2);
    assert_eq!(result_2, Value::NumV(6));
}