
// Grammar: like r5rs
// <program> :: <command or definition>
//
// <command or definition> :: <command>
//      | <definition>
//
// <command> :: <expression>
//
// <expression> :: (id <variable>)
//      | (box <expression>)
//      | (unbox <expression>)
//      | <literal>
//      | <lambda expression>
//      | <procedure call>
//      | <conditional>
//      | <assignment>
//      | <derived expression>
//
// <variable> :: <any <identifier> that isn't also a <syntactic keyword>>
//
// <syntactic keyword> :: <expression keyword>
// <expression keyword> ::
//      | id | app
//      | seq | lambda | let
//      | box | unbox | setbox
//      | nil | cons | node
//
// <litreal> :: <boolean> | <number>
// <boolean> :: (boolS ture) | (boolS false)
// <number> :: (numS  <num 10>)
// <num 10> :: <sign> <ureal R>
// <sign> :: <emptry> | + | -
// <ureal R> :: <digit>+
//
// <lambda expression> :: (lambda <formals> <body>)
// <formals> :: <variable>
// <body> :: <sequence>
// <sequence> :: <command> * <expression>  ----> <expression>+
//
// <procedure all> :: <function call>
//      | (<operator> <operand>*)
// <operator> :: + | - | bminusS | multS
// <operand> :: <expression>
//
// <function call> :: (app  <expression> <expression>)
//
// <conditional> :: (ifS <test> <consequent> <alternate>)
// <test> :: <expression>
// <consequent> :: <expression>
// <alternate> :: <expression> | <empty>
//
// <assignment>  :: (setbox <variable> <expression>)
//
// <definition> :: (let <variable> <expression>)
//
// <derived expression> :: (let <variable> <body>)    // lambda 表达式的<body>内的所有表达式被顺序求值
//      | (seq <sequence>)
//

#[macro_use]
#[allow(dead_code)]

extern crate nom;

use nom::{alpha, digit, IResult};

use std::str;
use std::str::FromStr;
use std::string::String;

#[derive(Debug,PartialEq)]
pub enum Expression {
    Nil,
    True,
    False,
    Num(i32),
    Plus(Box<Expression>, Box<Expression>),
    Uminus(Box<Expression>),
    Bminus(Box<Expression>, Box<Expression>),
    Mult(Box<Expression>, Box<Expression>),
    If {
        test: Box<Expression>,
        expr_if: Box<Expression>,
        else_expr: Option<Box<Expression>>,
    },
    ID(String), 
    Lambda { arg: String, body: Box<Expression> }, /*  Box_(Box<Expression>),
                                                    *  Unbox_(Box<Expression>),
                                                    *  Setbox_(Box<Expression>, Box<Expression>),
                                                    *  Seq(Box<Expression>, Box<Expression>), */
}

named!(expr_nil, tag!("nil"));

named!(unsigned_int<i32>, map_res!( // map_res! maps a function returning a Result on the Output of a parser
    map_res!(
        ws!(digit),
        str::from_utf8
    ),
    FromStr::from_str
));

named!(num_expr<i32>, map!( // map! maps a function on the result of a parser
    pair!(
        opt!(alt!(tag!("+") | tag!("-"))),
        unsigned_int
    ),
    | (sign, value): (Option<&[u8]>, i32) | {
        sign.and_then( |s| if s[0] == ('-' as u8) { Some(-1i32) } else {None}).unwrap_or(1i32) *value
    }
));

named!(expr_true, tag!("true"));
named!(expr_false, tag!("false"));

named!(expr_uminus<Expression>,
    do_parse!(
        tag!("(") >>
        tag!("-") >>
        v: expression >>
        tag!(")") >>
        (v)
    )
);

named!(expr_plus<(Expression, Expression)>, 
     do_parse!(
        tag!("(") >>
        tag!("+") >>
        left: expression >>
        right: expression >>
        tag!(")") >>
        (left, right)
    )
);

named!(expr_bminus<(Expression, Expression)>,
    do_parse!(
        tag!("(") >>
        tag!("-") >>
        left: expression >>
        right: expression >>
        tag!(")") >>
        (left, right)
    )
);

named!(expr_mult<(Expression, Expression)>,
    do_parse!(
        tag!("(") >>
        tag!("*") >>
        left: expression >>
        right: expression >>
        tag!(")") >>
        (left, right)
    )
);

named!(expr_if<(Expression, Expression, Option<Expression>)>,
    do_parse!(
        tag!("(") >>
        tag!("if") >>
        test: expression >>
        if_ex: expression >>
        else_ex: opt!(expression) >>
        tag!(")") >>
        (test, if_ex, else_ex)
    )
);

named!(expr_id<String>, do_parse!(
    s: map_res!(
        map_res!(
            ws!(alpha),
            str::from_utf8),
        FromStr::from_str
    ) >>
    (s)
));

named!(expr_lambda<(String, Expression)>, do_parse!(
    tag!("(") >>
    tag!("lambda") >>
    arg: map_res!(
        map_res!(
            ws!(alpha),
            str::from_utf8),
        FromStr::from_str
    ) >>
    body: expression >>
    tag!(")") >>
    (arg, body)
));

named!(expression<Expression>,
    ws!( // ws! transforms a parser to automatically consume whitespace between each token.
        alt!(  // alt! try a list of parsers, return the result of the first successful one
            expr_nil        => { |_| Expression::Nil }
            | expr_true     => { |_| Expression::True }
            | expr_false    => { |_| Expression::False }
            | num_expr      => { |num| Expression::Num(num) }
            | expr_uminus   => { |val| Expression::Uminus(Box::new(val)) }
            | expr_bminus   => { | (left, right) | Expression::Bminus(Box::new(left), Box::new(right)) }
            | expr_plus     => { | (left, right) | Expression::Plus(Box::new(left), Box::new(right)) }
            | expr_mult     => { | (left, right) | Expression::Mult(Box::new(left), Box::new(right)) }
            | expr_if       => { | (test, if_ex, else_ex) | Expression::If{
                test: Box::new(test),
                expr_if: Box::new(if_ex),
                else_expr: { match else_ex {
                    None => None,
                    Some(ex) => Some(Box::new(ex))
                }}}}
            | expr_id       => { |s| Expression::ID(s)}
            | expr_lambda   => { | (arg, body) | Expression::Lambda{
                arg: arg,
                body: Box::new(body)
                }}
        )
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_plus() {
        assert_eq!(expression(b"(+ 1 2)"), IResult::Done(&b""[..], Expression::Plus(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))));
    }

    #[test]
    fn parse_uminus() {
        assert_eq!(expression(b"(- 1)"), IResult::Done(&b""[..], Expression::Uminus(Box::new(Expression::Num(1)))));
    }

    #[test]
    fn parse_uminus2() {
        assert_eq!(expression(b"(- (+ 1 2))"), IResult::Done(&b""[..], Expression::Uminus(Box::new(Expression::Plus(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))))));
    }

    #[test]
    fn parse_bminus() {
        assert_eq!(expression(b"(- 1 2)"), IResult::Done(&b""[..], Expression::Bminus(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))));
    }

    #[test]
    fn parse_mult() {
        assert_eq!(expression(b"(* 1 2)"), IResult::Done(&b""[..], Expression::Mult(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))));
    }

    #[test]
    fn parse_if() {
        assert_eq!(expression(b"(if (+ 1 2) 3 4)"), IResult::Done(&b""[..], Expression::If{
            test: Box::new(Expression::Plus(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))),
            expr_if: Box::new(Expression::Num(3)),
            else_expr: Some(Box::new(Expression::Num(4)))
        }));
    }

/*
    #[test]
    fn parse_id() {
        assert_eq!(expression(b"(id x)"), IResult::Done(&b""[..], Expression::ID(String::from("x"))));
    }
*/

    #[test]
    fn parse_lambda() {
        assert_eq!(expression(b"(lambda x (+ x 1))"),
            IResult::Done(&b""[..],
                Expression::Lambda{
                    arg: String::from("x"),
                    body: Box::new(
                        Expression::Plus(
                            Box::new(Expression::ID(String::from("x"))),
                            Box::new(Expression::Num(1))))
                }));
    }
}
