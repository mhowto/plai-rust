
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

use nom::{alphanumeric, digit};

use std::str;
use std::str::FromStr;
use std::string::String;
use ty::Expression;

named!(expr_nil, tag!("nil"));

named!(unsigned_int<isize>, map_res!( // map_res! maps a function returning a Result on the Output of a parser
    map_res!(
        ws!(digit),
        str::from_utf8
    ),
    FromStr::from_str
));

named!(expr_symbol<String>, map_res!(
    map_res!(
        ws!(alphanumeric),
        // ws!(alpha),
        str::from_utf8),
    FromStr::from_str)); 

named!(num_expr<isize>, map!( // map! maps a function on the result of a parser
    pair!(
        opt!(alt!(tag!("+") | tag!("-"))),
        unsigned_int
    ),
    | (sign, value): (Option<&[u8]>, isize) | {
        sign.and_then( |s| if s[0] == ('-' as u8) { Some(-1isize) } else {None}).unwrap_or(1isize) *value
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

named!(expr_equal<(Expression, Expression)>,
    do_parse!(
        tag!("(") >>
        tag!("=") >>
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
    id: expr_symbol >>
    (id)
));

named!(expr_list<Vec<Expression> >, do_parse!(
    tag!("(") >>
    tag!("list") >>
    elements: many1!(expression) >>
    tag!(")") >>
    (elements)
));

named!(pub expr_list_id<Vec<String> >, do_parse!(
    tag!("(") >>
    tag!("list") >>
    ids: many1!(expr_symbol) >>
    tag!(")") >>
    (ids)
));

named!(pub expr_object<(Vec<String>, Vec<Expression>)>, ws!(do_parse!(
    tag!("(") >>
    tag!("obj") >>
    ns: expr_list_id >>
    vs: expr_list >>
    tag!(")") >>
    (ns, vs))
));

named!(expr_msg<(Expression, String, Expression)>, do_parse!(
    tag!("(") >>
    tag!("msg") >>
    object_id: expression >>
    method_name: expr_symbol >>
    argument: expression >>
    tag!(")") >>
    (object_id, method_name, argument)
));

named!(expr_lambda<(String, Expression)>, do_parse!(
    tag!("(") >>
    tag!("lambda") >>
    arg: expr_symbol >>
    body: expression >>
    tag!(")") >>
    (arg, body)
));

named!(expr_app<(Expression, Expression)>, do_parse!(
    tag!("(") >>
    tag!("app") >>
    func: expression >>
    arg: expression >>
    tag!(")") >>
    (func, arg)
));

named!(expr_box<Expression>, do_parse!(
    tag!("(") >>
    tag!("box") >>
    arg: expression >>
    tag!(")") >>
    (arg)
));

named!(expr_unbox<Expression>, do_parse!(
    tag!("(") >>
    tag!("unbox") >>
    arg: expression >>
    tag!(")") >>
    (arg)
));

named!(expr_setbox<(Expression, Expression)>, do_parse!(
    tag!("(") >>
    tag!("setbox") >>
    b: expression >>
    v: expression >>
    tag!(")") >>
    (b, v)
));

named!(expr_seq<(Expression, Expression)>, do_parse!(
    tag!("(") >>
    tag!("seq") >>
    b1: expression >>
    b2: expression >>
    tag!(")") >>
    (b1, b2)
));

named!(expr_let<(String, Expression, Expression)>, do_parse!(
    tag!("(") >>
    tag!("let") >>
    what_: expr_symbol >>
    to_: expression >>
    in_: expression >>
    tag!(")") >>
    (what_, to_, in_)
));

named!(expr_define<(String, Expression)>, do_parse!(
    tag!("(") >>
    tag!("define") >> 
    name: expr_symbol >>
    val: expression >>
    tag!(")") >>
    (name, val)
));

/*
named!(expr_defin_fn<(String, String, Expression)>, do_parse!(
));
*/

named!(pub expression<Expression>,
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
            | expr_equal    => { | (left, right) | Expression::Equal(Box::new(left), Box::new(right)) }
            | expr_if       => { | (test, if_ex, else_ex) | Expression::If{
                test: Box::new(test),
                expr_if: Box::new(if_ex),
                else_expr: { match else_ex {
                    None => None,
                    Some(ex) => Some(Box::new(ex))
                }}}}
            | expr_id       => { |s| Expression::ID(s)}
            | expr_list     => { |elements| Expression::List(elements) }
            | expr_object   => { |(ns, vs)| Expression::Object{ns: ns, vs: Box::new(Expression::List(vs))} }
            | expr_lambda   => { |(arg, body)| Expression::Lambda{
                arg: arg,
                body: Box::new(body)
                }}
            | expr_msg      => { |(object_id, method_name, argument)| Expression::App{
                func: Box::new(Expression::Msg{obj: Box::new(object_id), method: method_name}),
                arg: Box::new(argument)} }
            | expr_app      => { |(func, arg)| Expression::App{
                func: Box::new(func),
                arg: Box::new(arg)} }
            | expr_box      => { |arg| Expression::Box_(Box::new(arg))}
            | expr_unbox    => { |arg| Expression::Unbox_(Box::new(arg))}
            | expr_setbox   => { |(b, v)| Expression::Setbox_(Box::new(b), Box::new(v))}
            | expr_seq      => { |(b1, b2)| Expression::Seq(Box::new(b1), Box::new(b2))}
            | expr_let      => { |(what_, to_, in_)| Expression::Let{
                what_: what_,
                to_: Box::new(to_), 
                in_: Box::new(in_)} }
            | expr_define      => { |(name, val)| Expression::Define(name, Box::new(val)) }
        )
    )
);