#[derive(Debug,PartialEq,Clone)]
pub enum Expression {
    Nil,
    True,
    False,
    Num(isize),
    Plus(Box<Expression>, Box<Expression>),
    Uminus(Box<Expression>),
    Bminus(Box<Expression>, Box<Expression>),
    Mult(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    If {
        test: Box<Expression>,
        expr_if: Box<Expression>,
        else_expr: Option<Box<Expression>>,
    },
    ID(String), 
    Lambda { arg: String, body: Box<Expression> },
    App {func: Box<Expression>, arg: Box<Expression>},
    Box_(Box<Expression>),
    Unbox_(Box<Expression>),
    Setbox_(Box<Expression>, Box<Expression>),
    Seq(Box<Expression>, Box<Expression>),
    Let {what_: String, to_: Box<Expression>, in_: Box<Expression>}
}