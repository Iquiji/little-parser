use core::{panic, fmt};
use std::{rc::Rc, fmt::Display};

use scanner::Token;

mod scanner;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    token_idx: usize,
    call_stack: Vec<String>,
}
impl Parser {
    pub fn init_with_string(program: &str) -> Parser {
        Parser {
            tokens: scanner::scan_to_token_list(program),
            token_idx: 0,
            call_stack: vec![],
        }
    }
    fn current(&self) -> Token {
        self.tokens[self.token_idx].clone()
    }
    fn current_type(&self) -> scanner::ScannerTokenTypes {
        self.tokens[self.token_idx].ttype.clone()
    }
    fn _next(&self) -> Token {
        self.tokens[self.token_idx + 1].clone()
    }
    fn next_type(&self) -> scanner::ScannerTokenTypes {
        self.tokens[self.token_idx + 1].ttype.clone()
    }
    fn previous(&self) -> Token {
        self.tokens[self.token_idx - 1].clone()
    }
    fn advance(&mut self) -> Token {
        self.token_idx += 1;
        self.previous()
    }
    /// Advance if type is expected type else return false
    fn accept_if(&mut self, accepted_type: scanner::ScannerTokenTypes) -> bool {
        if self.current_type() == accepted_type {
            self.advance();
            true
        } else {
            false
        }
    }
    /// Advance if type is expected type else return error to eprintln!
    fn expect_type(&mut self, accepted_type: scanner::ScannerTokenTypes) -> bool {
        if self.accept_if(accepted_type) {
            true
        } else {
            eprintln!(
                "Unexpected Token: {:?}! Expected Type: {:?}",
                self.current(),
                accepted_type
            );
            false
        }
    }

    pub fn re_program(&mut self) -> Programm {
        let mut result = vec![];

        while self.current_type() != scanner::ScannerTokenTypes::EOF {
            result.push(self.re_expr());
        }

        Programm::Expression(result)
    }
    fn re_expr(&mut self) -> Expression {
        use scanner::KeywordEnum::*;
        use scanner::ScannerTokenTypes::*;

        // println!(
        //     "Self: {:#?},\ncurrent_type: {:#?}",
        //     self,
        //     self.current_type()
        // );

        if self.accept_if(LeftParantheses) {
            if self.accept_if(Keyword(Quote)) {
                // (quote <datum>) | ' <datum>
                self.call_stack.push("Quote".to_owned());

                let atom_to_quote = self.re_atom();
                self.expect_type(RightParantheses);

                self.call_stack.pop();
                return Expression::Quote(atom_to_quote);
            } else if self.accept_if(Keyword(Lambda)) {
                // (lambda <formals> <body>) bzw. (lambda (x y ...) Expr...)
                self.call_stack.push("Lambda".to_owned());
                
                self.expect_type(LeftParantheses);
                let mut formals = vec![];
                while self.accept_if(Identifier) {
                    formals.push(self.previous().lexeme);
                }
                self.expect_type(RightParantheses);

                let mut body = vec![];
                while self.current_type() != RightParantheses {
                    body.push(self.re_expr());
                }
                self.expect_type(RightParantheses);

                self.call_stack.pop();
                return Expression::Lambda(formals, body);
            } else if self.accept_if(Keyword(Cond)) {
                // (Cond (if then)(if2 then2)...)
                self.call_stack.push("cond".to_owned());

                let mut cond_cases = vec![];
                while self.accept_if(LeftParantheses) {
                    if self.accept_if(Keyword(Else)) {
                        cond_cases
                            .push((Expression::Atom(AtomTypes::Boolean(true)), self.re_expr()));
                    } else {
                        cond_cases.push((self.re_expr(), self.re_expr()));
                    }
                    self.expect_type(RightParantheses);
                }
                self.expect_type(RightParantheses);

                self.call_stack.pop();
                return Expression::Cond(cond_cases);
            } else if self.accept_if(Keyword(Let)) {
                // (let ((x value)(y value)...) Expr...)
                self.call_stack.push("let".to_owned());

                let mut bindings = vec![];

                self.expect_type(LeftParantheses);

                while self.accept_if(LeftParantheses) {
                    self.expect_type(Identifier);
                    let var_name = self.previous().lexeme;
                    bindings.push((var_name, self.re_expr()));
                    self.expect_type(RightParantheses);
                }
                self.expect_type(RightParantheses);

                let mut body = vec![];
                while self.current_type() != RightParantheses {
                    println!("Let body xD");
                    body.push(self.re_expr());
                }
                self.expect_type(RightParantheses);

                self.call_stack.pop();
                return Expression::Let(bindings, body);
            } else if self.accept_if(Keyword(Define)) {
                // (define <variable> <expression>)
                self.call_stack.push("define".to_owned());

                self.expect_type(Identifier);
                let name_to_bind_to = self.previous().lexeme;
                let define_to = self.re_expr();
                self.expect_type(RightParantheses);

                self.call_stack.pop();
                return Expression::Define(name_to_bind_to, Rc::new(define_to));
            } else if self.accept_if(Keyword(Else)) {
                eprintln!("Unexpected Else at beginning of Expr!");
                panic!();
            } else if self.accept_if(Identifier) {
                // (add1 1)
                self.call_stack.push("parenthesised identifier".to_owned());

                let ident_call_to = self.previous().lexeme;
                let mut expr_to_call_with = vec![];
                while self.current_type() != RightParantheses {
                    expr_to_call_with.push(self.re_expr());
                }
                self.expect_type(RightParantheses);

                self.call_stack.pop();
                return Expression::LambdaCall(
                    Rc::new(Expression::Identifier(ident_call_to)),
                    expr_to_call_with,
                );
            } else {
                // ((lambda ...) ...)
                eprintln!("Experimental Expr first as arg");
                let first_expr = self.re_expr();
                if let Expression::Lambda(bindings, body) = first_expr {
                    self.call_stack.push("Lambda as first in Parenthesised Expr".to_owned());

                    let mut expr_to_call_with = vec![];
                    while self.current_type() != RightParantheses {
                        expr_to_call_with.push(self.re_expr());
                    }

                    self.expect_type(RightParantheses);

                    self.call_stack.pop();
                    return Expression::LambdaCall(
                        Rc::new(Expression::Lambda(bindings, body)),
                        expr_to_call_with,
                    );
                }
                // (((lambda ...) x y z) ...)
                if let Expression::LambdaCall(to_call, args) = first_expr {
                    self.call_stack.push("Lambda call as first in Paranthesised Expr".to_owned());

                    let mut expr_to_call_with = vec![];
                    while self.current_type() != RightParantheses {
                        expr_to_call_with.push(self.re_expr());
                    }
                    
                    self.expect_type(RightParantheses);

                    self.call_stack.pop();
                    return Expression::LambdaCall(
                        Rc::new(Expression::LambdaCall(to_call, args)),
                        expr_to_call_with,
                    );
                }
                panic!(
                    "cant have expr that isnt a lambda or lambda-call as first in expr: {:?}",
                    first_expr
                );
            }
        } else if self.accept_if(Identifier) {
            self.call_stack.push("Ident first in Expr".to_owned());

            self.call_stack.pop();
            return Expression::Identifier(self.previous().lexeme);
        } else if self.accept_if(String) || self.accept_if(Boolean) || self.accept_if(Number) {
            self.call_stack.push("String Boolean or Number first in Expr".to_owned());

            self.call_stack.pop();
            return Expression::Atom(match self.previous().ttype {
                String => AtomTypes::String(self.previous().lexeme),
                Number => {
                    AtomTypes::Integer(i32::from_str_radix(&self.previous().lexeme, 10).unwrap())
                }
                Boolean => AtomTypes::Boolean(match self.current().lexeme.as_str() {
                    "#f" => false,
                    "#t" => true,
                    _ => unreachable!(),
                }),
                _ => unreachable!(),
            });
        } else if self.accept_if(Keyword(Quote)) {
            // 'datum
            self.call_stack.push("Quote first in Expr".to_owned());

            let atom_to_quote = self.re_atom();

            self.call_stack.pop();
            return Expression::Atom(atom_to_quote);
        }

        panic!("End of Expression Parser!!! should be unreachable! current: {:?}, dump: {:?}",self.current() ,self);
    }

    fn re_atom(&mut self) -> AtomTypes {
        match self.current_type() {
            scanner::ScannerTokenTypes::LeftParantheses => {
                self.expect_type(scanner::ScannerTokenTypes::LeftParantheses);
                // (...)
                let mut atom_list = vec![];
                while self.current_type() != scanner::ScannerTokenTypes::RightParantheses {
                    atom_list.push(self.re_atom());
                }
                self.expect_type(scanner::ScannerTokenTypes::RightParantheses);
                AtomTypes::List(atom_list)
            }
            scanner::ScannerTokenTypes::Identifier => AtomTypes::Symbol(self.advance().lexeme),
            scanner::ScannerTokenTypes::String => AtomTypes::String(self.advance().lexeme),
            scanner::ScannerTokenTypes::Number => {
                AtomTypes::Integer(i32::from_str_radix(&self.advance().lexeme, 10).unwrap())
            }
            scanner::ScannerTokenTypes::Boolean => {
                AtomTypes::Boolean(match self.advance().lexeme.as_str() {
                    "#f" => false,
                    "#t" => true,
                    _ => unreachable!(),
                })
            }
            err => panic!("Unexpected '{:?}'", err),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Programm {
    Expression(Vec<Expression>),
}

type Identifier = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Quote(AtomTypes),
    /// (Formals, Body)
    Lambda(Vec<Identifier>, Vec<Expression>),
    Cond(Vec<(Expression, Expression)>),
    Define(Identifier, Rc<Expression>),
    /// (Bindings, Body)
    Let(Vec<(Identifier, Expression)>, Vec<Expression>),
    LambdaCall(Rc<Expression>, Vec<Expression>),
    Atom(AtomTypes),
    Identifier(String),
}
impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Quote(to_display) => write!(f, "Quote - {}", to_display),
            Expression::Lambda(formals, body) => {
                write!(f, "Lambda def:\n").unwrap();
                write!(f, "-formals:\n").unwrap();
                for formal in formals{
                    write!(f, "{} ",formal).unwrap();
                }
                write!(f, "-body:\n").unwrap();
                for expr in body{
                    write!(f, "     ->{}\n",expr).unwrap();
                }
                write!(f, "\n")
            },
            Expression::Cond(conds) => {
                write!(f, "Cond:").unwrap();
                write!(f, "-conds:").unwrap();
                for expr in conds{
                    write!(f, "     ->{} => {}",expr.0,expr.1).unwrap();
                }
                write!(f, "\n")
            },
            Expression::Define(define_name, define_expr) => {
                write!(f, "Define def:\n").unwrap();
                write!(f, "-name: {}\n",define_name).unwrap();

                write!(f, "-body:\n").unwrap();
                write!(f, "     ->{}\n",define_expr).unwrap();
                write!(f, "\n")
            },
            Expression::Let(bindings, body) => {
                write!(f, "Let expr:\n").unwrap();
                write!(f, "-formals:\n").unwrap();
                for expr in bindings{
                    write!(f, "     ->{} <= {}\n",expr.0,expr.1).unwrap();
                }
                write!(f, "-body:\n").unwrap();
                for expr in body{
                    write!(f, "     ->{}\n",expr).unwrap();
                }
                write!(f, "\n")
            },
            Expression::LambdaCall(to_call, args) => {
                write!(f, "Lambda Call:\n").unwrap();
                write!(f, "-to call:\n").unwrap();
                write!(f, "{}",to_call).unwrap();

                write!(f, "-args:\n").unwrap();
                for expr in args{
                    write!(f, "     ->{}\n",expr).unwrap();
                }
                write!(f, "\n")
            },
            Expression::Atom(atom) => write!(f, "Atom: {}\n",atom),
            Expression::Identifier(ident) => write!(f, "Identifier: {}\n",ident),
        }
    }
 
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomTypes {
    Integer(i32),
    Symbol(String),
    String(String),
    Boolean(bool),
    List(Vec<AtomTypes>),
}
impl Display for AtomTypes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AtomTypes::Integer(to_display) => write!(f, "{}", to_display),
            AtomTypes::Symbol(to_display) => write!(f, "{}", to_display),
            AtomTypes::String(to_display) => write!(f, r#""{}""#, to_display),
            AtomTypes::Boolean(to_display) => write!(f, "{}", if *to_display { "#t" }else{ "#f" }),
            AtomTypes::List(to_display) => { 
                write!(f, "(").unwrap();
                to_display.iter().for_each(|x| { write!(f,"{}",x).unwrap();});
                write!(f, ")")
            } ,
        }
    }
 
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    #[test]
    fn atom_parsing() {
        use super::AtomTypes::*;

        let mut parser = Parser::init_with_string(r#"(a "b" cc 0 ((#t #f)(a b 9)))"#);

        assert_eq!(
            parser.re_atom(),
            AtomTypes::List(vec![
                Symbol("a".to_string()),
                String("b".to_string()),
                Symbol("cc".to_string()),
                Integer(0),
                List(vec![
                    List(vec![Boolean(true), Boolean(false)]),
                    List(vec![
                        Symbol("a".to_string()),
                        Symbol("b".to_string()),
                        Integer(9)
                    ])
                ])
            ])
        )
    }
    #[test]
    fn expr_parsing_1() {
        use super::AtomTypes::*;
        use super::Expression::*;

        let mut parser = Parser::init_with_string(
            r#"
        (let ([f (let ([x 'sam])
            (lambda (y z) (list x y z)))])
        (f 'i 'am))"#,
        );

        let res = parser.re_expr();
        // println!("\n\n{}\n\n",res);

        assert_eq!(
            res,
            Let(
                vec![(
                    "f".to_owned(),
                    Let(
                        vec![("x".to_owned(), Atom(Symbol("sam".to_owned())))],
                        vec![Lambda(
                            vec!["y".to_owned(), "z".to_owned()],
                            vec![LambdaCall(
                                Rc::new(Identifier("list".to_owned())),
                                vec![Identifier("x".to_owned()), Identifier("y".to_owned()),Identifier("z".to_owned())]
                            )]
                        )]
                    )
                )],
                vec![LambdaCall(Rc::new(Identifier("f".to_owned())),vec![Atom(Symbol("i".to_owned())),Atom(Symbol("am".to_owned()))])]
            )
        )
    }

    #[test]
    fn expr_parsing_2() {
        use super::AtomTypes::*;
        use super::Expression::*;
        use super::Programm;

        let mut parser = Parser::init_with_string(
            r#"
            (define add1
                (lambda (n)
                    (+ n 1)))
            (define sub1
                (lambda (n)
                    (- n 1)))
            (define o+
                (lambda (a b)
                    (cond 
                        ((zero? b) a)
                        (else (add1 (o+ a (sub1 b)))))))
            (define o-
                (lambda (a b)
                    (cond 
                        ((zero? b) a)
                        (else (sub1 (o- a (sub1 b)))))))"#,
        );

        assert_eq!(
            parser.re_program(),
            Programm::Expression(
                vec![
                    Define("add1".to_owned(), 
                        Rc::new(Lambda(vec!["n".to_owned()], 
                            vec![LambdaCall(Rc::new(Identifier("+".to_owned())), vec![Identifier("n".to_owned()), Atom(Integer(1))])]))), 
                    Define("sub1".to_owned(), 
                        Rc::new(Lambda(vec!["n".to_owned()], 
                            vec![LambdaCall(Rc::new(Identifier("-".to_owned())), vec![Identifier("n".to_owned()), Atom(Integer(1))])]))), 
                    Define("o+".to_owned(), 
                        Rc::new(Lambda(vec!["a".to_owned(), "b".to_owned()], 
                            vec![Cond(vec![
                                (LambdaCall(Rc::new(Identifier("zero?".to_owned())), vec![Identifier("b".to_owned())]), Identifier("a".to_owned())), 
                                (Atom(Boolean(true)), LambdaCall(Rc::new(Identifier("add1".to_owned())), vec![LambdaCall(Rc::new(Identifier("o+".to_string())), vec![Identifier("a".to_string()), LambdaCall(Rc::new(Identifier("sub1".to_owned())), vec![Identifier("b".to_owned())])])])
                                )]
                            )]
                        ))
                    ), 
                    Define("o-".to_owned(), 
                        Rc::new(Lambda(vec!["a".to_owned(), "b".to_owned()], 
                            vec![Cond(vec![
                                (LambdaCall(Rc::new(Identifier("zero?".to_owned())), vec![Identifier("b".to_owned())]), Identifier("a".to_owned())), 
                                (Atom(Boolean(true)), LambdaCall(Rc::new(Identifier("sub1".to_owned())), vec![LambdaCall(Rc::new(Identifier("o-".to_owned())), vec![Identifier("a".to_owned()), LambdaCall(Rc::new(Identifier("sub1".to_owned())), vec![Identifier("b".to_owned())])])]))])])))]),
        );
    }
}
