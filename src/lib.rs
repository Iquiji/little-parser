use core::panic;
use std::rc::Rc;

use scanner::Token;

mod scanner;

#[derive(Debug, Clone)]
struct Parser {
    tokens: Vec<Token>,
    token_idx: usize,
}
impl Parser {
    fn init_with_string(program: &str) -> Parser {
        Parser {
            tokens: scanner::scan_to_token_list(program),
            token_idx: 0,
        }
    }
    fn current(&self) -> Token {
        println!("Self: {:?},{:?}",self,self.current_type());
        self.tokens[self.token_idx].clone()
    }
    fn current_type(&self) -> scanner::ScannerTokenTypes {
        self.tokens[self.token_idx].ttype.clone()
    }
    fn next(&self) -> Token {
        self.tokens[self.token_idx + 1].clone()
    }
    fn next_type(&self) -> scanner::ScannerTokenTypes {
        self.tokens[self.token_idx + 1].ttype.clone()
    }
    fn advance(&mut self) {
        self.token_idx += 1;
    }
    fn re_program(&mut self) -> Programm {
        let mut result = vec![];

        while self.token_idx < self.tokens.len() {
            result.push(self.re_expr());
        }

        Programm::Expression(result)
    }
    fn re_expr(&mut self) -> Expression {
        match self.current_type() {
            scanner::ScannerTokenTypes::LeftParantheses => {
                match self.next_type() {
                    scanner::ScannerTokenTypes::Keyword(keyword) => {
                        match keyword {
                            scanner::KeywordEnum::Quote => {
                                self.advance();
                                self.advance();
                                let atom_to_quote = self.re_atom();
                                return Expression::Quote(atom_to_quote);
                            }
                            scanner::KeywordEnum::Lambda => {
                                self.advance();
                                self.advance();
                                let mut formals = vec![];
                                if self.current_type()
                                    != scanner::ScannerTokenTypes::LeftParantheses
                                {
                                    panic!("Unexpected: '{:?}'", self.current());
                                }
                                self.advance();
                                while self.current_type()
                                    != scanner::ScannerTokenTypes::RightParantheses
                                {
                                    if self.current_type() != scanner::ScannerTokenTypes::Identifier
                                    {
                                        panic!(
                                            "Unexpected: '{:?}',Expected Identifier in Lambda",
                                            self.current()
                                        );
                                    } else {
                                        formals.push(self.current().lexeme)
                                    }
                                    self.advance();
                                }
                                self.advance();
                                let mut body = vec![];
                                while self.current_type()
                                    != scanner::ScannerTokenTypes::RightParantheses
                                {
                                    body.push(self.re_expr())
                                }
                                self.advance();
                                return Expression::Lambda(formals, body);
                            }
                            scanner::KeywordEnum::Let => {
                                // (let ((x expr)(y expr)...) body)
                                self.advance();
                                self.advance();
                                let mut bindings = vec![];
                                if self.current_type()
                                    != scanner::ScannerTokenTypes::LeftParantheses
                                {
                                    panic!("Unexpected: '{:?}'", self.current());
                                }
                                self.advance();
                                self.advance();
                                while self.current_type()
                                    != scanner::ScannerTokenTypes::RightParantheses
                                {
                                    println!("let bindings: {:?}",bindings);
                                    if self.current_type() != scanner::ScannerTokenTypes::Identifier
                                    {
                                        panic!(
                                            "Unexpected: '{:?}',Expected Identifier in Let",
                                            self.current()
                                        );
                                    } else {
                                        bindings.push((self.current().lexeme, {
                                            self.advance();
                                            self.re_expr()
                                        }))
                                    }
                                    self.advance();
                                }
                                
                                self.advance();
                                let mut body = vec![];
                                while self.current_type()
                                    != scanner::ScannerTokenTypes::RightParantheses
                                {
                                    body.push(self.re_expr());
                                    self.advance();
                                }
                                return Expression::Let(bindings, body);
                            }
                            scanner::KeywordEnum::Cond => {
                                // (cond (case1 res1)(case2 res2)...)
                                self.advance();
                                self.advance();
                                let mut case_pairs = vec![];
                                while self.current_type()
                                    != scanner::ScannerTokenTypes::RightParantheses
                                {
                                    if self.next_type()
                                        == scanner::ScannerTokenTypes::Keyword(
                                            scanner::KeywordEnum::Else,
                                        )
                                    {
                                        let first = Expression::Atom(AtomTypes::Boolean(true));
                                        let second = self.re_expr();
                                        case_pairs.push((first, second));
                                    } else {
                                        let first = self.re_expr();
                                        let second = self.re_expr();
                                        case_pairs.push((first, second));
                                    }
                                    self.advance();
                                    self.advance();
                                }
                                return Expression::Cond(case_pairs);
                            }
                            scanner::KeywordEnum::Define => {
                                self.advance();
                                self.advance();
                                if self.current_type() != scanner::ScannerTokenTypes::Identifier {
                                    panic!("Unexpected: '{:?}' Expected Identifier for first in define",self.current());
                                }
                                return Expression::Define(self.current().lexeme, {
                                    self.advance();
                                    Rc::new(self.re_expr())
                                });
                            }
                            scanner::KeywordEnum::Else => {
                                todo!();
                            }
                        }
                    }
                    scanner::ScannerTokenTypes::Identifier => {
                        let call_id = self.current().lexeme;
                        self.advance();
                        self.advance();
                        let mut body = vec![];
                        while self.current_type() != scanner::ScannerTokenTypes::RightParantheses {
                            body.push(self.re_expr());
                        }
                        self.advance();
                        return Expression::LambdaCall(call_id, body);
                    }
                    _ => {
                        panic!("Unexpected: '{:?}' after LeftParantheses", self.next());
                    }
                }
            }
            scanner::ScannerTokenTypes::RightParantheses => todo!(),
            scanner::ScannerTokenTypes::Keyword(_) => todo!(),
            scanner::ScannerTokenTypes::Identifier => {
                let res = Expression::Identifier(self.current().lexeme);
                self.advance();
                return res;
            },
            scanner::ScannerTokenTypes::String => {
                let res = AtomTypes::String(self.current().lexeme);
                self.advance();
                return Expression::Atom(res);
            }
            scanner::ScannerTokenTypes::Number => {
                let res =
                    AtomTypes::Integer(i32::from_str_radix(&self.current().lexeme, 10).unwrap());
                self.advance();
                return Expression::Atom(res);
            }
            scanner::ScannerTokenTypes::Boolean => {
                let res = AtomTypes::Boolean(match self.current().lexeme.as_str() {
                    "#f" => false,
                    "#t" => true,
                    _ => unreachable!(),
                });
                self.advance();
                return Expression::Atom(res);
            }
            scanner::ScannerTokenTypes::EOF => todo!(),
        }
    }
    fn re_atom(&mut self) -> AtomTypes {
        match self.current_type() {
            scanner::ScannerTokenTypes::LeftParantheses => {
                // (...)
                let mut atom_list = vec![];
                while self.next_type() != scanner::ScannerTokenTypes::RightParantheses {
                    self.advance();
                    atom_list.push(self.re_atom());
                }
                self.advance();
                AtomTypes::List(atom_list)
            }
            scanner::ScannerTokenTypes::Identifier => AtomTypes::Symbol(self.current().lexeme),
            scanner::ScannerTokenTypes::String => AtomTypes::String(self.current().lexeme),
            scanner::ScannerTokenTypes::Number => {
                AtomTypes::Integer(i32::from_str_radix(&self.current().lexeme, 10).unwrap())
            }
            scanner::ScannerTokenTypes::Boolean => {
                AtomTypes::Boolean(match self.current().lexeme.as_str() {
                    "#f" => false,
                    "#t" => true,
                    _ => unreachable!(),
                })
            }
            err => panic!("Unexpected '{:?}'", err),
        }
    }
}

enum Programm {
    Expression(Vec<Expression>),
}

type Identifier = String;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expression {
    Quote(AtomTypes),
    Lambda(Vec<Identifier>, Vec<Expression>),
    Cond(Vec<(Expression, Expression)>),
    Define(Identifier, Rc<Expression>),
    Let(Vec<(Identifier, Expression)>, Vec<Expression>),
    LambdaCall(Identifier, Vec<Expression>),
    Atom(AtomTypes),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AtomTypes {
    Integer(i32),
    Symbol(String),
    String(String),
    Boolean(bool),
    List(Vec<AtomTypes>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Primitive {
    Quote,
    Lambda,
    Let,
    Cond,
    Define,
    Else,
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

        let mut parser = Parser::init_with_string(
            r#"
        (let ([f (let ([x 'sam])
            (lambda (y z) (list x y z)))])"#,
        );

        assert_eq!(parser.re_expr(), Expression::Let(vec![], vec![]))
    }
}
