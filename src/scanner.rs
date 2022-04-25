use core::iter::Enumerate;
use core::iter::Peekable;
use core::str::Chars;
use std::collections::HashMap;

#[cfg(test)]
mod test {

    use super::KeywordEnum::*;
    use super::ScannerTokenTypes::*;
    use super::*;

    #[test]
    fn test_1() {
        assert!('l'.is_ascii_alphanumeric());
        println!("Hello, world!");
        println!("Parse Result: {:#?}",scan_to_token_list("bibendum morbi non quam (nec dui luctus (a b '(arbitrary parenthesis 0102) c)) rutrum nulla"));
    }
    #[test]
    fn scanner_test_1() {
        let test_data = r#"
        (define add1
            (lambda (n)
                (+ n 1)))"#;
        let compare_to = vec![
            Token {
                ttype: LeftParantheses,
                lexeme: "(".to_string(),
                line: 2,
            },
            Token {
                ttype: Keyword(Define),
                lexeme: "define".to_string(),
                line: 2,
            },
            Token {
                ttype: Identifier,
                lexeme: "add1".to_string(),
                line: 3,
            },
            Token {
                ttype: LeftParantheses,
                lexeme: "(".to_string(),
                line: 3,
            },
            Token {
                ttype: Keyword(Lambda),
                lexeme: "lambda".to_string(),
                line: 3,
            },
            Token {
                ttype: LeftParantheses,
                lexeme: "(".to_string(),
                line: 3,
            },
            Token {
                ttype: Identifier,
                lexeme: "n".to_string(),
                line: 3,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 3,
            },
            Token {
                ttype: LeftParantheses,
                lexeme: "(".to_string(),
                line: 4,
            },
            Token {
                ttype: Identifier,
                lexeme: "+".to_string(),
                line: 4,
            },
            Token {
                ttype: Identifier,
                lexeme: "n".to_string(),
                line: 4,
            },
            Token {
                ttype: Number,
                lexeme: "1".to_string(),
                line: 4,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 4,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 4,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 4,
            },
            Token {
                ttype: EOF,
                lexeme: "".to_string(),
                line: 4,
            },
        ];

        assert_eq!(scan_to_token_list(test_data), compare_to);
    }
    #[test]
    fn scanner_test_2() {
        let test_data = r#"(cond let lambda else 'a b "c   ddd a" 0 9))))) )"#;
        let compare_to = vec![
            Token {
                ttype: LeftParantheses,
                lexeme: "(".to_string(),
                line: 1,
            },
            Token {
                ttype: Keyword(Cond),
                lexeme: "cond".to_string(),
                line: 1,
            },
            Token {
                ttype: Keyword(Let),
                lexeme: "let".to_string(),
                line: 1,
            },
            Token {
                ttype: Keyword(Lambda),
                lexeme: "lambda".to_string(),
                line: 1,
            },
            Token {
                ttype: Keyword(Else),
                lexeme: "else".to_string(),
                line: 1,
            },
            Token {
                ttype: Keyword(Quote),
                lexeme: "'".to_string(),
                line: 1,
            },
            Token {
                ttype: Identifier,
                lexeme: "a".to_string(),
                line: 1,
            },
            Token {
                ttype: Identifier,
                lexeme: "b".to_string(),
                line: 1,
            },
            Token {
                ttype: String,
                lexeme: "c   ddd a".to_string(),
                line: 1,
            },
            Token {
                ttype: Number,
                lexeme: "0".to_string(),
                line: 1,
            },
            Token {
                ttype: Number,
                lexeme: "9".to_string(),
                line: 1,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 1,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 1,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 1,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 1,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 1,
            },
            Token {
                ttype: RightParantheses,
                lexeme: ")".to_string(),
                line: 1,
            },
            Token {
                ttype: EOF,
                lexeme: "".to_string(),
                line: 1,
            },
        ];

        assert_eq!(scan_to_token_list(test_data), compare_to);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScannerTokenTypes {
    LeftParantheses,
    RightParantheses,
    Keyword(KeywordEnum),
    Identifier,
    String,
    Number,
    Boolean,
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeywordEnum {
    Quote,
    Lambda,
    Let,
    Cond,
    Define,
    Else,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub ttype: ScannerTokenTypes,
    pub lexeme: String,
    pub line: u32,
}

pub fn scan_to_token_list(in_str: &str) -> Vec<Token> {
    let mut string_iter = in_str.chars().enumerate().peekable();
    let mut token_list = vec![];
    let mut line = 1;
    while string_iter.peek().is_some() {
        // println!("debug: {:?}",token_list);
        while string_iter.peek().unwrap().1.is_ascii_whitespace() {
            if string_iter.next().unwrap().1 == '\n' {
                line += 1;
            };
        }
        token_list.push(scan_token(&mut string_iter, &mut line));
    }

    token_list.push(Token {
        ttype: ScannerTokenTypes::EOF,
        lexeme: "".to_string(),
        line,
    });
    token_list
}
fn scan_token(iter: &mut Peekable<Enumerate<Chars<'_>>>, line: &mut u32) -> Token {
    let mut keyword_map = HashMap::new();
    keyword_map.insert("quote", KeywordEnum::Quote);
    keyword_map.insert("lambda", KeywordEnum::Lambda);
    keyword_map.insert("let", KeywordEnum::Let);
    keyword_map.insert("cond", KeywordEnum::Cond);
    keyword_map.insert("else", KeywordEnum::Else);
    keyword_map.insert("define", KeywordEnum::Define);

    if let Some(current_char) = iter.next() {
        match current_char.1 {
            '(' | '[' | '{' => {
                return Token {
                    ttype: ScannerTokenTypes::LeftParantheses,
                    lexeme: current_char.1.to_string(),
                    line: line.clone(),
                };
            }
            ')' | ']' | '}' => {
                return Token {
                    ttype: ScannerTokenTypes::RightParantheses,
                    lexeme: current_char.1.to_string(),
                    line: line.clone(),
                };
            }
            '\'' => {
                return Token {
                    ttype: ScannerTokenTypes::Keyword(KeywordEnum::Quote),
                    lexeme: current_char.1.to_string(),
                    line: line.clone(),
                };
            }
            '#' => {
                if iter.peek().unwrap().1 == 'f' || iter.peek().unwrap().1 == 't' {
                    return Token {
                        ttype: ScannerTokenTypes::Boolean,
                        lexeme: current_char.1.to_string() + &iter.next().unwrap().1.to_string(),
                        line: line.clone(),
                    };
                } else {
                    panic!("Unexpected: {:?}line {:?}", iter.peek(), line);
                }
            }
            '"' => {
                let mut collector = "".to_string();
                while iter.peek().is_some() {
                    let current = iter.next().unwrap().1;
                    if current == '"' {
                        return Token {
                            ttype: ScannerTokenTypes::String,
                            lexeme: collector,
                            line: line.clone(),
                        };
                    }
                    if current == '\n' {
                        *line += 1;
                    }
                    collector.push(current);
                }
            }
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let mut collector = current_char.1.to_string();
                while iter.peek().is_some() {
                    if !iter.peek().unwrap().1.is_ascii_digit() {
                        return Token {
                            ttype: ScannerTokenTypes::Number,
                            lexeme: collector,
                            line: line.clone(),
                        };
                    }
                    let current = iter.next().unwrap().1;
                    collector.push(current);
                }
            }
            '-' => {
                return Token {
                    ttype: ScannerTokenTypes::Identifier,
                    lexeme: "-".to_string(),
                    line: line.clone(),
                };
            }
            '+' => {
                return Token {
                    ttype: ScannerTokenTypes::Identifier,
                    lexeme: "+".to_string(),
                    line: line.clone(),
                };
            }
            _ => {
                let mut accum = "".to_string();
                if current_char.1.is_ascii_alphabetic()
                    || [
                        '!', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^',
                    ]
                    .contains(&current_char.1)
                {
                    accum.push(current_char.1);

                    while iter.peek().is_some() {
                        if [')', ']', '}'].contains(&iter.peek().unwrap().1) {
                            return Token {
                                ttype: ScannerTokenTypes::Identifier,
                                lexeme: accum,
                                line: line.clone(),
                            };
                        }
                        if iter.peek().unwrap().1.is_ascii_alphanumeric()
                            || [
                                '!', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^',
                                '.', '+', '-',
                            ]
                            .contains(&iter.peek().unwrap().1)
                        {
                            accum.push(iter.next().unwrap().1);
                        } else if iter.peek().unwrap().1.is_ascii_whitespace() {
                            if iter.next().unwrap().1 == '\n' {
                                *line += 1;
                            }
                            if let Some(keyword) = keyword_map.get(&accum as &str) {
                                return Token {
                                    ttype: ScannerTokenTypes::Keyword(keyword.clone()),
                                    lexeme: accum,
                                    line: line.clone(),
                                };
                            }
                            return Token {
                                ttype: ScannerTokenTypes::Identifier,
                                lexeme: accum,
                                line: line.clone(),
                            };
                        } else {
                            panic!("Unexpected: {:?},line {:?}", iter.peek().unwrap().1, line);
                        }
                    }
                    return Token {
                        ttype: ScannerTokenTypes::Identifier,
                        lexeme: accum,
                        line: line.clone(),
                    };
                } else {
                    panic!(
                        "Unexpected: {:?} on {:?},line {:?}",
                        current_char.1, current_char.0, line
                    );
                }
            }
        }
    }

    unreachable!();
}
