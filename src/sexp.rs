use crate::lex;

use std::rc::Rc;

#[derive(Clone, Debug)]
pub(crate) enum Sexp {
    List(Vec<Rc<Sexp>>),
    Nil,         // ()
    True,        // #t
    Num(i64),    // A digit
    Sym(String), // A symbol
    Instrinsics(IntrinsicInstruction),
}

#[derive(Clone, Debug)]
pub(crate) enum IntrinsicInstruction {
    Let,
    Cdr,
    Cons,
    List,
    Car,

    // Quote,
    // Backtick,
    // Comma,
    Eval,
    Message,
    // Equal,
    // And,
    Progn,
    // Setq, ????
    Defmacro,
    If,
    While,
    Or,
    SplitSymbol,
    Integerp,
}

impl PartialEq for Sexp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Sexp::List(lhs), Sexp::List(rhs)) => lhs == rhs,
            (Sexp::Nil, Sexp::Nil) => true,
            (Sexp::True, Sexp::True) => true,
            (Sexp::Num(lhs), Sexp::Num(rhs)) => lhs == rhs,
            (Sexp::Sym(lhs), Sexp::Sym(rhs)) => lhs == rhs,
            (_, _) => false,
        }
    }
}

impl std::fmt::Display for IntrinsicInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            IntrinsicInstruction::Let => write!(f, "let")?,
            IntrinsicInstruction::Cdr => write!(f, "cdr")?,
            IntrinsicInstruction::Cons => write!(f, "cons")?,
            IntrinsicInstruction::List => write!(f, "list")?,
            IntrinsicInstruction::Car => write!(f, "car")?,
            // IntrinsicInstruction::Quote => write!(f, "quote")?, ????????
            // IntrinsicInstruction::Backtick => write!(f, "backtick")?, ?????
            // IntrinsicInstruction::Comma => write!(f, "comma")?,
            IntrinsicInstruction::Eval => write!(f, "eval")?,
            IntrinsicInstruction::Message => write!(f, "message")?,
            // IntrinsicInstruction::Equal => write!(f, "equal")?,
            // IntrinsicInstruction::And => write!(f, "and")?,
            IntrinsicInstruction::Progn => write!(f, "progn")?,
            // IntrinsicInstruction::Setq => write!(f, "setq")?,
            IntrinsicInstruction::Defmacro => write!(f, "defmacro")?,
            IntrinsicInstruction::If => write!(f, "if")?,
            IntrinsicInstruction::While => write!(f, "while")?,
            IntrinsicInstruction::Or => write!(f, "or")?,
            IntrinsicInstruction::SplitSymbol => write!(f, "split-symbol")?,
            IntrinsicInstruction::Integerp => write!(f, "integerp")?,
        }

        Ok(())
    }
}

use std::str::FromStr;

impl FromStr for IntrinsicInstruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "let" => Ok(IntrinsicInstruction::Let),
            "cdr" => Ok(IntrinsicInstruction::Cdr),
            "cons" => Ok(IntrinsicInstruction::Cons),
            "list" => Ok(IntrinsicInstruction::List),
            "car" => Ok(IntrinsicInstruction::Car),
            // "quote" => Ok(IntrinsicInstruction::Quote),
            // "backtick" => Ok(IntrinsicInstruction::Backtick),
            // "comma" => Ok(IntrinsicInstruction::Comma),
            "eval" => Ok(IntrinsicInstruction::Eval),
            "message" => Ok(IntrinsicInstruction::Message),
            // "equal" => Ok(IntrinsicInstruction::Equal),
            // "and" => Ok(IntrinsicInstruction::And),
            "progn" => Ok(IntrinsicInstruction::Progn),
            // "setq" => Ok(IntrinsicInstruction::Setq),
            "defmacro" => Ok(IntrinsicInstruction::Defmacro),
            "if" => Ok(IntrinsicInstruction::If),
            "while" => Ok(IntrinsicInstruction::While),
            "or" => Ok(IntrinsicInstruction::Or),
            "split-symbol" => Ok(IntrinsicInstruction::SplitSymbol),
            "integerp" => Ok(IntrinsicInstruction::Integerp),
            _ => Err(()),
        }
    }
}

impl std::fmt::Display for Sexp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Sexp::List(l) => {
                write!(f, "(")?;

                for (index, i) in l.iter().enumerate() {
                    if index == l.len() - 1 {
                        write!(f, "{}", i)?;
                    } else {
                        write!(f, "{} ", i)?;
                    }
                }
                write!(f, ")")?;
            }
            Sexp::Nil => write!(f, "nil")?,
            Sexp::True => write!(f, "t")?,
            Sexp::Num(d) => write!(f, "{}", d)?,
            Sexp::Sym(s) => write!(f, "{}", s)?,
            Self::Instrinsics(intrinsic) => write!(f, "{}", intrinsic)?, // Sexp::Let => write!(f, "let")?,
        }

        Ok(())
    }
}

// This expands backticks into the quotation syntax
fn expand_special_characters(tokens: &[lex::Token]) -> Vec<lex::Token> {
    let mut expanded_tokens: Vec<lex::Token> = vec![];
    let mut apostrophe_or_backtick_vec: Vec<lex::Token> = vec![];
    let mut skip = 0;

    for (index, token) in tokens.iter().enumerate() {
        if skip > 0 {
            skip -= 1;
            continue;
        }

        if let lex::Token::Apostrophe | lex::Token::Backtick | lex::Token::Comma = token {
            if let lex::Token::Apostrophe = token {
                apostrophe_or_backtick_vec.push(lex::Token::Apostrophe);
            } else if let lex::Token::Backtick = token {
                apostrophe_or_backtick_vec.push(lex::Token::Backtick);
            } else if let lex::Token::Comma = token {
                apostrophe_or_backtick_vec.push(lex::Token::Comma);
            }

            // peek into the next token
            let next_token = tokens.get(index + 1).unwrap();
            match next_token {
                // Look up until next right parends
                lex::Token::LeftParen => {
                    let mut left_encountered = 1;
                    let mut right_encountered = 0;
                    loop {
                        skip += 1;

                        if let lex::Token::LeftParen = tokens.get(index + skip + 1).unwrap() {
                            left_encountered += 1;
                        } else if let lex::Token::RightParen = tokens.get(index + skip + 1).unwrap() {
                            right_encountered += 1;

                            if left_encountered != right_encountered {
                                continue;
                            }

                            let slice = &tokens[index + 1..index + skip + 2].to_vec().clone();

                            for i in apostrophe_or_backtick_vec.iter() {
                                match i {
                                    lex::Token::Apostrophe => {
                                        expanded_tokens.push(lex::Token::LeftParen);
                                        expanded_tokens.push(lex::Token::String("quote".to_string()));
                                    }
                                    lex::Token::Backtick => {
                                        expanded_tokens.push(lex::Token::LeftParen);
                                        expanded_tokens.push(lex::Token::String("backtick".to_string()));
                                    }
                                    lex::Token::Comma => {
                                        expanded_tokens.push(lex::Token::LeftParen);
                                        expanded_tokens.push(lex::Token::String("comma".to_string()));
                                    }
                                    _ => {
                                        panic!("invalid syntax")
                                    }
                                }
                            }

                            let mut expanded = expand_special_characters(slice);

                            expanded_tokens.append(&mut expanded);

                            for _ in apostrophe_or_backtick_vec.iter() {
                                expanded_tokens.push(lex::Token::RightParen);
                            }

                            apostrophe_or_backtick_vec = vec![];
                            skip += 1;
                            break;
                        };
                    }
                }
                lex::Token::Comma | lex::Token::Backtick | lex::Token::Apostrophe => {
                    continue;
                }
                // If it's a symbol or a digit, then we need to wrap it in a quote
                lex::Token::String(_) | lex::Token::Digit(_) => {
                    for i in apostrophe_or_backtick_vec.iter() {
                        match i {
                            lex::Token::Apostrophe => {
                                expanded_tokens.push(lex::Token::LeftParen);
                                expanded_tokens.push(lex::Token::String("quote".to_string()));
                            }
                            lex::Token::Backtick => {
                                expanded_tokens.push(lex::Token::LeftParen);
                                expanded_tokens.push(lex::Token::String("backtick".to_string()));
                            }
                            lex::Token::Comma => {
                                expanded_tokens.push(lex::Token::LeftParen);
                                expanded_tokens.push(lex::Token::String("comma".to_string()));
                            }
                            _ => {
                                panic!("invalid syntax")
                            }
                        }
                    }

                    expanded_tokens.push(next_token.clone());

                    for _ in apostrophe_or_backtick_vec.iter() {
                        expanded_tokens.push(lex::Token::RightParen);
                    }
                    skip = 1;

                    apostrophe_or_backtick_vec = vec![];

                    continue;
                }
                lex::Token::RightParen => panic!("invalid syntax"),
            }

            continue;
        }

        expanded_tokens.push(token.clone());
    }

    expanded_tokens
}

pub(crate) fn create_sexp(tokens: Vec<lex::Token>) -> Rc<Sexp> {
    // Expand backticks and apostrophes into the quotation syntax
    let tokens = expand_special_characters(&tokens);

    let mut sexp_stack: Vec<Vec<Rc<Sexp>>> = vec![];

    let mut current_sexp: Vec<Rc<Sexp>> = vec![];

    for token in tokens {
        match token {
            lex::Token::LeftParen => {
                sexp_stack.push(current_sexp);
                current_sexp = vec![];
            }
            lex::Token::RightParen => {
                let finished_sexp = current_sexp;
                current_sexp = sexp_stack.pop().unwrap();
                current_sexp.push(Rc::new(Sexp::List(finished_sexp)));
            }
            lex::Token::Digit(d) => current_sexp.push(Rc::new(Sexp::Num(d))),
            lex::Token::String(s) => {
                if let Ok(intrinsic) = IntrinsicInstruction::from_str(&s) {
                    current_sexp.push(Rc::new(Sexp::Instrinsics(intrinsic)));
                } else {
                    // if s == "let" {
                    //     current_sexp.push(Rc::new(Sexp::Instrinsics(IntrinsicInstruction::Let)));
                    // } else if s == "cons" {
                    //     current_sexp.push(Rc::new(Sexp::Instrinsics(IntrinsicInstruction::Cons)));
                    // } else if s == "cdr" {
                    //     current_sexp.push(Rc::new(Sexp::Instrinsics(IntrinsicInstruction::Cdr)));
                    // } else if s == "list" {
                    //     current_sexp.push(Rc::new(Sexp::Instrinsics(IntrinsicInstruction::List)));
                    // } else if s == "car" {
                    //     current_sexp.push(Rc::new(Sexp::Instrinsics(IntrinsicInstruction::Car)));
                    // } else
                    if s == "nil" {
                        current_sexp.push(Rc::new(Sexp::Nil));
                    } else if s == "t" {
                        current_sexp.push(Rc::new(Sexp::True));
                    } else {
                        current_sexp.push(Rc::new(Sexp::Sym(s)));
                    }
                }
            }
            _ => panic!("invalid syntax"),
        }
    }

    current_sexp.first().unwrap().clone()
}
