use crate::lex;

#[derive(Clone, Debug)]
pub(crate) enum Sexp {
    List(Vec<Sexp>),
    Nil,         // ()
    True,        // #t
    Num(i64),    // A digit
    Sym(String), // A symbol
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

pub(crate) fn create_sexp(tokens: Vec<lex::Token>) -> Sexp {
    // Expand backticks and apostrophes into the quotation syntax
    let tokens = expand_special_characters(&tokens);

    let mut sexp_stack: Vec<Vec<Sexp>> = vec![];

    let mut current_sexp: Vec<Sexp> = vec![];

    for token in tokens {
        match token {
            lex::Token::LeftParen => {
                sexp_stack.push(current_sexp);
                current_sexp = vec![];
            }
            lex::Token::RightParen => {
                let finished_sexp = current_sexp;
                current_sexp = sexp_stack.pop().unwrap();
                current_sexp.push(Sexp::List(finished_sexp));
            }
            lex::Token::Digit(d) => current_sexp.push(Sexp::Num(d)),
            lex::Token::String(s) => {
                if s == "nil" {
                    current_sexp.push(Sexp::Nil);
                } else if s == "t" {
                    current_sexp.push(Sexp::True);
                } else {
                    current_sexp.push(Sexp::Sym(s));
                }
            }
            _ => panic!("invalid syntax"),
        }
    }

    current_sexp.first().unwrap().clone()
}
