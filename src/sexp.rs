use crate::lex;

#[derive(Clone, Debug)]
pub(crate) enum Atom {
    Nil,         // ()
    True,        // #t
    Num(i32),    // A digit
    Sym(String), // A symbol
}

impl std::cmp::PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom::Nil, Atom::Nil) => true,
            (Atom::True, Atom::True) => true,
            (Atom::Num(d1), Atom::Num(d2)) => d1 == d2,
            (Atom::Sym(s1), Atom::Sym(s2)) => s1 == s2,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Sexp {
    Atom(Atom),
    List(Vec<Sexp>),
}

impl std::cmp::PartialEq for Sexp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Sexp::Atom(a1), Sexp::Atom(a2)) => a1 == a2,
            (Sexp::List(l1), Sexp::List(l2)) => l1 == l2,
            _ => false,
        }
    }
}

impl std::fmt::Display for Sexp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Sexp::Atom(atom) => match atom {
                Atom::Nil => write!(f, "nil")?,
                Atom::True => write!(f, "t")?,
                Atom::Num(d) => write!(f, "{}", d)?,
                Atom::Sym(s) => write!(f, "{}", s)?,
            },
            Sexp::List(l) => {
                write!(f, "(")?;

                for i in l {
                    write!(f, "{} ", i)?;
                }
                write!(f, ")")?;
            }
        }

        Ok(())
    }
}

// This expands backticks into the quotation syntax
fn expand_special_characters(tokens: &Vec<lex::Token>) -> Vec<lex::Token> {
    let mut expanded_tokens: Vec<lex::Token> = vec![];

    // enumerate over tokens
    let mut skip = 0;

    let mut apostrophe_or_backtick_vec: Vec<lex::Token> = vec![];

    for (index, token) in tokens.iter().enumerate() {
        if skip > 0 {
            skip -= 1;
            continue;
        }

        if let lex::Token::Apostrophe | lex::Token::Backtick = token {
            if let lex::Token::Apostrophe = token {
                apostrophe_or_backtick_vec.push(lex::Token::Apostrophe);
            } else {
                apostrophe_or_backtick_vec.push(lex::Token::Backtick);
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
                lex::Token::Backtick | lex::Token::Apostrophe => {
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
                    // found_apostrophes = 0;

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
            lex::Token::Digit(d) => current_sexp.push(Sexp::Atom(Atom::Num(d))),
            lex::Token::String(s) => {
                if s == "nil" {
                    current_sexp.push(Sexp::Atom(Atom::Nil));
                } else if s == "t" {
                    current_sexp.push(Sexp::Atom(Atom::True));
                } else {
                    current_sexp.push(Sexp::Atom(Atom::Sym(s)))
                }
            }
            _ => panic!("invalid syntax"),
        }
    }

    current_sexp.get(0).unwrap().clone()
}
