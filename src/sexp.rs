use crate::lex;

#[derive(Clone, Debug)]
pub(crate) enum Atom {
    Nil,            // ()
    True,           // #t
    Digit(i32),     // A digit
    Symbol(String), // A symbol
}

impl std::cmp::PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom::Nil, Atom::Nil) => true,
            (Atom::True, Atom::True) => true,
            (Atom::Digit(d1), Atom::Digit(d2)) => d1 == d2,
            (Atom::Symbol(s1), Atom::Symbol(s2)) => s1 == s2,
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
                Atom::Digit(d) => write!(f, "{}", d)?,
                Atom::Symbol(s) => write!(f, "{}", s)?,
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

pub(crate) fn create_sexp(tokens: Vec<lex::Token>) -> Sexp {
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
            lex::Token::Digit(d) => current_sexp.push(Sexp::Atom(Atom::Digit(d))),
            lex::Token::String(s) => current_sexp.push(Sexp::Atom(Atom::Symbol(s))),
        }
    }

    current_sexp.get(0).unwrap().clone()
}
