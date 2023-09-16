use crate::lex;

#[derive(Clone, Debug)]
pub(crate) enum Atom {
    Nil,        // ()
    True,       // #t
    Digit(i32), // A digit
    Symbol(String), // A symbol
                // QuotedSymbol(String), // A quoted string
                // QuotedString(String), // A quoted string
}

#[derive(Clone, Debug)]
pub(crate) enum Sexp {
    Atom(Atom),
    List(Vec<Sexp>),
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
