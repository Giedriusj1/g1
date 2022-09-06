use crate::lex;

#[derive(Clone, Debug)]
pub(crate) enum Atom {
    Nil,
    True,
    Digit(i32),
    String(String)
}

#[derive(Clone, Debug)]
pub(crate) enum Sexp {
    Atom(Atom),
    List(Vec<Sexp>),
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
	    lex::Token::String(s) => current_sexp.push(Sexp::Atom(Atom::String(s)))
        }
    }

    current_sexp.get(0).unwrap().clone()
}
