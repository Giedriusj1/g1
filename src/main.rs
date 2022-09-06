mod eval;
mod lex;
mod sexp;

use std::fs;

fn main() {

    let text = fs::read_to_string("./test.g1")
        .unwrap()
        .lines()
        .filter(|&line| !line.trim().starts_with(";;"))
        .collect::<Vec<_>>()
        .join("\n");

    let tokens: Vec<lex::Token> = lex::extract_tokens(text);
    // println!("tokens {:#?}", tokens);

    let sexp = sexp::create_sexp(tokens);
    println!("sexp {:#?}", sexp);

    let eval = eval::eval_sexp(sexp);
    println!("eval {:#?}", eval);
}
