mod eval;
mod lex;
mod sexp;

fn main() {
    let text = String::from(
        "
(progn

(setq x 10)
(setq y 10)

  (defn addthree (a b)
    (+ (+ a b ) c))

(defn inc (a)
  (+ a 1))


(inc 11)

y
a


(if nil
 333 2)


  )


",
    );

    let tokens: Vec<lex::Token> = lex::extract_tokens(text);
    // println!("tokens {:#?}", tokens);

    let sexp = sexp::create_sexp(tokens);
    // println!("sexp {:#?}", sexp);

    let eval = eval::eval_sexp(sexp);
    println!("eval {:#?}", eval);
}
