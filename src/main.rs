mod eval;
mod lex;
mod sexp;

fn main() {
    // time how long it takes to run the program
    let start = std::time::Instant::now();

    let child = std::thread::Builder::new()
        .stack_size(512 * 1024 * 1024)
        .spawn(move || {
            // read parameter passed to the program
            let args: Vec<String> = std::env::args().collect();

            let filename: String = if args.len() < 2 { "./test.g1".to_string() } else { args[1].clone() };

            let text = std::fs::read_to_string(filename)
                .unwrap()
                .lines()
                .filter(|&line| !line.trim().starts_with(";;"))
                .collect::<Vec<_>>()
                .join("\n");

            let tokens: Vec<lex::Token> = lex::extract_tokens(text);
            println!("tokens {tokens:#?}");

            let sexp = sexp::create_sexp(tokens);
            println!("sexp {sexp:#?}");

            let eval = eval::eval_sexp(sexp);
            println!("eval {eval:#?}");
        })
        .unwrap();

    child.join().unwrap();

    let end = std::time::Instant::now();
    println!("execution time: {:?}", end - start);
}
