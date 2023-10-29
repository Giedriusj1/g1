mod eval;
mod lex;
mod sexp;

fn create_sexp_from_file(filename: String) -> sexp::Sexp {
    // Wrap the file contents in a progn, so that we can evaluate multiple expressions
    let mut text: String = "(progn ".to_string();

    text.push_str(
        std::fs::read_to_string(filename)
            .unwrap()
            .lines()
            .filter(|&line| !line.trim().starts_with(";;"))
            .collect::<Vec<_>>()
            .join("\n")
            .as_str(),
    );

    text.push_str(")");

    let tokens: Vec<lex::Token> = lex::extract_tokens(text);
    // println!("tokens {tokens:#?}");

    sexp::create_sexp(tokens)
}

fn main() {
    let child = std::thread::Builder::new()
        .stack_size(512 * 1024 * 1024)
        .spawn(move || {
            // time how long it takes to run the program
            let start = std::time::Instant::now();

            // read parameter passed to the program
            let args: Vec<String> = std::env::args().collect();

            let filename = if args.len() < 2 { "./test.g1".to_string() } else { args[1].clone() };

            let intrinsics_sexp = create_sexp_from_file("./intrinsics.g1".to_string());

            let sexp = create_sexp_from_file(filename);

            // Create eval state struct
            let mut eval_state = eval::EvalState::new();

            // Evaluate intrinsics
            // TODO: check results
            eval::eval_sexp(&intrinsics_sexp, &mut eval_state);

            let eval = eval::eval_sexp(&sexp, &mut eval_state);

            println!("eval to: {eval}");

            let end = std::time::Instant::now();
            println!("execution time: {:?}", end - start);
        })
        .unwrap();

    child.join().unwrap();
}
