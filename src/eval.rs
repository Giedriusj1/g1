use crate::sexp::Sexp;

use std::collections::HashMap;

#[derive(Clone)]
pub(crate) struct EvalState {
    macro_map: HashMap<String, Sexp>,
    global_map: HashMap<String, Sexp>,
    fn_map: Vec<HashMap<String, Sexp>>,
}

impl EvalState {
    pub(crate) fn new() -> EvalState {
        EvalState { macro_map: HashMap::new(), global_map: HashMap::new(), fn_map: vec![] }
    }
}

fn eval_sexp_to_num(sexp: &Sexp, state: &mut EvalState) -> i64 {
    let res = eval_sexp(sexp, state);

    if let Sexp::Num(n) = res {
        n
    } else {
        panic!("expected a number, but found: {res}")
    }
}

fn get_from_fn_map_or_global(var_name: &String, state: &EvalState) -> Option<Sexp> {
    // TODO: this is actually quite inefficient, because if we are in a deep nested function,
    // and we want to access a global var, we have to iterate through all the fn_maps first.

    // Check the functions map first (that way we default to local variables first, and allow for
    // variable shadowing)
    for var in state.fn_map.iter().rev() {
        if let Some(sexp) = var.get(var_name) {
            return Some(sexp.to_owned());
        }
    }

    // Check the global map last.
    if let Some(sexp) = state.global_map.get(var_name) {
        return Some(sexp.to_owned());
    }

    None
}

fn eval_commas_within_backtick(sexp: &Sexp, state: &mut EvalState) -> Sexp {
    match sexp {
        Sexp::List(l) => Sexp::List(
            l.iter()
                .map(|sexp| {
                    if let Sexp::List(l) = sexp {
                        if let Some(Sexp::Sym(s)) = l.first() {
                            if s == "comma" {
                                eval_sexp(l.get(1).unwrap(), state)
                            } else {
                                eval_commas_within_backtick(&Sexp::List(l.clone()), state)
                            }
                        } else {
                            eval_commas_within_backtick(&Sexp::List(l.clone()), state)
                        }
                    } else {
                        sexp.clone()
                    }
                })
                .collect::<Vec<Sexp>>(),
        ),
        _ => sexp.clone(),
    }
}

fn execute_function(fnbody: Vec<Sexp>, fncall: &[Sexp], state: &mut EvalState) -> Sexp {
    // ourfn is the function body. fncall is a list containing the function name, and the arguments to the function.
    // for example: ((a b) (+ a b))

    // fncall is a list containing the function name, and the arguments to the function
    // (add 2 3)

    let fnparams = match fnbody.first().unwrap() {
        Sexp::List(l) => l,
        _ => panic!("function params should be a list"),
    };

    let mut fn_param_map: HashMap<String, Sexp> = HashMap::new();
    // Map variable names to values passed into the function
    for (index, var) in fnparams.iter().enumerate() {
        let name = if let Sexp::Sym(s) = var { s } else { panic!("function param name should be a symbol") };

        fn_param_map.insert(name.to_owned(), eval_sexp(fncall.get(index + 1).unwrap(), state));
    }

    state.fn_map.push(fn_param_map);

    let ret = eval_sexp(fnbody.get(1).unwrap(), state);

    state.fn_map.pop();

    ret
}

pub(crate) fn eval_sexp(sexp: &Sexp, state: &mut EvalState) -> Sexp {
    match sexp {
        Sexp::Sym(s) => match get_from_fn_map_or_global(s, state) {
            Some(var) => var,
            None => panic!("symbol {s} is not defined"),
        },
        Sexp::List(l) => {
            match l.first() {
                None => Sexp::Nil, // empty list evaluates to nil
                Some(first_elem_sexp) => {
                    match first_elem_sexp {
                        Sexp::Nil => Sexp::Nil,
                        Sexp::True => Sexp::True,
                        Sexp::Num(_) => panic!("don't know how to evaluate a list starting with a number"),
                        Sexp::Sym(s) => {
                            match s.as_str() {
                                "let" => {
                                    // Let statement should bind variables for the duration of the let statement.
                                    let let_param_list = l.get(1).unwrap();

                                    match let_param_list {
                                        Sexp::List(l) => {
                                            let mut current_function_param_map: HashMap<String, Sexp> = HashMap::new();

                                            for var in l {
                                                match var {
                                                    Sexp::List(l) => {
                                                        match l.first().unwrap() {
                                                            Sexp::Sym(let_var_name) => {
                                                                let var = l.get(1).unwrap().clone();

                                                                let var_eval = eval_sexp(&var, state);

                                                                current_function_param_map
                                                                    .insert(let_var_name.to_owned(), var_eval);
                                                            }
                                                            _ => panic!("let is missing variable name"),
                                                        };
                                                    }
                                                    _ => {
                                                        panic!("let expect as list, but found")
                                                    }
                                                }
                                            }

                                            state.fn_map.push(current_function_param_map);
                                        }
                                        _ => {
                                            panic!(
                                                "let expects a param list as a first argument, but found {}",
                                                let_param_list
                                            );
                                        }
                                    }

                                    // Skip the actual "let", and the subsequent paramlist,
                                    // then eval all remaining and return last
                                    let statements = l.iter().enumerate().skip(2);

                                    if statements.len() > 0 {
                                        for (pos, statement) in statements {
                                            if pos + 1 == l.len() {
                                                let r = eval_sexp(statement, state);

                                                state.fn_map.pop();

                                                return r;
                                            } else {
                                                eval_sexp(statement, state);
                                            }
                                        }
                                    } else {
                                        state.fn_map.pop();
                                        return Sexp::Nil;
                                    }
                                }
                                "cdr" => {
                                    assert_eq!(l.len(), 2);
                                    let cdr = eval_sexp(l.get(1).unwrap(), state);
                                    return match cdr {
                                        Sexp::List(mut l) => {
                                            l.remove(0);
                                            Sexp::List(l)
                                        }
                                        _ => panic!("cdr needs a list, but found {}", cdr),
                                    };
                                }
                                "cons" => {
                                    assert_eq!(l.len(), 3);
                                    let first = eval_sexp(l.get(1).unwrap(), state);

                                    let second = eval_sexp(l.get(2).unwrap(), state);

                                    match second {
                                        Sexp::List(mut l) => {
                                            l.insert(0, first);
                                            return Sexp::List(l);
                                        }
                                        _ => return Sexp::List(vec![first, second]),
                                    }
                                }
                                "list" => {
                                    if l.len() == 1 {
                                        return Sexp::Nil;
                                    } else {
                                        return Sexp::List(
                                            l.iter().skip(1).map(|sexp| eval_sexp(sexp, state)).collect(),
                                        );
                                    }
                                }
                                "car" => match eval_sexp(l.get(1).unwrap(), state) {
                                    Sexp::List(l) => return l.first().unwrap().clone(),
                                    _ => panic!("car needs a list"),
                                },
                                "quote" => {
                                    return l.get(1).unwrap().clone();
                                }
                                "backtick" => {
                                    return eval_commas_within_backtick(l.get(1).unwrap(), state);
                                }
                                "comma" => {
                                    panic!("comma should only be used within backtick");
                                }
                                "eval" => {
                                    return eval_sexp(&eval_sexp(l.get(1).unwrap(), state), state);
                                }
                                "message" => {
                                    println!("{}", eval_sexp(l.get(1).unwrap(), state));
                                    return Sexp::True;
                                }
                                "=" => {
                                    if eval_sexp_to_num(l.get(1).unwrap(), state)
                                        == eval_sexp_to_num(l.get(2).unwrap(), state)
                                    {
                                        return Sexp::True;
                                    } else {
                                        return Sexp::Nil;
                                    }
                                }
                                "equal" => {
                                    if eval_sexp(l.get(1).unwrap(), state) == eval_sexp(l.get(2).unwrap(), state) {
                                        return Sexp::True;
                                    } else {
                                        return Sexp::Nil;
                                    }
                                }
                                "and" => {
                                    // Eval args until one of them yields nil, then return nil.
                                    // The remaining args are not evalled at all.
                                    // If no arg yields nil, return the last arg's value.
                                    for (pos, statement) in l.iter().enumerate().skip(1) {
                                        if pos + 1 == l.len() {
                                            return eval_sexp(statement, state);
                                        } else if let Sexp::Nil = eval_sexp(statement, state) {
                                            return Sexp::Nil;
                                        }
                                    }
                                }
                                "-" => {
                                    return Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap(), state)
                                            - eval_sexp_to_num(l.get(2).unwrap(), state),
                                    );
                                }
                                "+" => {
                                    return Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap(), state)
                                            + eval_sexp_to_num(l.get(2).unwrap(), state),
                                    );
                                }
                                "*" => {
                                    return Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap(), state)
                                            * eval_sexp_to_num(l.get(2).unwrap(), state),
                                    );
                                }
                                "<" => {
                                    if eval_sexp_to_num(l.get(1).unwrap(), state)
                                        < eval_sexp_to_num(l.get(2).unwrap(), state)
                                    {
                                        return Sexp::True;
                                    } else {
                                        return Sexp::Nil;
                                    }
                                }
                                ">" => {
                                    if eval_sexp_to_num(l.get(1).unwrap(), state)
                                        > eval_sexp_to_num(l.get(2).unwrap(), state)
                                    {
                                        return Sexp::True;
                                    } else {
                                        return Sexp::Nil;
                                    }
                                }
                                "%" => {
                                    return Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap(), state)
                                            % eval_sexp_to_num(l.get(2).unwrap(), state),
                                    );
                                }
                                "/" => {
                                    return Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap(), state)
                                            / eval_sexp_to_num(l.get(2).unwrap(), state),
                                    );
                                }
                                "progn" => {
                                    // Skip the actual "progn", then eval all and return last
                                    for (pos, statement) in l.iter().enumerate().skip(1) {
                                        if pos + 1 == l.len() {
                                            return eval_sexp(statement, state);
                                        } else {
                                            eval_sexp(statement, state);
                                        }
                                    }
                                }
                                "setq" => match l.get(1).unwrap().clone() {
                                    Sexp::Sym(s) => {
                                        let eval_value = eval_sexp(l.get(2).unwrap(), state);

                                        // Let's see if the symbol exists in local scope first
                                        for fn_map in state.fn_map.iter_mut() {
                                            if fn_map.get(&s).is_some() {
                                                fn_map.insert(s.clone(), eval_value.clone());

                                                return eval_value;
                                            }
                                        }

                                        state.global_map.insert(s.clone(), eval_value.clone());

                                        return eval_value;
                                    }
                                    _ => {
                                        panic!("setq expects a symbol");
                                    }
                                },
                                "defmacro" => {
                                    let macro_name = match l.get(1).unwrap() {
                                        Sexp::Sym(s) => s,
                                        _ => panic!("defmacro name should be a symbol"),
                                    };

                                    let macro_body = l.clone().into_iter().skip(2).collect::<Vec<Sexp>>();

                                    state.macro_map.insert(macro_name.clone(), Sexp::List(macro_body));

                                    return Sexp::Nil;
                                }
                                "if" => {
                                    // eval the conditional statement:
                                    match eval_sexp(l.get(1).unwrap(), state) {
                                        Sexp::Nil => match l.get(3) {
                                            // if false, eval the else statement
                                            Some(sexp) => return eval_sexp(sexp, state),
                                            None => {
                                                return Sexp::Nil;
                                            }
                                        },
                                        _ => {
                                            if let Some(else_statement) = l.get(2) {
                                                return eval_sexp(else_statement, state);
                                            } else {
                                                return Sexp::Nil;
                                            }
                                        }
                                    }
                                }
                                "while" => {
                                    // keep evaluating the body while the test is true
                                    let test = l.get(1).unwrap();

                                    let body = l.get(2).unwrap();

                                    loop {
                                        match eval_sexp(test, state) {
                                            Sexp::Nil => {
                                                return Sexp::Nil;
                                            }
                                            _ => {
                                                eval_sexp(body, state);
                                            }
                                        }
                                    }
                                }
                                "or" => {
                                    let first = eval_sexp(l.get(1).unwrap(), state);

                                    // if first evals to nil, return second
                                    if let Sexp::Nil = first {
                                        return eval_sexp(l.get(2).unwrap(), state);
                                    } else {
                                        return first;
                                    }
                                }
                                _ => {}
                            }

                            // check if we have a macro with this name
                            if let Some(macros) = state.macro_map.clone().get(s) {
                                match macros {
                                    Sexp::List(macro_list) => {
                                        let macro_params = match macro_list.first().unwrap() {
                                            Sexp::List(l) => l,
                                            _ => panic!("macro params should be a list"),
                                        };

                                        let mut current_function_param_map: HashMap<String, Sexp> = HashMap::new();
                                        // Map variable names to values passed into the function
                                        for (index, var) in macro_params.iter().enumerate() {
                                            if let Sexp::Sym(var_name) = var {
                                                let value = l.get(index + 1).unwrap().clone();

                                                current_function_param_map.insert(var_name.to_owned(), value);
                                            } else {
                                                panic!("macro param name should be a symbol")
                                            };
                                        }

                                        state.fn_map.push(current_function_param_map);

                                        let ret = eval_sexp(macro_list.get(1).unwrap(), state);

                                        state.fn_map.pop();

                                        return ret;
                                    }
                                    _ => {
                                        panic!("macro should be a list")
                                    }
                                }
                            }

                            // Not an intrinsic, or a macro.... could it be a variable?
                            match get_from_fn_map_or_global(s, state) {
                                Some(var) => match var {
                                    Sexp::List(ourfn) => execute_function(ourfn, l.as_slice(), state),
                                    _ => var,
                                },
                                None => panic!("unrecognized symbol: {s}"),
                            }
                        }
                        Sexp::List(_) => {
                            // We are evaluating a list, whose first element is a list
                            // This must be a function call!
                            let r = eval_sexp(first_elem_sexp, state);

                            match r {
                                Sexp::List(ourfn) => execute_function(ourfn, l.as_slice(), state),
                                _ => r,
                            }
                        }
                    }
                }
            }
        }
        _ => sexp.clone(), // An atom that is not a symbol can evaluate to itself
    }
}
