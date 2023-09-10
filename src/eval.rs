use crate::sexp::{Atom, Sexp};

use std::collections::HashMap;

pub(crate) fn eval_sexp(sexp: Sexp) -> Sexp {
    let mut global_map: HashMap<String, Sexp> = HashMap::new();

    let mut fn_map: Vec<HashMap<String, Sexp>> = vec![];

    eval_sexp_internal(sexp, &mut global_map, &mut fn_map)
}

fn eval_sexp_expect_digit(
    sexp: Sexp,
    g_map: &mut HashMap<String, Sexp>,
    fn_map: &mut Vec<HashMap<String, Sexp>>,
) -> i32 {
    match eval_sexp_internal(sexp, g_map, fn_map) {
        Sexp::Atom(Atom::Digit(d)) => d,
        _ => panic!(),
    }
}

fn get_from_fn_map_or_global(
    var_name: String,
    g_map: &HashMap<String, Sexp>,
    fn_map: &Vec<HashMap<String, Sexp>>,
) -> Option<Sexp> {
    // We'll try to find the var in a function var stack first

    for var in fn_map.iter().rev() {
        if let Some(sexp) = var.get(&var_name) {
            return Some(sexp.to_owned());
        }
    }

    g_map.get(&var_name).map(|sexp| sexp.to_owned())
}

pub(crate) fn eval_sexp_internal(
    sexp: Sexp,
    g_map: &mut HashMap<String, Sexp>,
    fn_map: &mut Vec<HashMap<String, Sexp>>,
) -> Sexp {
    match sexp {
        Sexp::Atom(a) => match a {
            Atom::Symbol(s) => {
                if s == "nil" {
                    return Sexp::Atom(Atom::Nil);
                }
                if s == "t" {
                    return Sexp::Atom(Atom::True);
                }

                match get_from_fn_map_or_global(s.clone(), g_map, fn_map) {
                    Some(var) => match var {
                        Sexp::Atom(a) => Sexp::Atom(a),
                        Sexp::List(_l) => todo!("todo here pls"),
                    },
                    None => panic!("symbol {s} is not defined"),
                }
            }
            _ => Sexp::Atom(a),
        },
        Sexp::List(l) => {
            if l.is_empty() {
                Sexp::Atom(Atom::Nil) // empty list, so treat it as nil
            } else {
                match l.get(0).unwrap() {
                    Sexp::Atom(a) => match a {
                        Atom::Symbol(s) => {
                            if s == "quote" {
                                return l.get(1).unwrap().clone();
                            } else if s == "message" {
                                println!("message {:#?}", eval_sexp_internal(l.get(1).unwrap().clone(), g_map, fn_map));
                                return Sexp::Atom(Atom::True);
                            } else if s == "==" {
                                if eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                    == eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == "-" {
                                return Sexp::Atom(Atom::Digit(
                                    eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                        - eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map),
                                ));
                            } else if s == "+" {
                                return Sexp::Atom(Atom::Digit(
                                    eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                        + eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map),
                                ));
                            } else if s == "*" {
                                return Sexp::Atom(Atom::Digit(
                                    eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                        * eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map),
                                ));
                            } else if s == "<" {
                                if eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                    < eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == ">" {
                                if eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                    > eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == "%" {
                                return Sexp::Atom(Atom::Digit(
                                    eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                        % eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map),
                                ));
                            } else if s == "progn" {
                                // Eval all and return last
                                let statements = l.iter().enumerate().skip(1); // Skip the actual "progn"

                                for (pos, statement) in statements {
                                    if pos + 1 == l.len() {
                                        return eval_sexp_internal(statement.clone(), g_map, fn_map);
                                    } else {
                                        eval_sexp_internal(statement.clone(), g_map, fn_map);
                                    }
                                }
                            } else if s == "setq" {
                                match l.get(1).unwrap().clone() {
                                    Sexp::Atom(Atom::Symbol(s)) => {
                                        let new_value = eval_sexp_internal(l.get(2).unwrap().clone(), g_map, fn_map);

                                        g_map.insert(s, new_value.clone());

                                        return new_value;
                                    }
                                    _ => {
                                        panic!("a");
                                    }
                                }
                            } else if s == "defun" {
                                let defun_name = match l.get(1).unwrap() {
                                    Sexp::Atom(a) => match a {
                                        Atom::Symbol(s) => s,
                                        _ => panic!("aa"),
                                    },
                                    Sexp::List(_) => panic!(),
                                };

                                // eprintln!("defun {l:#?}");
                                g_map.insert(defun_name.to_owned(), Sexp::List(l));

                                return Sexp::Atom(Atom::Nil);
                            } else if s == "if" {
                                // eprintln!("l.get(1).unwrap() {:#?}", l.get(1).unwrap());

                                // eval the conditional statement:
                                match eval_sexp_internal(l.get(1).unwrap().to_owned(), g_map, fn_map) {
                                    Sexp::Atom(Atom::Nil) => match l.get(3) {
                                        // if false, eval the else statement
                                        Some(sexp) => return eval_sexp_internal(sexp.to_owned(), g_map, fn_map),
                                        None => {
                                            return Sexp::Atom(Atom::Nil);
                                        }
                                    },
                                    _ => {
                                        return eval_sexp_internal(l.get(2).unwrap().to_owned(), g_map, fn_map);
                                    }
                                }
                            } else if s == "or" {
                                let first = eval_sexp_internal(l.get(1).unwrap().to_owned(), g_map, fn_map);

                                // if first evals to nil, return second
                                if let Sexp::Atom(Atom::Nil) = first {
                                    return eval_sexp_internal(l.get(2).unwrap().to_owned(), g_map, fn_map);
                                } else {
                                    return first;
                                }
                            }
                            match get_from_fn_map_or_global(s.clone(), g_map, fn_map) {
                                Some(var) => {
                                    match var {
                                        Sexp::Atom(a) => Sexp::Atom(a),

                                        Sexp::List(ourfn) => {
                                            let ourfnparams = match ourfn.get(2).unwrap() {
                                                Sexp::List(l) => l,
                                                _ => panic!("function params should be a list"),
                                            };

                                            let mut current_function_param_map: HashMap<String, Sexp> = HashMap::new();
                                            // Map variable names to values passed into the function
                                            for (index, var) in ourfnparams.iter().enumerate() {
                                                // println!("index {:#?}, var: {:#?}", index, var);

                                                let var_string =
                                                    if let Sexp::Atom(Atom::Symbol(s)) = var { s } else { panic!("") };

                                                let v = l.get(index + 1).unwrap().clone();

                                                let var_eval = eval_sexp_internal(v, g_map, fn_map);

                                                current_function_param_map.insert(var_string.to_owned(), var_eval);
                                            }

                                            fn_map.push(current_function_param_map);

                                            let ret = eval_sexp_internal(ourfn.get(3).unwrap().clone(), g_map, fn_map);

                                            fn_map.pop();

                                            ret
                                        }
                                    }
                                }
                                None => panic!("unrecognized symbol: {s}"),
                            }
                        }
                        Atom::Nil => Sexp::Atom(Atom::Nil),
                        Atom::True => Sexp::Atom(Atom::True),
                        Atom::Digit(_) => panic!(),
                    },
                    Sexp::List(_) => eval_sexp_internal(Sexp::List(l), g_map, fn_map),
                } // match first list elem
            } // not empty list?
        } // is a list?
    } // match Sexp
}
