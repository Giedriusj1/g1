use crate::sexp;

use std::collections::HashMap;

pub(crate) fn eval_sexp(sexp: sexp::Sexp) -> sexp::Atom {
    let mut global_map: HashMap<String, sexp::Sexp> = HashMap::new();

    let mut fn_map: Vec<HashMap<String, sexp::Sexp>> = vec![];

    return eval_sexp_internal(sexp, &mut global_map, &mut fn_map);
}

fn eval_sexp_expect_digit(
    sexp: sexp::Sexp,
    g_map: &mut HashMap<String, sexp::Sexp>,
    fn_map: &mut Vec<HashMap<String, sexp::Sexp>>,
) -> i32 {
    match eval_sexp_internal(sexp, g_map, fn_map) {
        sexp::Atom::Digit(d) => d,
        _ => panic!(),
    }
}

fn get_from_fn_map_or_global(
    var_name: String,
    g_map: &mut HashMap<String, sexp::Sexp>,
    fn_map: &mut Vec<HashMap<String, sexp::Sexp>>,
) -> Option<sexp::Sexp> {
    // We'll try to find the var in a function var stack first

    for var in fn_map.iter().rev() {
        match var.get(&var_name) {
            Some(sexp) => return Some(sexp.to_owned()),
            None => (),
        }
    }

    match g_map.get(&var_name) {
        Some(sexp) => return Some(sexp.to_owned()),
        None => None,
    }
}

pub(crate) fn eval_sexp_internal(
    sexp: sexp::Sexp,
    g_map: &mut HashMap<String, sexp::Sexp>,
    fn_map: &mut Vec<HashMap<String, sexp::Sexp>>,
) -> sexp::Atom {
    // TODO: we probably want sexp returned here
    match sexp {
        sexp::Sexp::Atom(a) => match a {
            sexp::Atom::String(s) => {
                if s == "nil" {
                    return sexp::Atom::Nil;
                }
                if s == "t" {
                    return sexp::Atom::True;
                }

                match get_from_fn_map_or_global(s.clone(), g_map, fn_map) {
                    Some(var) => match var {
                        sexp::Sexp::Atom(a) => a,
                        sexp::Sexp::List(_) => panic!("todo here pls"),
                    },
                    None => panic!("symbol {} is not defined", s),
                }
            }
            _ => a,
        },
        sexp::Sexp::List(l) => {
            if l.len() == 0 {
                sexp::Atom::Nil // empty list, so treat it as nil
            } else {
                match l.get(0).unwrap() {
                    sexp::Sexp::Atom(a) => match a {
                        sexp::Atom::String(s) => {
                            if s == "msg" {
                                eprintln!("msg: {:#?}", eval_sexp_internal(l.get(1).unwrap().clone(), g_map, fn_map));
                                return sexp::Atom::True;
                            } else if s == "==" {
                                if eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                    == eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map)
                                {
                                    return sexp::Atom::True;
                                } else {
                                    return sexp::Atom::Nil;
                                }
                            } else if s == "-" {
                                return sexp::Atom::Digit(
                                    eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                        - eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map),
                                );
                            } else if s == "+" {
                                return sexp::Atom::Digit(
                                    eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                        + eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map),
                                );
                            } else if s == "*" {
                                return sexp::Atom::Digit(
                                    eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                        + eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map),
                                );
                            } else if s == "<" {
                                if eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                    < eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map)
                                {
                                    return sexp::Atom::True;
                                } else {
                                    return sexp::Atom::Nil;
                                }
                            } else if s == ">" {
                                if eval_sexp_expect_digit(l.get(1).unwrap().clone(), g_map, fn_map)
                                    > eval_sexp_expect_digit(l.get(2).unwrap().clone(), g_map, fn_map)
                                {
                                    return sexp::Atom::True;
                                } else {
                                    return sexp::Atom::Nil;
                                }
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
                                    sexp::Sexp::Atom(sexp::Atom::String(s)) => {
                                        g_map.insert(s, l.get(2).unwrap().clone());

                                        return sexp::Atom::Nil;
                                    }
                                    _ => {
                                        panic!("a");
                                    }
                                }
                            } else if s == "defun" {
                                let defun_name = match l.get(1).unwrap() {
                                    sexp::Sexp::Atom(a) => match a {
                                        sexp::Atom::String(s) => s,
                                        _ => panic!("aa"),
                                    },
                                    sexp::Sexp::List(_) => panic!(),
                                };

                                eprintln!("defun {:#?}", l);
                                g_map.insert(defun_name.to_owned(), sexp::Sexp::List(l));

                                return sexp::Atom::Nil;
                            } else if s == "if" {
                                println!("l.get(1).unwrap() {:#?}", l.get(1).unwrap());

                                // eval the conditional statement:
                                match eval_sexp_internal(l.get(1).unwrap().to_owned(), g_map, fn_map) {
                                    sexp::Atom::Nil => {
                                        return eval_sexp_internal(l.get(3).unwrap().to_owned(), g_map, fn_map);
                                    }
                                    _ => {
                                        return eval_sexp_internal(l.get(2).unwrap().to_owned(), g_map, fn_map);
                                    }
                                }
                            }
                            match get_from_fn_map_or_global(s.clone(), g_map, fn_map) {
                                Some(var) => {
                                    match var {
                                        sexp::Sexp::Atom(a) => a,

                                        sexp::Sexp::List(ourfn) => {
                                            let ourfnparams = match ourfn.get(2).unwrap() {
                                                sexp::Sexp::List(l) => l,
                                                _ => panic!("function params should be a list"),
                                            };

                                            let mut current_function_param_map: HashMap<String, sexp::Sexp> =
                                                HashMap::new();
                                            // Map variable names to values passed into the function
                                            for (index, var) in ourfnparams.iter().enumerate() {
                                                // println!("index {:#?}, var: {:#?}", index, var);

                                                let var_string = match var {
                                                    sexp::Sexp::Atom(a) => match a {
                                                        sexp::Atom::String(s) => s,
                                                        _ => panic!(""),
                                                    },
                                                    _ => panic!(""),
                                                };

                                                let v = l.get(index + 1).unwrap().clone();

                                                let var_eval = eval_sexp_internal(v, g_map, fn_map);

                                                current_function_param_map
                                                    .insert(var_string.to_owned(), sexp::Sexp::Atom(var_eval));
                                            }

                                            fn_map.push(current_function_param_map);

                                            let ret = eval_sexp_internal(ourfn.get(3).unwrap().clone(), g_map, fn_map);

                                            fn_map.pop();

                                            ret
                                        }
                                    }
                                }
                                None => panic!("symbol {} unrecognized", s),
                            }
                        }
                        sexp::Atom::Nil => sexp::Atom::Nil,
                        sexp::Atom::True => sexp::Atom::True,
                        sexp::Atom::Digit(_) => panic!(),
                    },
                    sexp::Sexp::List(_) => return eval_sexp_internal(sexp::Sexp::List(l), g_map, fn_map),
                } // match first list elem
            } // not empty list?
        } // is a list?
    } // match Sexp
}
