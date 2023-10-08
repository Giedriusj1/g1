use crate::sexp::{Atom, Sexp};

use std::collections::HashMap;

pub(crate) fn eval_sexp(sexp: &Sexp) -> Sexp {
    let mut global_map: HashMap<String, Sexp> = HashMap::new();

    let mut fn_map: Vec<HashMap<String, Sexp>> = vec![];

    eval_sexp_internal(sexp, &mut global_map, &mut fn_map)
}

fn eval_sexp_expect_num(
    sexp: &Sexp,
    g_map: &mut HashMap<String, Sexp>,
    fn_map: &mut Vec<HashMap<String, Sexp>>,
) -> i32 {
    match eval_sexp_internal(sexp, g_map, fn_map) {
        Sexp::Atom(Atom::Num(d)) => d,
        _ => panic!("expected a number"),
    }
}

fn get_from_fn_map_or_global(
    var_name: &String,
    g_map: &HashMap<String, Sexp>,
    fn_map: &[HashMap<String, Sexp>],
) -> Option<Sexp> {
    // Check the global map first
    match g_map.get(var_name) {
        Some(sexp) => return Some(sexp.to_owned()),
        None => {
            // Check the function maps now
            for var in fn_map.iter().rev() {
                if let Some(sexp) = var.get(var_name) {
                    return Some(sexp.to_owned());
                }
            }
        }
    }

    None
}

pub(crate) fn eval_sexp_internal(
    sexp: &Sexp,
    g_map: &mut HashMap<String, Sexp>,
    fn_map: &mut Vec<HashMap<String, Sexp>>,
) -> Sexp {
    match sexp {
        Sexp::Atom(a) => match a {
            Atom::Sym(s) => match get_from_fn_map_or_global(s, g_map, fn_map) {
                Some(var) => var,
                None => panic!("symbol {s} is not defined"),
            },
            // An atom that is not a symbol can evaluate to itself
            _ => sexp.clone(),
        },
        Sexp::List(l) => {
            if l.is_empty() {
                Sexp::Atom(Atom::Nil) // empty list, so treat it as nil
            } else {
                match l.get(0).unwrap() {
                    Sexp::Atom(a) => match a {
                        Atom::Sym(s) => {
                            if s == "cdr" {
                                assert_eq!(l.len(), 2);
                                return match eval_sexp_internal(l.get(1).unwrap(), g_map, fn_map) {
                                    Sexp::Atom(_) => panic!("cdr needs a list"),
                                    Sexp::List(l) => {
                                        let mut vec = l.clone();
                                        vec.remove(0);
                                        Sexp::List(vec)
                                    }
                                };
                            } else if s == "cons" {
                                assert_eq!(l.len(), 3);
                                let first = eval_sexp_internal(l.get(1).unwrap(), g_map, fn_map);

                                match eval_sexp_internal(l.get(2).unwrap(), g_map, fn_map) {
                                    Sexp::Atom(a) => return Sexp::List(vec![first, Sexp::Atom(a)]),
                                    Sexp::List(l) => {
                                        let mut vec = vec![first];
                                        vec.extend(l.clone());

                                        return Sexp::List(vec);
                                    }
                                }
                            } else if s == "list" {
                                if l.len() == 1 {
                                    return Sexp::Atom(Atom::Nil);
                                }

                                let mut vec = vec![];

                                for sexp in l.iter().skip(1) {
                                    vec.push(eval_sexp_internal(sexp, g_map, fn_map));
                                }

                                return Sexp::List(vec);
                            } else if s == "car" {
                                match eval_sexp_internal(l.get(1).unwrap(), g_map, fn_map) {
                                    Sexp::Atom(_) => todo!(),
                                    Sexp::List(l) => return l.get(0).unwrap().clone(),
                                }
                            } else if s == "quote" {
                                return l.get(1).unwrap().clone();
                            } else if s == "message" {
                                println!("{}", eval_sexp_internal(l.get(1).unwrap(), g_map, fn_map));
                                return Sexp::Atom(Atom::True);
                            } else if s == "=" {
                                if eval_sexp_expect_num(l.get(1).unwrap(), g_map, fn_map)
                                    == eval_sexp_expect_num(l.get(2).unwrap(), g_map, fn_map)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == "equal" {
                                if eval_sexp_internal(l.get(1).unwrap(), g_map, fn_map)
                                    == eval_sexp_internal(l.get(2).unwrap(), g_map, fn_map)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == "and" {
                                // DEFUN ("and", Fand, Sand, 0, UNEVALLED, 0,
                                //        doc: /* Eval args until one of them yields nil, then return nil.
                                // The remaining args are not evalled at all.
                                // If no arg yields nil, return the last arg's value.
                                // usage: (and CONDITIONS...)  */)

                                for (pos, statement) in l.iter().enumerate().skip(1) {
                                    if pos + 1 == l.len() {
                                        return eval_sexp_internal(statement, g_map, fn_map);
                                    } else if let Sexp::Atom(Atom::Nil) = eval_sexp_internal(statement, g_map, fn_map) {
                                        return Sexp::Atom(Atom::Nil);
                                    }
                                }
                            } else if s == "-" {
                                return Sexp::Atom(Atom::Num(
                                    eval_sexp_expect_num(l.get(1).unwrap(), g_map, fn_map)
                                        - eval_sexp_expect_num(l.get(2).unwrap(), g_map, fn_map),
                                ));
                            } else if s == "+" {
                                return Sexp::Atom(Atom::Num(
                                    eval_sexp_expect_num(l.get(1).unwrap(), g_map, fn_map)
                                        + eval_sexp_expect_num(l.get(2).unwrap(), g_map, fn_map),
                                ));
                            } else if s == "*" {
                                return Sexp::Atom(Atom::Num(
                                    eval_sexp_expect_num(l.get(1).unwrap(), g_map, fn_map)
                                        * eval_sexp_expect_num(l.get(2).unwrap(), g_map, fn_map),
                                ));
                            } else if s == "<" {
                                if eval_sexp_expect_num(l.get(1).unwrap(), g_map, fn_map)
                                    < eval_sexp_expect_num(l.get(2).unwrap(), g_map, fn_map)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == ">" {
                                if eval_sexp_expect_num(l.get(1).unwrap(), g_map, fn_map)
                                    > eval_sexp_expect_num(l.get(2).unwrap(), g_map, fn_map)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == "%" {
                                return Sexp::Atom(Atom::Num(
                                    eval_sexp_expect_num(l.get(1).unwrap(), g_map, fn_map)
                                        % eval_sexp_expect_num(l.get(2).unwrap(), g_map, fn_map),
                                ));
                            } else if s == "progn" {
                                // Skip the actual "progn", then eval all and return last
                                for (pos, statement) in l.iter().enumerate().skip(1) {
                                    if pos + 1 == l.len() {
                                        return eval_sexp_internal(statement, g_map, fn_map);
                                    } else {
                                        eval_sexp_internal(statement, g_map, fn_map);
                                    }
                                }
                            } else if s == "set" {
                                match eval_sexp_internal(l.get(1).unwrap(), g_map, fn_map) {
                                    Sexp::Atom(Atom::Sym(s)) => {
                                        let new_value = eval_sexp_internal(l.get(2).unwrap(), g_map, fn_map);

                                        g_map.insert(s, new_value.clone());

                                        return new_value;
                                    }
                                    _ => panic!("set expects a symbol"),
                                };
                            } else if s == "setq" {
                                match l.get(1).unwrap().clone() {
                                    Sexp::Atom(Atom::Sym(s)) => {
                                        let new_value = eval_sexp_internal(l.get(2).unwrap(), g_map, fn_map);

                                        g_map.insert(s, new_value.clone());

                                        return new_value;
                                    }
                                    _ => {
                                        panic!("a");
                                    }
                                }
                            } else if s == "if" {
                                // eval the conditional statement:
                                match eval_sexp_internal(l.get(1).unwrap(), g_map, fn_map) {
                                    Sexp::Atom(Atom::Nil) => match l.get(3) {
                                        // if false, eval the else statement
                                        Some(sexp) => return eval_sexp_internal(sexp, g_map, fn_map),
                                        None => {
                                            return Sexp::Atom(Atom::Nil);
                                        }
                                    },
                                    _ => {
                                        return eval_sexp_internal(l.get(2).unwrap(), g_map, fn_map);
                                    }
                                }
                            } else if s == "or" {
                                let first = eval_sexp_internal(l.get(1).unwrap(), g_map, fn_map);

                                // if first evals to nil, return second
                                if let Sexp::Atom(Atom::Nil) = first {
                                    return eval_sexp_internal(l.get(2).unwrap(), g_map, fn_map);
                                } else {
                                    return first;
                                }
                            }
                            match get_from_fn_map_or_global(s, g_map, fn_map) {
                                Some(var) => {
                                    match var {
                                        Sexp::Atom(a) => Sexp::Atom(a),

                                        Sexp::List(ourfn) => {
                                            let ourfnparams = match ourfn.get(0).unwrap() {
                                                Sexp::List(l) => l,
                                                _ => panic!("function params should be a list"),
                                            };

                                            let mut current_function_param_map: HashMap<String, Sexp> = HashMap::new();
                                            // Map variable names to values passed into the function
                                            for (index, var) in ourfnparams.iter().enumerate() {
                                                // println!("index {:#?}, var: {:#?}", index, var);

                                                let var_string =
                                                    if let Sexp::Atom(Atom::Sym(s)) = var { s } else { panic!("") };

                                                let v = l.get(index + 1).unwrap().clone();

                                                let var_eval = eval_sexp_internal(&v, g_map, fn_map);

                                                current_function_param_map.insert(var_string.to_owned(), var_eval);
                                            }

                                            fn_map.push(current_function_param_map);

                                            let ret = eval_sexp_internal(ourfn.get(1).unwrap(), g_map, fn_map);

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
                        Atom::Num(_) => panic!(),
                    },
                    Sexp::List(_) => eval_sexp_internal(&Sexp::List(l.clone()), g_map, fn_map), // TODO: is this clone necessary?
                } // match first list elem
            } // not empty list?
        } // is a list?
    } // match Sexp
}
