use crate::sexp;

use std::collections::HashMap;

pub(crate) fn eval_sexp(sexp: sexp::Sexp) -> sexp::Atom {
    let mut map: HashMap<String, sexp::Sexp> = HashMap::new();

    return eval_sexp_internal(sexp, &mut map);
}

pub(crate) fn eval_sexp_internal(
    sexp: sexp::Sexp,
    map: &mut HashMap<String, sexp::Sexp>,
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

                if map.contains_key(&s) {
                    match map.get(&s).unwrap().to_owned() {
                        sexp::Sexp::Atom(a) => a,
                        sexp::Sexp::List(_) => panic!("todo here pls"),
                    }
                } else {
                    panic!("symbol {} is not defined", s);
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
                                eprintln!(
                                    "msg: {:#?}",
                                    eval_sexp_internal(l.get(1).unwrap().clone(), map)
                                );
                                sexp::Atom::True
                            } else if s == "-" {
                                // We are being asked to subtract two numbers
                                let first = match eval_sexp_internal(l.get(1).unwrap().clone(), map)
                                {
                                    sexp::Atom::Digit(d) => d,
                                    _ => panic!(),
                                };

                                let second =
                                    match eval_sexp_internal(l.get(2).unwrap().clone(), map) {
                                        sexp::Atom::Digit(d) => d,
                                        _ => panic!(),
                                    };

                                return sexp::Atom::Digit(first - second);
                            } else if s == "+" {
                                let first = match eval_sexp_internal(l.get(1).unwrap().clone(), map)
                                {
                                    sexp::Atom::Digit(d) => d,
                                    _ => panic!(),
                                };

                                let second =
                                    match eval_sexp_internal(l.get(2).unwrap().clone(), map) {
                                        sexp::Atom::Digit(d) => d,
                                        _ => panic!(),
                                    };

                                sexp::Atom::Digit(first + second)
                            } else if s == "<" {
                                // We are being asked to compare two numbers
                                let first = match eval_sexp_internal(l.get(1).unwrap().clone(), map)
                                {
                                    sexp::Atom::Digit(d) => d,
                                    _ => panic!(),
                                };

                                let second =
                                    match eval_sexp_internal(l.get(2).unwrap().clone(), map) {
                                        sexp::Atom::Digit(d) => d,
                                        _ => panic!(),
                                    };

                                if first < second {
                                    return sexp::Atom::True;
                                } else {
                                    return sexp::Atom::Nil;
                                }
                            } else if s == "progn" {
                                // Eval all and return last
                                for (pos, statement) in l.iter().enumerate() {
                                    // println!("pos {:#?}", pos);
                                    // skip the first one, since it'll be progn
                                    if pos != 0 {
                                        if pos + 1 == l.len() {
                                            return eval_sexp_internal(statement.clone(), map);
                                        } else {
                                            eval_sexp_internal(statement.clone(), map);
                                        }
                                    }
                                }
                                panic!("progn failed");
                            } else if s == "setq" {
                                match l.get(1).unwrap().clone() {
                                    sexp::Sexp::Atom(sexp::Atom::String(s)) => {
                                        map.insert(s, l.get(2).unwrap().clone());

                                        sexp::Atom::Nil
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
                                map.insert(defun_name.to_owned(), sexp::Sexp::List(l));

                                sexp::Atom::Nil
                            } else if s == "if" {
                                println!("l.get(1).unwrap() {:#?}", l.get(1).unwrap());

                                // eval the conditional statement:
                                let a = eval_sexp_internal(l.get(1).unwrap().to_owned(), map);

                                match a {
                                    sexp::Atom::Nil => {
                                        return eval_sexp_internal(
                                            l.get(3).unwrap().to_owned(),
                                            map,
                                        );
                                    }
                                    _ => {
                                        return eval_sexp_internal(
                                            l.get(2).unwrap().to_owned(),
                                            map,
                                        );
                                    }
                                }
                            } else if map.contains_key(s) {
                                match map.get(s).unwrap().to_owned() {
                                    sexp::Sexp::Atom(a) => a,

                                    sexp::Sexp::List(ourfn) => {
                                        let ourfnparams = match ourfn.get(2).unwrap() {
                                            sexp::Sexp::List(l) => l,
                                            _ => panic!("function params should be a list"),
                                        };

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

                                            let var_eval = eval_sexp_internal(v, map);

                                            map.insert(
                                                var_string.to_owned(),
                                                sexp::Sexp::Atom(var_eval),
                                            );
                                        }

                                        eval_sexp_internal(ourfn.get(3).unwrap().clone(), map)
                                    }
                                }
                            } else {
                                panic!("symbol {} unrecognized", s);
                            }
                        }
                        sexp::Atom::Nil => sexp::Atom::Nil,
                        sexp::Atom::True => sexp::Atom::True,
                        sexp::Atom::Digit(_) => panic!(),
                    },
                    sexp::Sexp::List(_) => return eval_sexp_internal(sexp::Sexp::List(l), map),
                } // match first list elem
            } // not empty list?
        } // is a list?
    } // match Sexp
}
