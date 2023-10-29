use crate::sexp::{Atom, Sexp};

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

fn eval_sexp_to_num(sexp: &Sexp, eval_state: &mut EvalState) -> i32 {
    let res = eval_sexp(sexp, eval_state);

    if let Sexp::Atom(Atom::Num(n)) = res {
        n
    } else {
        panic!("expected a num, but found: {:#?}", res)
    }
}

fn get_from_fn_map_or_global(var_name: &String, eval_state: &EvalState) -> Option<Sexp> {
    // TODO: this is actually quite inefficient, because if we are in a deep nested function,
    // and we want to access a global var, we have to iterate through all the fn_maps first.

    // Check the functions map first (that way we default to local variables first, and allow for
    // variable shadowing)
    for var in eval_state.fn_map.iter().rev() {
        if let Some(sexp) = var.get(var_name) {
            return Some(sexp.to_owned());
        }
    }

    // Check the global map last.
    if let Some(sexp) = eval_state.global_map.get(var_name) {
        return Some(sexp.to_owned());
    }

    None
}

pub(crate) fn eval_sexp(sexp: &Sexp, eval_state: &mut EvalState) -> Sexp {
    match sexp {
        Sexp::Atom(a) => match a {
            Atom::Sym(s) => match get_from_fn_map_or_global(s, eval_state) {
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
                            if s == "let" {
                                // Let statement should bind variables for the duration of the let statement.

                                let param_list = l.get(1).unwrap();
                                // println!("param_list: {}", param_list);
                                match param_list {
                                    Sexp::List(l) => {
                                        // println!("l: {:?}", l);

                                        let mut current_function_param_map: HashMap<String, Sexp> = HashMap::new();

                                        for var in l {
                                            match var {
                                                Sexp::Atom(_) => {
                                                    panic!("param list expects a list")
                                                }
                                                Sexp::List(l) => {
                                                    let var_string: &String = match l.get(0).unwrap() {
                                                        Sexp::Atom(Atom::Sym(s)) => s,
                                                        _ => panic!(),
                                                    };

                                                    let v = l.get(1).unwrap().clone();

                                                    let var_eval = eval_sexp(&v, eval_state);

                                                    current_function_param_map.insert(var_string.to_owned(), var_eval);
                                                }
                                            }
                                        }

                                        eval_state.fn_map.push(current_function_param_map);
                                    }
                                    _ => panic!("paramlist should be a list"),
                                }

                                // Skip the actual "let", and the subsequent paramlist,
                                // then eval all remaining and return last
                                let statements = l.iter().enumerate().skip(2);

                                if statements.len() > 0 {
                                    for (pos, statement) in statements {
                                        // println!("pos: {}, statement: {}", pos, statement);
                                        if pos + 1 == l.len() {
                                            let r = eval_sexp(statement, eval_state);

                                            eval_state.fn_map.pop();

                                            return r;
                                        } else {
                                            eval_sexp(statement, eval_state);
                                        }
                                    }
                                } else {
                                    eval_state.fn_map.pop();
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == "cdr" {
                                assert_eq!(l.len(), 2);
                                return match eval_sexp(l.get(1).unwrap(), eval_state) {
                                    Sexp::Atom(_) => panic!("cdr needs a list"),
                                    Sexp::List(l) => {
                                        let mut vec = l.clone();
                                        vec.remove(0);
                                        Sexp::List(vec)
                                    }
                                };
                            } else if s == "cons" {
                                assert_eq!(l.len(), 3);
                                let first = eval_sexp(l.get(1).unwrap(), eval_state);

                                match eval_sexp(l.get(2).unwrap(), eval_state) {
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
                                    vec.push(eval_sexp(sexp, eval_state));
                                }

                                return Sexp::List(vec);
                            } else if s == "car" {
                                match eval_sexp(l.get(1).unwrap(), eval_state) {
                                    Sexp::Atom(_) => todo!(),
                                    Sexp::List(l) => return l.get(0).unwrap().clone(),
                                }
                            } else if s == "quote" {
                                return l.get(1).unwrap().clone();
                            } else if s == "backtick" {
                                return l.get(1).unwrap().clone();
                            } else if s == "comma" {
                                // behave as quote for now, but as some point we'll want to start evaluating
                                return l.get(1).unwrap().clone();
                            } else if s == "eval" {
                                let statement = l.get(1).unwrap();
                                // println!("eval statement: {}", statement);
                                let after_eval = eval_sexp(statement, eval_state);
                                // println!("after eval: {}", after_eval);

                                return eval_sexp(&after_eval, eval_state);
                            } else if s == "message" {
                                println!("{}", eval_sexp(l.get(1).unwrap(), eval_state));
                                return Sexp::Atom(Atom::True);
                            } else if s == "=" {
                                if eval_sexp_to_num(l.get(1).unwrap(), eval_state)
                                    == eval_sexp_to_num(l.get(2).unwrap(), eval_state)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == "equal" {
                                if eval_sexp(l.get(1).unwrap(), eval_state) == eval_sexp(l.get(2).unwrap(), eval_state)
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
                                        return eval_sexp(statement, eval_state);
                                    } else if let Sexp::Atom(Atom::Nil) = eval_sexp(statement, eval_state) {
                                        return Sexp::Atom(Atom::Nil);
                                    }
                                }
                            } else if s == "-" {
                                return Sexp::Atom(Atom::Num(
                                    eval_sexp_to_num(l.get(1).unwrap(), eval_state)
                                        - eval_sexp_to_num(l.get(2).unwrap(), eval_state),
                                ));
                            } else if s == "+" {
                                return Sexp::Atom(Atom::Num(
                                    eval_sexp_to_num(l.get(1).unwrap(), eval_state)
                                        + eval_sexp_to_num(l.get(2).unwrap(), eval_state),
                                ));
                            } else if s == "*" {
                                return Sexp::Atom(Atom::Num(
                                    eval_sexp_to_num(l.get(1).unwrap(), eval_state)
                                        * eval_sexp_to_num(l.get(2).unwrap(), eval_state),
                                ));
                            } else if s == "<" {
                                if eval_sexp_to_num(l.get(1).unwrap(), eval_state)
                                    < eval_sexp_to_num(l.get(2).unwrap(), eval_state)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == ">" {
                                if eval_sexp_to_num(l.get(1).unwrap(), eval_state)
                                    > eval_sexp_to_num(l.get(2).unwrap(), eval_state)
                                {
                                    return Sexp::Atom(Atom::True);
                                } else {
                                    return Sexp::Atom(Atom::Nil);
                                }
                            } else if s == "%" {
                                return Sexp::Atom(Atom::Num(
                                    eval_sexp_to_num(l.get(1).unwrap(), eval_state)
                                        % eval_sexp_to_num(l.get(2).unwrap(), eval_state),
                                ));
                            } else if s == "progn" {
                                // Skip the actual "progn", then eval all and return last
                                for (pos, statement) in l.iter().enumerate().skip(1) {
                                    if pos + 1 == l.len() {
                                        return eval_sexp(statement, eval_state);
                                    } else {
                                        eval_sexp(statement, eval_state);
                                    }
                                }
                            } else if s == "set" {
                                match eval_sexp(l.get(1).unwrap(), eval_state) {
                                    Sexp::Atom(Atom::Sym(s)) => {
                                        let new_value = eval_sexp(l.get(2).unwrap(), eval_state);

                                        // Let's see if the symbol exists in local scope first
                                        for f in eval_state.fn_map.iter_mut() {
                                            if f.get(&s).is_some() {
                                                f.insert(s, new_value.clone());

                                                return new_value;
                                            }
                                        }

                                        eval_state.global_map.insert(s, new_value.clone());

                                        return new_value;
                                    }
                                    _ => panic!("set expects a symbol"),
                                };
                            } else if s == "setq" {
                                match l.get(1).unwrap().clone() {
                                    Sexp::Atom(Atom::Sym(s)) => {
                                        let new_value = eval_sexp(l.get(2).unwrap(), eval_state);

                                        // Let's see if the symbol exists in local scope first
                                        for f in eval_state.fn_map.iter_mut() {
                                            if f.get(&s).is_some() {
                                                f.insert(s, new_value.clone());

                                                return new_value;
                                            }
                                        }

                                        eval_state.global_map.insert(s, new_value.clone());

                                        return new_value;
                                    }
                                    _ => {
                                        panic!("setq expects a symbol");
                                    }
                                }
                            } else if s == "defmacro" {
                                let macro_name = match l.get(1).unwrap() {
                                    Sexp::Atom(Atom::Sym(s)) => s,
                                    _ => panic!("defmacro name should be a symbol"),
                                };

                                // println!("macro_name: {}", macro_name);
                                // println!("defmacro: {:#?}", l);

                                let macro_body = l.clone().into_iter().skip(2).collect::<Vec<Sexp>>();

                                // println!("macro_body: {:#?}", macro_body);

                                eval_state.macro_map.insert(macro_name.clone(), Sexp::List(macro_body));

                                return Sexp::Atom(Atom::Nil);
                            } else if s == "if" {
                                // eval the conditional statement:
                                match eval_sexp(l.get(1).unwrap(), eval_state) {
                                    Sexp::Atom(Atom::Nil) => match l.get(3) {
                                        // if false, eval the else statement
                                        Some(sexp) => return eval_sexp(sexp, eval_state),
                                        None => {
                                            return Sexp::Atom(Atom::Nil);
                                        }
                                    },
                                    _ => {
                                        return eval_sexp(l.get(2).unwrap(), eval_state);
                                    }
                                }
                            } else if s == "or" {
                                let first = eval_sexp(l.get(1).unwrap(), eval_state);

                                // if first evals to nil, return second
                                if let Sexp::Atom(Atom::Nil) = first {
                                    return eval_sexp(l.get(2).unwrap(), eval_state);
                                } else {
                                    return first;
                                }
                            }

                            // check if we have a macro with this name
                            if let Some(macros) = eval_state.macro_map.clone().get(s) {
                                match macros {
                                    Sexp::List(macro_list) => {
                                        let macro_params = match macro_list.get(0).unwrap() {
                                            Sexp::List(l) => l,
                                            _ => panic!("function params should be a list"),
                                        };

                                        let mut current_function_param_map: HashMap<String, Sexp> = HashMap::new();
                                        // Map variable names to values passed into the function
                                        for (index, var) in macro_params.iter().enumerate() {
                                            // println!("index {:#?}, var: {:#?}", index, var);

                                            let var_name =
                                                if let Sexp::Atom(Atom::Sym(s)) = var { s } else { panic!("") };

                                            let v = l.get(index + 1).unwrap().clone();

                                            current_function_param_map.insert(var_name.to_owned(), v);
                                        }

                                        eval_state.fn_map.push(current_function_param_map);

                                        let ret = eval_sexp(macro_list.get(1).unwrap(), eval_state);

                                        eval_state.fn_map.pop();

                                        return ret;
                                    }
                                    Sexp::Atom(_) => panic!("macro should be a list"),
                                }
                            }

                            // Not an intrinsic, or a macro.... could it be a variable?
                            match get_from_fn_map_or_global(s, eval_state) {
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

                                                let var_eval = eval_sexp(&v, eval_state);

                                                current_function_param_map.insert(var_string.to_owned(), var_eval);
                                            }

                                            eval_state.fn_map.push(current_function_param_map);

                                            let ret = eval_sexp(ourfn.get(1).unwrap(), eval_state);

                                            eval_state.fn_map.pop();

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
                    Sexp::List(_) => {
                        match eval_sexp(l.get(0).unwrap(), eval_state) {
                            Sexp::Atom(a) => Sexp::Atom(a),
                            Sexp::List(ourfn) => {
                                // We are evaluating a list, whose first element is a list
                                // This must  a function call!

                                let ourfnparams = match ourfn.get(0).unwrap() {
                                    Sexp::List(l) => l,
                                    _ => panic!("function params should be a list"),
                                };

                                let mut current_function_param_map: HashMap<String, Sexp> = HashMap::new();
                                // Map variable names to values passed into the function
                                for (index, var) in ourfnparams.iter().enumerate() {
                                    // println!("index {:#?}, var: {:#?}", index, var);

                                    let var_string = if let Sexp::Atom(Atom::Sym(s)) = var { s } else { panic!("") };

                                    let v = l.get(index + 1).unwrap().clone();

                                    let var_eval = eval_sexp(&v, eval_state);

                                    current_function_param_map.insert(var_string.to_owned(), var_eval);
                                }

                                eval_state.fn_map.push(current_function_param_map);

                                let ret = eval_sexp(ourfn.get(1).unwrap(), eval_state);

                                eval_state.fn_map.pop();

                                ret
                            }
                        }
                    }
                } // match first list elem
            } // not empty list?
        } // is a list?
    } // match Sexp
}
