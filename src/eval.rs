use crate::sexp::Sexp;

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub(crate) struct EvalState {
    macro_map: HashMap<String, Rc<Sexp>>,
    global_map: HashMap<String, Rc<Sexp>>,
    fn_map: Vec<HashMap<String, Rc<Sexp>>>,
}

impl EvalState {
    pub(crate) fn new() -> EvalState {
        EvalState { macro_map: HashMap::new(), global_map: HashMap::new(), fn_map: vec![] }
    }
}

fn eval_sexp_to_num(sexp: Rc<Sexp>, state: &mut EvalState) -> i64 {
    let res = eval_sexp(sexp, state);

    if let Sexp::Num(n) = *res {
        n
    } else {
        panic!("expected a number, but found: {res}")
    }
}

fn get_from_fn_map_or_global(var_name: &String, state: &EvalState) -> Option<Rc<Sexp>> {
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

fn eval_commas_within_backtick(sexp: Rc<Sexp>, state: &mut EvalState) -> Rc<Sexp> {
    let root_sexp = sexp.clone();

    match *root_sexp {
        Sexp::List(ref l) => Rc::new(Sexp::List(
            l.iter()
                .map(|sexp| {
                    let t = sexp.clone();

                    if let Sexp::List(ref l) = *t {
                        // let l = l.clone();
                        let first = l.first().cloned();

                        if let Some(first) = first {
                            let first = first.clone();
                            if let Sexp::Sym(ref s) = *first {
                                if s == "comma" {
                                    eval_sexp(l.get(1).unwrap().clone(), state)
                                } else {
                                    eval_commas_within_backtick(Rc::new(Sexp::List(l.clone())), state)
                                }
                            } else {
                                // TODO: remove tis duplication
                                eval_commas_within_backtick(Rc::new(Sexp::List(l.clone())), state)
                            }
                        } else {
                            eval_commas_within_backtick(Rc::new(Sexp::List(l.clone())), state)
                        }
                    } else {
                        sexp.clone()
                    }
                })
                .collect::<Vec<Rc<Sexp>>>(),
        )),
        _ => sexp.clone(),
    }
}

// fn execute_function(fnbody: Vec<Sexp>, fncall: &[Sexp], state: &mut EvalState) -> Sexp {
fn execute_function(fnbody: &Vec<Rc<Sexp>>, fncall: &Vec<Rc<Sexp>>, state: &mut EvalState) -> Rc<Sexp> {
    // ourfn is the function body. fncall is a list containing the function name, and the arguments to the function.
    // for example: ((a b) (+ a b))

    // fncall is a list containing the function name, and the arguments to the function
    // (add 2 3)

    let f = fnbody.first().unwrap().clone();

    let fnparams = match *f {
        Sexp::List(ref l) => l,
        _ => panic!("function params should be a list"),
    };

    let mut fn_param_map: HashMap<String, Rc<Sexp>> = HashMap::new();
    // Map variable names to values passed into the function
    for (index, var) in fnparams.iter().enumerate() {
        let name = if let Sexp::Sym(ref s) = **var { s } else { panic!("function param name should be a symbol") };

        fn_param_map.insert(name.to_owned(), eval_sexp(fncall.get(index + 1).unwrap().clone(), state));
    }

    state.fn_map.push(fn_param_map);

    let ret = eval_sexp(fnbody.get(1).unwrap().clone(), state);

    state.fn_map.pop();

    ret
}

pub(crate) fn eval_sexp(sexp: Rc<Sexp>, state: &mut EvalState) -> Rc<Sexp> {
    match *sexp {
        Sexp::Sym(ref s) => match get_from_fn_map_or_global(s, state) {
            Some(var) => var,
            None => panic!("symbol {s} is not defined"),
        },
        Sexp::List(ref l) => {
            match l.first() {
                None => Rc::new(Sexp::Nil), // empty list evaluates to nil
                Some(first_elem_sexp) => {
                    match **first_elem_sexp {
                        Sexp::Nil => Rc::new(Sexp::Nil),
                        Sexp::True => Rc::new(Sexp::True),
                        Sexp::Num(_) => panic!("don't know how to evaluate a list starting with a number"),
                        Sexp::Sym(ref s) => {
                            match s.as_str() {
                                "let" => {
                                    // Let statement should bind variables for the duration of the let statement.
                                    let let_param_list = l.get(1).unwrap();

                                    match **let_param_list {
                                        Sexp::List(ref l) => {
                                            let mut current_function_param_map: HashMap<String, Rc<Sexp>> =
                                                HashMap::new();

                                            for var in l {
                                                match **var {
                                                    Sexp::List(ref l) => {
                                                        match **l.first().unwrap() {
                                                            Sexp::Sym(ref let_var_name) => {
                                                                let var = l.get(1).unwrap().clone();

                                                                let var_eval = eval_sexp(var, state);

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
                                                let r = eval_sexp(statement.clone(), state);

                                                state.fn_map.pop();

                                                return r;
                                            } else {
                                                eval_sexp(statement.clone(), state);
                                            }
                                        }
                                    } else {
                                        state.fn_map.pop();
                                        return Rc::new(Sexp::Nil);
                                    }
                                }
                                "cdr" => {
                                    assert_eq!(l.len(), 2);
                                    let cdr = eval_sexp(l.get(1).unwrap().clone(), state);

                                    return match *cdr {
                                        Sexp::List(ref l) => {
                                            let mut c = l.clone();

                                            c.remove(0);
                                            Rc::new(Sexp::List(c))
                                        }
                                        _ => panic!("cdr needs a list, but found {}", cdr),
                                    };
                                }
                                "cons" => {
                                    assert_eq!(l.len(), 3);
                                    let first = eval_sexp(l.get(1).unwrap().clone(), state);

                                    let second = eval_sexp(l.get(2).unwrap().clone(), state);

                                    match *second {
                                        Sexp::List(ref l) => {
                                            let mut c = l.clone();
                                            c.insert(0, first);
                                            return Rc::new(Sexp::List(c));
                                        }
                                        _ => return Rc::new(Sexp::List(vec![first, second])),
                                    }
                                }
                                "list" => {
                                    if l.len() == 1 {
                                        return Rc::new(Sexp::Nil);
                                    } else {
                                        // let sexp = sexp.clone();

                                        return Rc::new(Sexp::List(
                                            l.iter().skip(1).map(|sexp| eval_sexp(sexp.clone(), state)).collect(),
                                        ));
                                    }
                                }
                                "car" => match *eval_sexp(l.get(1).unwrap().clone(), state) {
                                    Sexp::List(ref l) => match l.first() {
                                        Some(elem) => return elem.clone(),
                                        None => panic!("car needs a list with at least one element"),
                                    },
                                    _ => panic!("car needs a list"),
                                },
                                "quote" => {
                                    return l.get(1).unwrap().clone();
                                }
                                "backtick" => {
                                    return eval_commas_within_backtick(l.get(1).unwrap().clone(), state);
                                }
                                "comma" => {
                                    panic!("comma should only be used within backtick");
                                }
                                "eval" => {
                                    return eval_sexp(eval_sexp(l.get(1).unwrap().clone(), state), state);
                                }
                                "message" => {
                                    println!("{}", eval_sexp(l.get(1).unwrap().clone(), state));
                                    return Rc::new(Sexp::True);
                                }
                                "=" => {
                                    if eval_sexp_to_num(l.get(1).unwrap().clone(), state)
                                        == eval_sexp_to_num(l.get(2).unwrap().clone(), state)
                                    {
                                        return Rc::new(Sexp::True);
                                    } else {
                                        return Rc::new(Sexp::Nil);
                                    }
                                }
                                "equal" => {
                                    if eval_sexp(l.get(1).unwrap().clone(), state)
                                        == eval_sexp(l.get(2).unwrap().clone(), state)
                                    {
                                        return Rc::new(Sexp::True);
                                    } else {
                                        return Rc::new(Sexp::Nil);
                                    }
                                }
                                "and" => {
                                    // Eval args until one of them yields nil, then return nil.
                                    // The remaining args are not evalled at all.
                                    // If no arg yields nil, return the last arg's value.
                                    for (pos, statement) in l.iter().enumerate().skip(1) {
                                        if pos + 1 == l.len() {
                                            return eval_sexp(statement.clone(), state);
                                        } else if let Sexp::Nil = *eval_sexp(statement.clone(), state) {
                                            return Rc::new(Sexp::Nil);
                                        }
                                    }
                                }
                                "-" => {
                                    return Rc::new(Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap().clone(), state)
                                            - eval_sexp_to_num(l.get(2).unwrap().clone(), state),
                                    ));
                                }
                                "+" => {
                                    return Rc::new(Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap().clone(), state)
                                            + eval_sexp_to_num(l.get(2).unwrap().clone(), state),
                                    ));
                                }
                                "*" => {
                                    return Rc::new(Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap().clone(), state)
                                            * eval_sexp_to_num(l.get(2).unwrap().clone(), state),
                                    ));
                                }
                                "<" => {
                                    if eval_sexp_to_num(l.get(1).unwrap().clone(), state)
                                        < eval_sexp_to_num(l.get(2).unwrap().clone(), state)
                                    {
                                        return Rc::new(Sexp::True);
                                    } else {
                                        return Rc::new(Sexp::Nil);
                                    }
                                }
                                ">" => {
                                    if eval_sexp_to_num(l.get(1).unwrap().clone(), state)
                                        > eval_sexp_to_num(l.get(2).unwrap().clone(), state)
                                    {
                                        return Rc::new(Sexp::True);
                                    } else {
                                        return Rc::new(Sexp::Nil);
                                    }
                                }
                                "%" => {
                                    return Rc::new(Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap().clone(), state)
                                            % eval_sexp_to_num(l.get(2).unwrap().clone(), state),
                                    ));
                                }
                                "/" => {
                                    return Rc::new(Sexp::Num(
                                        eval_sexp_to_num(l.get(1).unwrap().clone(), state)
                                            / eval_sexp_to_num(l.get(2).unwrap().clone(), state),
                                    ));
                                }
                                "progn" => {
                                    // Skip the actual "progn", then eval all and return last
                                    for (pos, statement) in l.iter().enumerate().skip(1) {
                                        if pos + 1 == l.len() {
                                            return eval_sexp(statement.clone(), state);
                                        } else {
                                            eval_sexp(statement.clone(), state);
                                        }
                                    }
                                }
                                "setq" => match *l.get(1).unwrap().clone() {
                                    Sexp::Sym(ref s) => {
                                        let eval_value = eval_sexp(l.get(2).unwrap().clone(), state);

                                        // Let's see if the symbol exists in local scope first
                                        for fn_map in state.fn_map.iter_mut().rev() {
                                            if fn_map.get(s).is_some() {
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
                                    let macro_name = match *l.get(1).unwrap().clone() {
                                        Sexp::Sym(ref s) => s.clone(),
                                        _ => panic!("defmacro name should be a symbol"),
                                    };

                                    let macro_body: Vec<Rc<Sexp>> = l.clone().into_iter().skip(2).collect();

                                    state.macro_map.insert(macro_name.clone(), Rc::new(Sexp::List(macro_body)));

                                    return Rc::new(Sexp::Nil);
                                }
                                "if" => {
                                    // eval the conditional statement:
                                    match *eval_sexp(l.get(1).unwrap().clone(), state) {
                                        Sexp::Nil => match l.get(3) {
                                            // if false, eval the else statement
                                            Some(sexp) => return eval_sexp(sexp.clone(), state),
                                            None => {
                                                return Rc::new(Sexp::Nil);
                                            }
                                        },
                                        _ => {
                                            if let Some(else_statement) = l.get(2) {
                                                return eval_sexp(else_statement.clone(), state);
                                            } else {
                                                return Rc::new(Sexp::Nil);
                                            }
                                        }
                                    }
                                }
                                "while" => {
                                    // keep evaluating the body while the test is true
                                    let test = l.get(1).unwrap();

                                    let body = l.get(2).unwrap();

                                    loop {
                                        match *eval_sexp(test.clone(), state) {
                                            Sexp::Nil => {
                                                return Rc::new(Sexp::Nil);
                                            }
                                            _ => {
                                                eval_sexp(body.clone(), state);
                                            }
                                        }
                                    }
                                }
                                "or" => {
                                    let first = eval_sexp(l.get(1).unwrap().clone(), state);

                                    // if first evals to nil, return second
                                    if let Sexp::Nil = *first {
                                        return eval_sexp(l.get(2).unwrap().clone(), state);
                                    } else {
                                        return first;
                                    }
                                }
                                "split-symbol" => {
                                    let symbol = eval_sexp(l.get(1).unwrap().clone(), state);

                                    if let Sexp::Sym(ref s) = *symbol {
                                        let delimiter = match l.get(2) {
                                            Some(d) => {
                                                let d = d.clone();

                                                if let Sexp::Sym(ref s) = *eval_sexp(d, state) {
                                                    s.clone()
                                                } else {
                                                    panic!("split-symbol expects a symbol as a delimiter");
                                                }
                                            }
                                            None => "".to_string(),
                                        };

                                        let mut ret: Vec<Rc<Sexp>> = vec![];

                                        for split in s.split(delimiter.as_str()).filter(|c| !c.is_empty()) {
                                            if let Ok(n) = split.parse::<i64>() {
                                                ret.push(Rc::new(Sexp::Num(n)))
                                            } else {
                                                ret.push(Rc::new(Sexp::Sym(split.to_string())));
                                            }
                                        }

                                        return Rc::new(Sexp::List(ret));
                                    } else {
                                        panic!("split-symbol expects a symbol, but found: {symbol}");
                                    }
                                }
                                "concat-to-symbol" => {
                                    let mut ret = "".to_string();

                                    if let Sexp::List(ref l) = *eval_sexp(l.get(1).unwrap().clone(), state) {
                                        for sexp in l.iter() {
                                            if let Sexp::Sym(ref s) = **sexp {
                                                ret.push_str(s);
                                            } else if let Sexp::Num(ref n) = **sexp {
                                                ret.push_str(n.to_string().as_str());
                                            } else {
                                                panic!("concat-to-symbol expects a list of symbols or numbers");
                                            }
                                        }
                                    };

                                    if ret.is_empty() {
                                        // println!("ret is empty, returning nil");
                                        return Rc::new(Sexp::Nil);
                                    }

                                    return Rc::new(Sexp::Sym(ret));
                                }
                                "integerp" => {
                                    let sexp = eval_sexp(l.get(1).unwrap().clone(), state);

                                    match *sexp {
                                        Sexp::Num(_) => return Rc::new(Sexp::True),
                                        _ => return Rc::new(Sexp::Nil),
                                    }
                                }

                                "int-to-symbol" => {
                                    let sexp = eval_sexp(l.get(1).unwrap().clone(), state);

                                    match *sexp {
                                        Sexp::Num(n) => return Rc::new(Sexp::Sym(n.to_string())),
                                        _ => return sexp,
                                    }
                                }

                                _ => {}
                            }

                            // check if we have a macro with this name
                            if let Some(macros) = state.macro_map.clone().get(s).cloned() {
                                match *macros {
                                    Sexp::List(ref macro_list) => {
                                        let z = macro_list.first().unwrap();

                                        let macro_params = match **z {
                                            Sexp::List(ref l) => l,
                                            _ => panic!("macro params should be a list"),
                                        };

                                        let mut current_function_param_map: HashMap<String, Rc<Sexp>> = HashMap::new();
                                        // Map variable names to values passed into the function
                                        for (index, var) in macro_params.iter().enumerate() {
                                            if let Sexp::Sym(ref var_name) = **var {
                                                let value = l.get(index + 1).unwrap().clone();

                                                current_function_param_map.insert(var_name.to_owned(), value);
                                            } else {
                                                panic!("macro param name should be a symbol")
                                            };
                                        }

                                        state.fn_map.push(current_function_param_map);

                                        let ret = eval_sexp(macro_list.get(1).unwrap().clone(), state);

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
                                Some(var) => {
                                    let t = var.clone();
                                    match *t {
                                        Sexp::List(ref ourfn) => execute_function(ourfn, l, state),
                                        _ => var,
                                    }
                                }
                                None => panic!("unrecognized symbol: {s}"),
                            }
                        }
                        Sexp::List(_) => {
                            // We are evaluating a list, whose first element is a list
                            // This must be a function call!
                            let r = eval_sexp(first_elem_sexp.clone(), state);

                            match *r {
                                Sexp::List(ref ourfn) => execute_function(ourfn, l, state),
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
