use crate::sexp::{IntrinsicInstruction, Sexp};

use std::collections::BTreeMap;
use std::rc::Rc;

#[derive(Clone)]
pub(crate) struct EvalState {
    macro_map: BTreeMap<String, Rc<Sexp>>,
    global_map: BTreeMap<String, Rc<Sexp>>,
    fn_map: Vec<BTreeMap<String, Rc<Sexp>>>,
}

impl EvalState {
    pub(crate) fn new() -> EvalState {
        EvalState { macro_map: BTreeMap::new(), global_map: BTreeMap::new(), fn_map: vec![] }
    }
}

fn eval_sexp_to_num(sexp: Rc<Sexp>, state: &mut EvalState) -> i64 {
    let res = eval_sexp(sexp, state);
    match *res {
        Sexp::Num(n) => n,
        _ => panic!("expected a number, but found: {res}"),
    }
}

fn get_from_fn_map_or_global(var_name: &str, state: &EvalState) -> Option<Rc<Sexp>> {
    // Check function scopes first for variable shadowing
    for var_map in state.fn_map.iter().rev() {
        if let Some(sexp) = var_map.get(var_name) {
            return Some(Rc::clone(sexp));
        }
    }

    // Check global scope last
    state.global_map.get(var_name).cloned()
}

fn eval_commas_within_backtick(sexp: Rc<Sexp>, state: &mut EvalState) -> Rc<Sexp> {
    match *sexp {
        Sexp::List(ref l) => {
            let mut result = Vec::with_capacity(l.len());

            for sexp in l {
                if let Sexp::List(ref inner_list) = **sexp {
                    if let Some(first) = inner_list.first() {
                        if let Sexp::Sym(ref s) = **first {
                            if s == "comma" {
                                result.push(eval_sexp(inner_list[1].clone(), state));
                                continue;
                            }
                        }
                        result.push(eval_commas_within_backtick(Rc::new(Sexp::List(inner_list.clone())), state));
                    } else {
                        result.push(eval_commas_within_backtick(Rc::new(Sexp::List(inner_list.clone())), state));
                    }
                } else {
                    result.push(Rc::clone(sexp));
                }
            }

            Rc::new(Sexp::List(result))
        }
        _ => Rc::clone(&sexp),
    }
}

fn execute_function(fnbody: &[Rc<Sexp>], fncall: &[Rc<Sexp>], state: &mut EvalState) -> Rc<Sexp> {
    let fnparams = match *fnbody[0] {
        Sexp::List(ref l) => l,
        _ => panic!("function params should be a list"),
    };

    let mut fn_param_map = BTreeMap::new();

    // Map params to evaluated args
    for (index, var) in fnparams.iter().enumerate() {
        let name = match **var {
            Sexp::Sym(ref s) => s,
            _ => panic!("function param name should be a symbol"),
        };

        fn_param_map.insert(name.to_owned(), eval_sexp(fncall[index + 1].clone(), state));
    }

    state.fn_map.push(fn_param_map);
    let ret = eval_sexp(fnbody[1].clone(), state);
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
            if l.is_empty() {
                return Rc::new(Sexp::Nil); // empty list evaluates to nil
            }

            let first_elem_sexp = &l[0];
            match **first_elem_sexp {
                Sexp::Nil => Rc::new(Sexp::Nil),
                Sexp::True => Rc::new(Sexp::True),
                Sexp::Num(_) => panic!("don't know how to evaluate a list starting with a number"),

                Sexp::Instrinsics(IntrinsicInstruction::Let) => {
                    // Let statement should bind variables for the duration of the let statement.
                    let let_param_list = l.get(1).unwrap();

                    match **let_param_list {
                        Sexp::List(ref params) => {
                            let mut current_function_param_map = BTreeMap::new();

                            for var in params {
                                if let Sexp::List(ref binding) = **var {
                                    if let Sexp::Sym(ref let_var_name) = *binding[0] {
                                        let var_eval = eval_sexp(binding[1].clone(), state);
                                        current_function_param_map.insert(let_var_name.to_owned(), var_eval);
                                    } else {
                                        panic!("let is missing variable name");
                                    }
                                } else {
                                    panic!("let expects a list, but found");
                                }
                            }

                            state.fn_map.push(current_function_param_map);
                        }
                        _ => {
                            panic!("let expects a param list as a first argument, but found {}", let_param_list);
                        }
                    }

                    // Skip the actual "let", and the subsequent paramlist,
                    // then eval all remaining and return last
                    let statements = &l[2..];
                    let len = statements.len();

                    if len > 0 {
                        let mut result = Rc::new(Sexp::Nil);
                        for (i, statement) in statements.iter().enumerate() {
                            result = eval_sexp(statement.clone(), state);
                            if i < len - 1 {
                                // Only continue evaluating if not the last statement
                                continue;
                            }
                        }
                        state.fn_map.pop();
                        return result;
                    } else {
                        state.fn_map.pop();
                        Rc::new(Sexp::Nil)
                    }
                }

                Sexp::Instrinsics(IntrinsicInstruction::Cdr) => {
                    assert_eq!(l.len(), 2);
                    let cdr = eval_sexp(l[1].clone(), state);

                    match *cdr {
                        Sexp::List(ref list) => {
                            if list.is_empty() {
                                Rc::new(Sexp::List(Vec::new()))
                            } else {
                                let mut c = Vec::with_capacity(list.len() - 1);
                                c.extend_from_slice(&list[1..]);
                                Rc::new(Sexp::List(c))
                            }
                        }
                        _ => panic!("cdr needs a list, but found {}", cdr),
                    }
                }

                Sexp::Instrinsics(IntrinsicInstruction::Cons) => {
                    assert_eq!(l.len(), 3);
                    let first = eval_sexp(l[1].clone(), state);
                    let second = eval_sexp(l[2].clone(), state);

                    match *second {
                        Sexp::List(ref list) => {
                            let mut c = Vec::with_capacity(list.len() + 1);
                            c.push(first);
                            c.extend_from_slice(list);
                            Rc::new(Sexp::List(c))
                        }
                        _ => Rc::new(Sexp::List(vec![first, second])),
                    }
                }

                Sexp::Instrinsics(IntrinsicInstruction::List) => {
                    if l.len() == 1 {
                        Rc::new(Sexp::Nil)
                    } else {
                        let mut result = Vec::with_capacity(l.len() - 1);
                        for sexp in l.iter().skip(1) {
                            result.push(eval_sexp(sexp.clone(), state));
                        }
                        Rc::new(Sexp::List(result))
                    }
                }
                Sexp::Instrinsics(IntrinsicInstruction::Car) => match *eval_sexp(l[1].clone(), state) {
                    Sexp::List(ref list) => match list.first() {
                        Some(elem) => Rc::clone(elem),
                        None => panic!("car needs a list with at least one element"),
                    },
                    _ => panic!("car needs a list"),
                },
                Sexp::Instrinsics(IntrinsicInstruction::Eval) => eval_sexp(eval_sexp(l[1].clone(), state), state),
                Sexp::Instrinsics(IntrinsicInstruction::Message) => {
                    println!("{}", eval_sexp(l[1].clone(), state));
                    Rc::new(Sexp::True)
                }
                Sexp::Instrinsics(IntrinsicInstruction::Progn) => {
                    // Skip the actual "progn", then eval all and return last
                    let statements = &l[1..];
                    let len = statements.len();

                    if len == 0 {
                        panic!("progn should have at least 2 arguments");
                    }

                    let mut result = Rc::new(Sexp::Nil);
                    for (i, statement) in statements.iter().enumerate() {
                        result = eval_sexp(statement.clone(), state);
                        if i < len - 1 {
                            // Only continue evaluating if not the last statement
                            continue;
                        }
                    }
                    result
                }
                Sexp::Instrinsics(IntrinsicInstruction::Defmacro) => {
                    let macro_name = match *l[1] {
                        Sexp::Sym(ref s) => s.clone(),
                        _ => panic!("defmacro name should be a symbol"),
                    };

                    let macro_body: Vec<Rc<Sexp>> = l.iter().skip(2).cloned().collect();
                    state.macro_map.insert(macro_name, Rc::new(Sexp::List(macro_body)));
                    Rc::new(Sexp::Nil)
                }
                Sexp::Instrinsics(IntrinsicInstruction::If) => {
                    // eval the conditional statement:
                    match *eval_sexp(l[1].clone(), state) {
                        Sexp::Nil => match l.get(3) {
                            // if false, eval the else statement
                            Some(sexp) => eval_sexp(sexp.clone(), state),
                            None => Rc::new(Sexp::Nil),
                        },
                        _ => {
                            if let Some(then_statement) = l.get(2) {
                                eval_sexp(then_statement.clone(), state)
                            } else {
                                Rc::new(Sexp::Nil)
                            }
                        }
                    }
                }
                Sexp::Instrinsics(IntrinsicInstruction::While) => {
                    // keep evaluating the body while the test is true
                    let test = &l[1];
                    let body = &l[2];

                    loop {
                        match *eval_sexp(test.clone(), state) {
                            Sexp::Nil => return Rc::new(Sexp::Nil),
                            _ => {
                                eval_sexp(body.clone(), state);
                            }
                        }
                    }
                }

                Sexp::Instrinsics(IntrinsicInstruction::Or) => {
                    let first = eval_sexp(l[1].clone(), state);

                    // if first evals to nil, return second
                    if let Sexp::Nil = *first {
                        eval_sexp(l[2].clone(), state)
                    } else {
                        first
                    }
                }
                Sexp::Instrinsics(IntrinsicInstruction::SplitSymbol) => {
                    let symbol = eval_sexp(l[1].clone(), state);

                    if let Sexp::Sym(ref s) = *symbol {
                        let delimiter = match l.get(2) {
                            Some(d) => {
                                if let Sexp::Sym(ref s) = *eval_sexp(d.clone(), state) {
                                    s.clone()
                                } else {
                                    panic!("split-symbol expects a symbol as a delimiter");
                                }
                            }
                            None => String::new(),
                        };

                        let mut ret = Vec::new();
                        for split in s.split(&delimiter).filter(|c| !c.is_empty()) {
                            if let Ok(n) = split.parse::<i64>() {
                                ret.push(Rc::new(Sexp::Num(n)))
                            } else {
                                ret.push(Rc::new(Sexp::Sym(split.to_string())));
                            }
                        }

                        if ret.is_empty() {
                            Rc::new(Sexp::Nil)
                        } else {
                            Rc::new(Sexp::List(ret))
                        }
                    } else {
                        panic!("split-symbol expects a symbol, but found: {symbol}");
                    }
                }

                Sexp::Instrinsics(IntrinsicInstruction::Integerp) => match *eval_sexp(l[1].clone(), state) {
                    Sexp::Num(_) => Rc::new(Sexp::True),
                    _ => Rc::new(Sexp::Nil),
                },

                Sexp::Sym(ref s) => {
                    match s.as_str() {
                        "quote" => Rc::clone(&l[1]),
                        "backtick" => eval_commas_within_backtick(l[1].clone(), state),
                        "=" => {
                            if eval_sexp_to_num(l[1].clone(), state) == eval_sexp_to_num(l[2].clone(), state) {
                                Rc::new(Sexp::True)
                            } else {
                                Rc::new(Sexp::Nil)
                            }
                        }
                        "equal" => {
                            if eval_sexp(l[1].clone(), state) == eval_sexp(l[2].clone(), state) {
                                Rc::new(Sexp::True)
                            } else {
                                Rc::new(Sexp::Nil)
                            }
                        }
                        "and" => {
                            // Eval args until one of them yields nil, then return nil.
                            // The remaining args are not evalled at all.
                            // If no arg yields nil, return the last arg's value.
                            let statements = &l[1..];
                            let len = statements.len();

                            if len == 0 {
                                return Rc::new(Sexp::True); // Empty 'and' is true
                            }

                            let mut result = Rc::new(Sexp::True);
                            for statement in statements {
                                result = eval_sexp(statement.clone(), state);
                                if let Sexp::Nil = *result {
                                    return result; // Short-circuit on first nil
                                }
                            }
                            result
                        }
                        "-" => Rc::new(Sexp::Num(
                            eval_sexp_to_num(l[1].clone(), state) - eval_sexp_to_num(l[2].clone(), state),
                        )),
                        "+" => Rc::new(Sexp::Num(
                            eval_sexp_to_num(l[1].clone(), state) + eval_sexp_to_num(l[2].clone(), state),
                        )),
                        "*" => Rc::new(Sexp::Num(
                            eval_sexp_to_num(l[1].clone(), state) * eval_sexp_to_num(l[2].clone(), state),
                        )),
                        "<" => {
                            if eval_sexp_to_num(l[1].clone(), state) < eval_sexp_to_num(l[2].clone(), state) {
                                Rc::new(Sexp::True)
                            } else {
                                Rc::new(Sexp::Nil)
                            }
                        }
                        ">" => {
                            if eval_sexp_to_num(l[1].clone(), state) > eval_sexp_to_num(l[2].clone(), state) {
                                Rc::new(Sexp::True)
                            } else {
                                Rc::new(Sexp::Nil)
                            }
                        }
                        "%" => Rc::new(Sexp::Num(
                            eval_sexp_to_num(l[1].clone(), state) % eval_sexp_to_num(l[2].clone(), state),
                        )),
                        "/" => Rc::new(Sexp::Num(
                            eval_sexp_to_num(l[1].clone(), state) / eval_sexp_to_num(l[2].clone(), state),
                        )),
                        "setq" => {
                            if let Sexp::Sym(ref s) = *l[1] {
                                let eval_value = eval_sexp(l[2].clone(), state);

                                // Let's see if the symbol exists in local scope first
                                for fn_map in state.fn_map.iter_mut().rev() {
                                    if fn_map.get(s).is_some() {
                                        fn_map.insert(s.clone(), Rc::clone(&eval_value));
                                        return eval_value;
                                    }
                                }

                                state.global_map.insert(s.clone(), Rc::clone(&eval_value));
                                eval_value
                            } else {
                                panic!("setq expects a symbol, found: {}", l[1]);
                            }
                        }
                        "concat-to-symbol" => {
                            if let Sexp::List(ref list) = *eval_sexp(l[1].clone(), state) {
                                let mut ret = String::with_capacity(list.len() * 8); // Estimate capacity

                                for sexp in list {
                                    match **sexp {
                                        Sexp::Sym(ref s) => ret.push_str(s),
                                        Sexp::Num(ref n) => ret.push_str(&n.to_string()),
                                        _ => panic!("concat-to-symbol expects a list of symbols or numbers"),
                                    }
                                }

                                if ret.is_empty() {
                                    Rc::new(Sexp::Nil)
                                } else {
                                    Rc::new(Sexp::Sym(ret))
                                }
                            } else {
                                Rc::new(Sexp::Nil)
                            }
                        }
                        "int-to-symbol" => {
                            let sexp = eval_sexp(l[1].clone(), state);

                            match *sexp {
                                Sexp::Num(n) => Rc::new(Sexp::Sym(n.to_string())),
                                _ => sexp,
                            }
                        }

                        _ => {
                            // check if we have a macro with this name
                            if let Some(macros) = state.macro_map.get(s).cloned() {
                                if let Sexp::List(ref macro_list) = *macros {
                                    if let Sexp::List(ref macro_params) = *macro_list[0] {
                                        let mut current_function_param_map = BTreeMap::new();

                                        // Map variable names to values passed into the function
                                        for (index, var) in macro_params.iter().enumerate() {
                                            if let Sexp::Sym(ref var_name) = **var {
                                                let value = l[index + 1].clone();
                                                current_function_param_map.insert(var_name.to_owned(), value);
                                            } else {
                                                panic!("macro param name should be a symbol")
                                            };
                                        }

                                        state.fn_map.push(current_function_param_map);
                                        let ret = eval_sexp(macro_list[1].clone(), state);
                                        state.fn_map.pop();
                                        return ret;
                                    } else {
                                        panic!("macro params should be a list");
                                    }
                                } else {
                                    panic!("macro should be a list")
                                }
                            }

                            // Not an intrinsic, or a macro.... could it be a variable?
                            match get_from_fn_map_or_global(s, state) {
                                Some(var) => match *var {
                                    Sexp::List(ref ourfn) => execute_function(ourfn, l, state),
                                    _ => var,
                                },
                                None => panic!("unrecognized symbol: {s}"),
                            }
                        }
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
        _ => Rc::clone(&sexp), // An atom that is not a symbol can evaluate to itself
    }
}
