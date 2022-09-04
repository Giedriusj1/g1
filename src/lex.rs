#[derive(Debug)]
pub(crate) enum Token {
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Digit(i32),
    String(String),
}

fn extract_single_char(
    mut chars: std::str::Chars,
    c: char,
    token_type: Token,
) -> Option<(Token, usize)> {
    let mut count = 0;

    loop {
        count = count + 1;

        match chars.next() {
            Some(char) => {
                if char == ' ' {
                    continue;
                }

                if char == c {
                    return Some((token_type, count));
                }
            }

            None => break,
        }

        break;
    }

    return None;
}

fn extract_number(mut chars: std::str::Chars) -> Option<(Token, usize)> {
    let mut count = 0;

    let mut digits = String::new();

    loop {
        count = count + 1;

        match chars.next() {
            Some(char) => {
                if char == ' ' {
                    if digits.is_empty() {
                        continue;
                    } else {
                        break;
                    }
                }

                if char.is_numeric() {
                    digits.push(char);
                } else {
                    count = count - 1;
                    break;
                }
            }

            None => break,
        }
    }

    match digits.parse::<i32>() {
        Ok(n) => return Some((Token::Digit(n), count)),
        Err(_) => (return None),
    }
}

fn extract_string(mut chars: std::str::Chars) -> Option<(Token, usize)> {
    let mut count = 0;

    let mut letters = String::new();

    loop {
        count = count + 1;

        match chars.next() {
            Some(char) => {
                if char == ' ' {
                    if letters.is_empty() {
                        continue;
                    } else {
                        break;
                    }
                }

                if char.is_alphabetic() {
                    letters.push(char);
                } else {
                    count = count - 1;
                    break;
                }
            }

            None => break,
        }
    }

    if letters.is_empty() {
        return None;
    } else {
        Some((Token::String(String::from(letters)), count))
    }
}

pub(crate) fn extract_token(chars: std::str::Chars) -> Option<(Token, usize)> {
    let match_functions: Vec<fn(chars: std::str::Chars) -> Option<(Token, usize)>> = vec![
        move |x| extract_single_char(x, '(', Token::LeftParen),
        move |x| extract_single_char(x, ')', Token::RightParen),
        move |x| extract_single_char(x, '+', Token::Plus),
        move |x| extract_single_char(x, '-', Token::Minus),
        extract_number,
        extract_string,
    ];

    for match_fn in match_functions {
        let ret = match_fn(chars.clone());
        match ret {
            Some(_) => return ret,
            None => continue,
        }
    }

    return None;
}

pub(crate) fn extract_tokens(stra: String) -> Vec<Token> {
    let str = stra.replace("\n", " ");

    let mut tokens: Vec<Token> = vec![];

    let mut start_chars = str.chars();
    loop {
        match extract_token(start_chars.clone()) {
            Some((token, size)) => {
                tokens.push(token);

                for _ in 0..size {
                    start_chars.next();
                }
            }
            None => break,
        }
    }

    tokens
}
