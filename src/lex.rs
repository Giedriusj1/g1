#[derive(Debug, Clone)]
pub(crate) enum Token {
    LeftParen,
    RightParen,
    Apostrophe,
    Backtick,
    Comma,
    Digit(i64),
    String(String),
}

fn extract_single_char(mut chars: std::str::Chars, c: char, token_type: Token) -> Option<(Token, usize)> {
    let mut count = 0;

    loop {
        count += 1;

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

    None
}

fn extract_alphanumeric_symbol(mut chars: std::str::Chars) -> Option<(Token, usize)> {
    let mut count = 0;

    let mut letters = String::new();

    loop {
        count += 1;

        match chars.next() {
            Some(char) => {
                if char == ' ' || char == '\t' {
                    if letters.is_empty() {
                        continue;
                    } else {
                        break;
                    }
                }

                if char.is_alphanumeric()
                    || char == '-'
                    || char == '+'
                    || char == '<'
                    || char == '>'
                    || char == '='
                    || char == '*'
                    || char == '%'
                    || char == '/'
                {
                    letters.push(char);
                } else {
                    count -= 1;
                    break;
                }
            }

            None => break,
        }
    }

    if letters.is_empty() {
        None
    } else {
        match letters.parse::<i64>() {
            Ok(n) => Some((Token::Digit(n), count)),
            // must contain letters, so it's a string
            Err(_) => Some((Token::String(letters), count)),
        }
    }
}

#[allow(clippy::type_complexity)]
pub(crate) fn extract_token(chars: std::str::Chars) -> Option<(Token, usize)> {
    let match_functions: Vec<fn(chars: std::str::Chars) -> Option<(Token, usize)>> = vec![
        move |x| extract_single_char(x, '(', Token::LeftParen),
        move |x| extract_single_char(x, ')', Token::RightParen),
        move |x| extract_single_char(x, '\'', Token::Apostrophe),
        move |x| extract_single_char(x, '`', Token::Backtick),
        move |x| extract_single_char(x, ',', Token::Comma),
        extract_alphanumeric_symbol,
    ];

    for match_fn in match_functions {
        let ret = match_fn(chars.clone());
        match ret {
            Some(_) => return ret,
            None => continue,
        }
    }

    None
}

pub(crate) fn extract_tokens(stra: String) -> Vec<Token> {
    let str = stra.replace('\n', " ");
    let str = str.replace('\t', " ");

    let mut tokens: Vec<Token> = vec![];

    let mut start_chars = str.chars();

    while let Some((token, size)) = extract_token(start_chars.clone()) {
        tokens.push(token);

        for _ in 0..size {
            start_chars.next();
        }
    }

    tokens
}
