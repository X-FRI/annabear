use std::rc::Rc;

type ParseResult<T> = Result<T, String>;

struct Parser<T>(Box<dyn Fn(String) -> ParseResult<(T, String)>>);

fn char_parser(to_match: char) -> Parser<char> {
    let to_match = Rc::new(to_match);

    Parser::<char>(Box::new(move |str: String| match &str.chars().nth(0) {
        Some(first) => {
            if first.eq(to_match.as_ref()) {
                Ok((first.to_owned(), String::from(&str[1..])))
            } else {
                Err(format!("Expecting '{}'. Got '{}'", to_match, first))
            }
        }
        None => Err("No more input".to_string()),
    }))
}

impl<T: 'static> Parser<T> {
    fn run(self, str: String) -> ParseResult<(T, String)> {
        let Parser(parser) = &self;
        parser(str.clone())
    }

    fn and_then(self, then: Parser<T>) -> Parser<(T, T)> {
        Parser::<(T, T)>(Box::new(move |str: String| {
            let Parser(parser1) = &self;
            let Parser(parser2) = &then;
            match parser1(str.clone()) {
                Ok((matched1, remaning)) => match parser2(remaning.clone()) {
                    Ok((matched2, remaning)) => Ok(((matched1, matched2), remaning)),
                    Err(e) => Err(e),
                },
                Err(e) => Err(e),
            }
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::char_parser;

    #[test]
    fn parse_char() {
        let to_match = 'a';
        let result = char_parser('a').run("abc".to_string());
        assert_eq!(result, Ok((to_match, "bc".to_string())))
    }

    #[test]
    fn and_then() {
        let result = char_parser('a')
            .and_then(char_parser('b'))
            .run("abc".to_string());
        assert_eq!(result, Ok((('a', 'b'), "c".to_string())))
    }
}
