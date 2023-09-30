use std::iter::FusedIterator;

#[derive(Eq, PartialEq, Debug)]
pub enum Token<'prog> {
    Id(&'prog str),
    Lambda,
    Dot,
    LParen,
    RParen,
    Let,
    Eq,
    In,
}

pub struct Tokenizer<'prog> {
    pub remaining: &'prog str,
}

impl<'prog> Tokenizer<'prog> {
    pub fn new(program: &'prog str) -> Self {
        Self { remaining: program }
    }

    fn skip_whitespace(&mut self) {
        self.remaining = self.remaining.trim_start()
    }
}

impl<'prog> Iterator for Tokenizer<'prog> {
    type Item = Token<'prog>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        if self.remaining.is_empty() {
            return None;
        }

        let lexeme_len = self
            .remaining
            .chars()
            .take_while(|&c| !(is_reserved(c) || c.is_whitespace()))
            .map(|c| c.len_utf8())
            .sum();

        let token = if lexeme_len == 0 {
            let ch = self
                .remaining
                .chars()
                .next()
                .expect("remaining is not empty");
            self.remaining = &self.remaining[ch.len_utf8()..];

            match ch {
                '(' => Token::LParen,
                ')' => Token::RParen,
                '.' => Token::Dot,
                '=' => Token::Eq,
                '\\' | 'λ' => Token::Lambda,
                _ => panic!("Unexpected character: {}", ch),
            }
        } else {
            let lexeme = &self.remaining[..lexeme_len];
            self.remaining = &self.remaining[lexeme_len..];

            match lexeme {
                "let" => Token::Let,
                "in" => Token::In,
                _ => Token::Id(lexeme),
            }
        };

        Some(token)
    }
}

impl FusedIterator for Tokenizer<'_> {}

fn is_reserved(ch: char) -> bool {
    ch == '(' || ch == ')' || ch == '.' || ch == '\\' || ch == 'λ' || ch == '='
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(program: &str) -> Vec<Token> {
        Tokenizer { remaining: program }.collect()
    }

    #[test]
    fn test_tokenize() {
        use Token::*;

        assert_eq!(tokenize("abc"), vec![Id("abc")]);
        assert_eq!(tokenize("("), vec![LParen]);
        assert_eq!(tokenize(")"), vec![RParen]);
        assert_eq!(tokenize("."), vec![Dot]);
        assert_eq!(tokenize("let"), vec![Let]);
        assert_eq!(tokenize("in"), vec![In]);
        assert_eq!(tokenize("\\"), vec![Lambda]);
        assert_eq!(tokenize("λ"), vec![Lambda]);
        assert_eq!(tokenize("="), vec![Eq]);

        assert_eq!(
            tokenize("abc  ( \\ )"),
            vec![Id("abc"), LParen, Lambda, RParen]
        );

        assert_eq!(
            tokenize("\\a. a \n . \t )"),
            vec![Lambda, Id("a"), Dot, Id("a"), Dot, RParen]
        );

        assert_eq!(tokenize(""), Vec::new());
    }
}
