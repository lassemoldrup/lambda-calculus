use std::iter::FusedIterator;

#[derive(Eq, PartialEq, Debug)]
pub enum Token<'id> {
    Id(&'id str),
    Lambda,
    Dot,
    LParen,
    RParen,
    Let,
    Eq,
    In,
}

pub fn tokenize(program: &str) -> Vec<Token> {
    Tokenizer { remaining: program }.collect()
}

pub struct Tokenizer<'a> {
    remaining: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(program: &'a str) -> Self {
        Self { remaining: program }
    }

    fn skip_whitespace(&mut self) {
        self.remaining = self.remaining.trim_start()
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

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

#[test]
fn test_tokenize() {
    use Token as T;

    assert_eq!(tokenize("abc"), vec![T::Id("abc")]);
    assert_eq!(tokenize("("), vec![T::LParen]);
    assert_eq!(tokenize(")"), vec![T::RParen]);
    assert_eq!(tokenize("."), vec![T::Dot]);
    assert_eq!(tokenize("let"), vec![T::Let]);
    assert_eq!(tokenize("in"), vec![T::In]);
    assert_eq!(tokenize("\\"), vec![T::Lambda]);
    assert_eq!(tokenize("λ"), vec![T::Lambda]);
    assert_eq!(tokenize("="), vec![T::Eq]);

    assert_eq!(
        tokenize("abc  ( \\ )"),
        vec![T::Id("abc"), T::LParen, T::Lambda, T::RParen]
    );

    assert_eq!(
        tokenize("\\a. a \n . \t )"),
        vec![T::Lambda, T::Id("a"), T::Dot, T::Id("a"), T::Dot, T::RParen]
    );

    assert_eq!(tokenize(""), Vec::new());
}
