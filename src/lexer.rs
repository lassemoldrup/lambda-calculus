#[derive(Eq, PartialEq, Debug)]
pub enum Token {
    Id(String),
    Separator(char),
    Lambda,
    Macro,
}


struct Tokenizer<'a> {
    remaining_text: &'a str,
}

impl<'a> Tokenizer<'a> {
    fn skip_whitespace(&mut self) {
        self.remaining_text = self.remaining_text.trim_start()
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        if self.remaining_text.is_empty() {
            return None;
        }

        let mut lexeme_len = self.remaining_text.chars()
            .take_while(|&c| !(is_reserved(c) || c.is_whitespace()))
            .count();
        if lexeme_len == 0 {
            lexeme_len += 1;
        }
        let lexeme = &self.remaining_text[..lexeme_len];
        self.remaining_text = &self.remaining_text[lexeme_len..];

        Some(tokenize_lexeme(lexeme))
    }
}

fn is_reserved(ch: char) -> bool {
    ch == '(' || ch == ')' || ch == '.' || ch == ';' || ch == '\\' || ch == '='
}

fn tokenize_lexeme(lexeme: &str) -> Token {
    match lexeme {
        "(" | ")" | "." | ";" => Token::Separator(lexeme.chars().next().unwrap()),
        "\\" => Token::Lambda,
        "=" => Token::Macro,
        _ => Token::Id(lexeme.to_owned()),
    }
}

pub fn tokenize(program: &str) -> Vec<Token> {
    Tokenizer {
        remaining_text: program
    }.collect()
}

#[test]
fn test_tokenize() {
    assert_eq!(tokenize("abc"), vec![Token::Id("abc".to_owned())]);
    assert_eq!(tokenize("("), vec![Token::Separator('(')]);
    assert_eq!(tokenize(")"), vec![Token::Separator(')')]);
    assert_eq!(tokenize("."), vec![Token::Separator('.')]);
    assert_eq!(tokenize(";"), vec![Token::Separator(';')]);
    assert_eq!(tokenize("\\"), vec![Token::Lambda]);
    assert_eq!(tokenize("="), vec![Token::Macro]);

    assert_eq!(tokenize("abc  ( \\ )"),
               vec![Token::Id("abc".to_owned()), Token::Separator('('), Token::Lambda, Token::Separator(')')]);

    assert_eq!(tokenize("\\ a . a \n . \t )"),
               vec![Token::Lambda, Token::Id("a".to_owned()), Token::Separator('.'),
                    Token::Id("a".to_owned()), Token::Separator('.'), Token::Separator(')')]);

    assert_eq!(tokenize(""), Vec::new());
}