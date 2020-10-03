#[derive(Eq, PartialEq, Debug)]
pub enum Token {
    Id(String),
    Separator(char),
    Lambda,
}


struct Tokenizer<'a> {
    remaining_text: &'a str,
}

impl<'a> Tokenizer<'a> {
    fn new(program: &'a str) -> Self {
        Tokenizer {
            remaining_text: program,
        }
    }

    fn skip_whitespace(&mut self) {
        self.remaining_text = self.remaining_text.trim_start()
    }

    fn tokenize_separator(&mut self) -> Option<Token> {
        let first_char = self.remaining_text.chars().next();
        match first_char {
            Some(ch) if is_separator(ch) => {
                self.remaining_text = self.remaining_text.char_indices()
                    .nth(1)
                    .and_then(|(i, _)| self.remaining_text.get(i..))
                    .unwrap_or("");
                Some(Token::Separator(ch))
            }
            _ => None
        }
    }

    fn tokenize_id_or_lambda(&mut self) -> Option<Token> {
        if self.remaining_text.is_empty() {
            return None;
        }

        let mut end_index= self.remaining_text.len();
        for (index, ch) in self.remaining_text.char_indices() {
            if is_separator(ch) || ch.is_whitespace() {
                end_index = index;
                break;
            }
        }

        let lexeme = &self.remaining_text[..end_index];

        self.remaining_text = &self.remaining_text.get(end_index..)
            .unwrap_or("");

        match lexeme {
            "fn" => Some(Token::Lambda),
            _ => Some(Token::Id(lexeme.to_owned())),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        self.tokenize_separator()
            .or_else(|| self.tokenize_id_or_lambda())
    }
}

fn is_separator(ch: char) -> bool {
    ch == '(' || ch == ')' || ch == '.'
}


pub fn tokenize(program: String) -> Vec<Token> {
    Tokenizer::new(program.as_str()).collect()
}

#[test]
fn test_tokenize_separator() {
    let mut tokenizer = Tokenizer::new("(");
    assert_eq!(tokenizer.tokenize_separator(), Some(Token::Separator('(')));

    tokenizer = Tokenizer::new(")");
    assert_eq!(tokenizer.tokenize_separator(), Some(Token::Separator(')')));

    tokenizer = Tokenizer::new(".");
    assert_eq!(tokenizer.tokenize_separator(), Some(Token::Separator('.')));

    tokenizer = Tokenizer::new(".abc.");
    assert_eq!(tokenizer.tokenize_separator(), Some(Token::Separator('.')));
}

#[test]
fn test_tokenize_id_or_lambda() {
    let mut tokenizer = Tokenizer::new("abc");
    assert_eq!(tokenizer.tokenize_id_or_lambda(), Some(Token::Id("abc".to_owned())));

    tokenizer = Tokenizer::new("fn");
    assert_eq!(tokenizer.tokenize_id_or_lambda(), Some(Token::Lambda));

    tokenizer = Tokenizer::new("fna");
    assert_eq!(tokenizer.tokenize_id_or_lambda(), Some(Token::Id("fna".to_owned())));

    tokenizer = Tokenizer::new("abc(de)");
    assert_eq!(tokenizer.tokenize_id_or_lambda(), Some(Token::Id("abc".to_owned())));
}

#[test]
fn test_tokenize() {
    assert_eq!(tokenize("abc".to_owned()), vec![Token::Id("abc".to_owned())]);
    assert_eq!(tokenize("(".to_owned()), vec![Token::Separator('(')]);
    assert_eq!(tokenize(")".to_owned()), vec![Token::Separator(')')]);
    assert_eq!(tokenize(".".to_owned()), vec![Token::Separator('.')]);
    assert_eq!(tokenize("fn".to_owned()), vec![Token::Lambda]);

    assert_eq!(tokenize("abc  ( fn )".to_owned()),
               vec![Token::Id("abc".to_owned()), Token::Separator('('), Token::Lambda, Token::Separator(')')]);

    assert_eq!(tokenize("fn a . a \n . \t )".to_owned()),
               vec![Token::Lambda, Token::Id("a".to_owned()), Token::Separator('.'),
                    Token::Id("a".to_owned()), Token::Separator('.'), Token::Separator(')')]);

    assert_eq!(tokenize(String::new()), Vec::new());
}