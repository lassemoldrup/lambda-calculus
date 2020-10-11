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
    fn new(program: &'a str) -> Self {
        Tokenizer {
            remaining_text: program,
        }
    }

    fn skip_whitespace(&mut self) {
        self.remaining_text = self.remaining_text.trim_start()
    }

    fn tokenize_char(&mut self) -> Option<Token> {
        let first_char = self.remaining_text.chars().next();
        match first_char {
            Some(ch) if is_separator(ch) => {
                self.skip_char();
                Some(Token::Separator(ch))
            },
            Some('\\') => {
                self.skip_char();
                Some(Token::Lambda)
            },
            Some('=') => {
                self.skip_char();
                Some(Token::Macro)
            },
            _ => None
        }
    }

    fn skip_char(&mut self) {
        self.remaining_text = self.remaining_text.char_indices()
            .nth(1)
            .and_then(|(i, _)| self.remaining_text.get(i..))
            .unwrap_or("");
    }

    fn tokenize_id(&mut self) -> Option<Token> {
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

        Some(Token::Id(lexeme.to_owned()))
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        self.tokenize_char()
            .or_else(|| self.tokenize_id())
    }
}

fn is_separator(ch: char) -> bool {
    ch == '(' || ch == ')' || ch == '.' || ch == ';'
}


pub fn tokenize(program: &str) -> Vec<Token> {
    Tokenizer::new(program).collect()
}

#[test]
fn test_tokenize_char() {
    let mut tokenizer = Tokenizer::new("(");
    assert_eq!(tokenizer.tokenize_char(), Some(Token::Separator('(')));

    tokenizer = Tokenizer::new(")");
    assert_eq!(tokenizer.tokenize_char(), Some(Token::Separator(')')));

    tokenizer = Tokenizer::new(".");
    assert_eq!(tokenizer.tokenize_char(), Some(Token::Separator('.')));

    tokenizer = Tokenizer::new(";");
    assert_eq!(tokenizer.tokenize_char(), Some(Token::Separator(';')));

    tokenizer = Tokenizer::new("\\");
    assert_eq!(tokenizer.tokenize_char(), Some(Token::Lambda));

    tokenizer = Tokenizer::new("=");
    assert_eq!(tokenizer.tokenize_char(), Some(Token::Macro));

    tokenizer = Tokenizer::new("\\x.x");
    assert_eq!(tokenizer.tokenize_char(), Some(Token::Lambda));

    tokenizer = Tokenizer::new(".abc.");
    assert_eq!(tokenizer.tokenize_char(), Some(Token::Separator('.')));
}

#[test]
fn test_tokenize_id() {
    let mut tokenizer = Tokenizer::new("abc");
    assert_eq!(tokenizer.tokenize_id(), Some(Token::Id("abc".to_owned())));

    tokenizer = Tokenizer::new("abc(de)");
    assert_eq!(tokenizer.tokenize_id(), Some(Token::Id("abc".to_owned())));
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