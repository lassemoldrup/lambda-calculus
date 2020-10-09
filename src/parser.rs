use std::fmt::{Debug, Formatter, self, Display};
use std::error::Error;
use super::lexer::*;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum AstNode {
    Var(String),
    Abs(String, Box<AstNode>),
    App(Box<AstNode>, Box<AstNode>),
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AstNode::Var(x) => write!(f, "{}", x),
            AstNode::Abs(x, t) => write!(f, "λ{}.{}", x, t),
            AstNode::App(t1, t2) => {
                match &**t1 {
                    AstNode::Var(_) |
                    AstNode::App(_, _) => write!(f, "{} ", t1)?,
                    AstNode::Abs(_, _) => write!(f, "({}) ", t1)?,
                }
                match &**t2 {
                    AstNode::Var(_) => write!(f, "{}", t2),
                    AstNode::App(_, _) |
                    AstNode::Abs(_, _) => write!(f, "({})", t2),
                }
            }
        }
    }
}


pub struct ParseError(String);

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Parse error: {}", self.0)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

impl Error for ParseError { }

impl From<&str> for ParseError {
    fn from(s: &str) -> Self {
        ParseError(s.to_owned())
    }
}

impl From<String> for ParseError {
    fn from(s: String) -> Self {
        ParseError(s)
    }
}

type Result<T> = std::result::Result<T, ParseError>;


pub fn parse(tokens: &[Token]) -> Result<AstNode> {
    use Token::*;
    use AstNode::*;

    match tokens {
        [] => Err("Term expected".into()),
        [Id(id)] => Ok(Var(id.clone())),
        [Separator('('), mid @.., Separator(')')] if is_correctly_parenthesized(mid) =>
            parse(mid),
        [Id(_), ..] | [Separator('('), ..] => {
            let (first, last) = partition_at_last_term(tokens)?;
            Ok(App(Box::new(parse(first)?), Box::new(parse(last)?)))
        },
        [Lambda, Id(id), Separator('.'), tail @..] =>
            Ok(Abs(id.clone(), Box::new(parse(tail)?))),
        [tok] => Err(format!("Term expected, found {:?}", tok).into()),
        _ => Err("Generic error message".into()),
    }
}

fn is_correctly_parenthesized(tokens: &[Token]) -> bool {
    use Token::*;

    let mut paren_depth = 0;
    tokens.iter().all(|tok| {
        match tok {
            Separator('(') => paren_depth += 1,
            Separator(')') => paren_depth -= 1,
            _ => {}
        }
        paren_depth >= 0
    }) && paren_depth == 0
}

fn partition_at_last_term(tokens: &[Token]) -> Result<(&[Token], &[Token])> {
    use Token::*;

    // Check if last term is unparenthesized lambda
    let mut paren_depth = 0;
    for (i, tok) in tokens.iter().enumerate() {
        match tok {
            Separator('(') => paren_depth += 1,
            Separator(')') => {
                paren_depth -= 1;
                if paren_depth < 0 {
                    return Err(format!("Unexpected character {:?}", tok).into())
                }
            }
            Lambda if paren_depth == 0 => return Ok((&tokens[..i], &tokens[i..])),
            _ => {}
        }
    }

    match tokens {
        [.., Id(_)] => Ok((&tokens[..tokens.len()-1], &tokens[tokens.len()-1..])),
        [.., Separator(')')] => {
            let mut paren_depth = 0;
            for (i, tok) in tokens.iter().enumerate().rev() {
                match tok {
                    Separator('(') => {
                        paren_depth += 1;
                        match paren_depth {
                            0 => return Ok((&tokens[..i], &tokens[i..])),
                            1 => return Err(format!("Unexpected character {:?}", Separator(')')).into()),
                            _ => {},
                        }
                    },
                    Separator(')') => paren_depth -= 1,
                    _ => {},
                }
            }
            Err(format!("Unexpected character {:?}", Separator(')')).into())
        },
        [_] => panic!("Tried to partition single token"),
        [] => panic!("Tried to partition empty token list"),
        [.., tok] => Err(format!("Unexpected token {:?}", tok).into())
    }
}


#[test]
fn test_parse() {
    use AstNode::*;

    assert_eq!(parse(&tokenize("abc")).unwrap(), Var("abc".to_owned()));
    assert_eq!(parse(&tokenize("(abc)")).unwrap(), Var("abc".to_owned()));
    assert_eq!(parse(&tokenize("a (abc)")).unwrap(),
               App(Box::new(Var("a".to_owned())), Box::new(Var("abc".to_owned()))));
    assert_eq!(parse(&tokenize("fn a. a")).unwrap(),
               Abs("a".to_owned(), Box::new(Var("a".to_owned()))));
    assert_eq!(parse(&tokenize("x fn a. a")).unwrap(),
               App(Box::new(Var("x".to_owned())),
                   Box::new(Abs("a".to_owned(), Box::new(Var("a".to_owned()))))));
    assert_eq!(parse(&tokenize("(fn x. a x) (fn b. b) c")).unwrap(),
               App(Box::new(App(Box::new(Abs("x".to_string(),
                                             Box::new(App(Box::new(Var("a".to_owned())),
                                                          Box::new(Var("x".to_owned())))))),
                                Box::new(Abs("b".to_owned(), Box::new(Var("b".to_owned())))))),
                   Box::new(Var("c".to_owned()))));
    assert_eq!(parse(&tokenize("fn a. fn b. a b")).unwrap(),
               Abs("a".to_owned(), Box::new(Abs("b".to_owned(),
                                                Box::new(App(Box::new(Var("a".to_owned())),
                                                             Box::new(Var("b".to_owned()))))))));

    assert!(parse(&tokenize(")")).is_err());
    assert!(parse(&tokenize("abc(")).is_err());
    assert!(parse(&tokenize("fn a a")).is_err());
    assert!(parse(&tokenize("(fn a. a() fn b. b)")).is_err());
    assert!(parse(&tokenize("(fn a. a) fn b. b c.")).is_err());
}