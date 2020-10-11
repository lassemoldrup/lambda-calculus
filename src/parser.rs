use std::fmt::{Debug, Formatter, self, Display};
use std::error::Error;
use super::lexer::*;
use Token::*;

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

pub type Result<T> = std::result::Result<T, ParseError>;


struct MacroDef(String, AstNode);


pub fn parse(tokens: &[Token]) -> Result<AstNode> {
    let mut macros = Vec::new();
    parse_macros(tokens, &mut macros)
        .and_then(|tks| parse_term(tks))
        .map(|t| apply_macros(t, macros))
}

fn parse_term(tokens: &[Token]) -> Result<AstNode> {
    use AstNode::*;

    let terms = partition_terms(tokens)?;

    if terms.is_empty() {
        return Err("Term expected".into());
    }

    if terms.len() == 1 {
        return match tokens {
            [Id(id)] =>
                Ok(Var(id.clone())),
            [Lambda, Id(id), Separator('.'), tail @ ..] =>
                Ok(Abs(id.clone(), parse_term(tail)?.into())),
            [Separator('('), mid @ .., Separator(')')] =>
                parse_term(mid),
            _ =>
                Err("Generic error".into()),
        }
    }

    // t1 t2 t3 t4 => App ( App ( App ( t1, t2 ), t3 ), t4 )
    let first = parse_term(terms.first().unwrap());
    terms.iter()
        .skip(1)
        .fold(first, |acc, t| Ok(App(acc?.into(), parse_term(t)?.into())))
}

fn partition_terms(tokens: &[Token]) -> Result<Vec<&[Token]>> {
    let mut terms = Vec::new();

    let mut rest = tokens;
    while !rest.is_empty() {
        let partition = partition_first_term(rest)?;
        terms.push(partition.0);
        rest = partition.1;
    }

    Ok(terms)
}

fn partition_first_term(tokens: &[Token]) -> Result<(&[Token], &[Token])> {
    match tokens {
        [Id(_), ..] =>
            Ok(tokens.split_at(1)),
        [Lambda, Id(_), Separator('.'), ..] =>
            Ok((tokens, &[])),
        [Separator('('), ..] =>
            partition_parens(tokens, 0),
        [tok, ..] =>
            Err(format!("Expected term, found {:?}", tok).into()),
        [] =>
            panic!("Tried to partition empty slice"),
    }
}

fn partition_parens(tokens: &[Token], depth: i32) -> Result<(&[Token], &[Token])> {
    fn inner(tokens: &[Token], index: usize, depth: i32) -> Result<usize> {
        match tokens {
            [Separator('('), tail @..] => inner(tail, index + 1, depth + 1),
            [Separator(')'), ..] if depth == 1 => Ok(index + 1),
            [Separator(')'), tail @..] => inner(tail, index + 1, depth - 1),
            [_, tail @..] => inner(tail, index + 1, depth),
            [] => Err("Missing ')'".into()),
        }
    }

    let split_index = inner(tokens, 0, depth)?;
    Ok((&tokens[..split_index], tokens.get(split_index..).unwrap_or(&[])))
}

/// Adds macro definitions to vec, returns the remaining tokens
fn parse_macros<'a>(tokens: &'a [Token], macros: &mut Vec<MacroDef>) -> Result<&'a [Token]> {
    Ok(tokens)
}

fn apply_macros(term: AstNode, macros: Vec<MacroDef>) -> AstNode {
    term
}


#[test]
fn test_parse() {
    use AstNode::*;

    assert_eq!(parse(&tokenize("abc")).unwrap(), Var("abc".to_owned()));
    assert_eq!(parse(&tokenize("(abc)")).unwrap(), Var("abc".to_owned()));
    assert_eq!(parse(&tokenize("a (abc)")).unwrap(),
               App(Box::new(Var("a".to_owned())), Box::new(Var("abc".to_owned()))));
    assert_eq!(parse(&tokenize("\\a. a")).unwrap(),
               Abs("a".to_owned(), Box::new(Var("a".to_owned()))));
    assert_eq!(parse(&tokenize("x \\a. a")).unwrap(),
               App(Box::new(Var("x".to_owned())),
                   Box::new(Abs("a".to_owned(), Box::new(Var("a".to_owned()))))));
    assert_eq!(parse(&tokenize("(\\x. a x) (\\b. b) c")).unwrap(),
               App(Box::new(App(Box::new(Abs("x".to_string(),
                                             Box::new(App(Box::new(Var("a".to_owned())),
                                                          Box::new(Var("x".to_owned())))))),
                                Box::new(Abs("b".to_owned(), Box::new(Var("b".to_owned())))))),
                   Box::new(Var("c".to_owned()))));
    assert_eq!(parse(&tokenize("\\a. \\b. a b")).unwrap(),
               Abs("a".to_owned(), Box::new(Abs("b".to_owned(),
                                                Box::new(App(Box::new(Var("a".to_owned())),
                                                             Box::new(Var("b".to_owned()))))))));

    assert!(parse(&tokenize(")")).is_err());
    assert!(parse(&tokenize("abc(")).is_err());
    assert!(parse(&tokenize("\\a a")).is_err());
    assert!(parse(&tokenize("(\\a. a() \\b. b)")).is_err());
    assert!(parse(&tokenize("(\\a. a) \\b. b c.")).is_err());
}