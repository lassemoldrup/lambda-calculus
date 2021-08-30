use std::borrow::Cow;
use std::fmt::{Debug, Formatter, self, Display};
use std::error::Error;
use super::lexer::*;
use Token::*;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum AstNode<'a> {
    Var(Cow<'a, str>),
    Abs(Cow<'a, str>, Box<AstNode<'a>>),
    App(Box<AstNode<'a>>, Box<AstNode<'a>>),
}

impl<'a> Display for AstNode<'a> {
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


struct MacroDef<'a>(&'a str, AstNode<'a>);


pub fn parse<'a>(tokens: &[Token<'a>]) -> Result<AstNode<'a>> {
    let mut macros = Vec::new();
    parse_macros(tokens, &mut macros)
        .and_then(|tks| parse_term(tks))
        .map(|t| apply_macros(t, macros))
}

fn parse_term<'a>(tokens: &[Token<'a>]) -> Result<AstNode<'a>> {
    use AstNode::*;

    let terms = partition_terms(tokens)?;

    if terms.is_empty() {
        return Err("Term expected".into());
    }

    if terms.len() == 1 {
        return match tokens {
            &[Id(id)] =>
                Ok(Var(Cow::from(id))),
            &[Lambda, Id(id), Separator('.'), ref tail @ ..] =>
                Ok(Abs(Cow::from(id), parse_term(tail)?.into())),
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

fn partition_terms<'a, 'b>(tokens: &'a [Token<'b>]) -> Result<Vec<&'a [Token<'b>]>> {
    let mut terms = Vec::new();

    let mut rest = tokens;
    while !rest.is_empty() {
        let partition = partition_first_term(rest)?;
        terms.push(partition.0);
        rest = partition.1;
    }

    Ok(terms)
}

fn partition_first_term<'a, 'b>(tokens: &'a [Token<'b>]) -> Result<(&'a [Token<'b>], &'a [Token<'b>])> {
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

fn partition_parens<'a, 'b>(tokens: &'a [Token<'b>], depth: i32) -> Result<(&'a [Token<'b>], &'a [Token<'b>])> {
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
fn parse_macros<'a, 'b>(tokens: &'a [Token<'b>], macros: &mut Vec<MacroDef<'b>>) -> Result<&'a [Token<'b>]> {
    let macro_split = tokens.split(|tok| *tok == Separator(';'))
        .collect::<Vec<_>>();
    let (remaining, macro_defs) = macro_split.split_last().unwrap();

    for macro_def in macro_defs {
        match macro_def {
            [Id(id), Macro, tail @..] =>
                macros.push(MacroDef(id, parse_term(tail)?)),
            _ =>
                return Err("Invalid macro definition".into()),
        }
    }

    Ok(remaining)
}

fn apply_macros<'a>(mut term: AstNode<'a>, mut macros: Vec<MacroDef<'a>>) -> AstNode<'a> {
    for i in 0..macros.len() {
        for j in i+1..macros.len() {
            macros[j].1 = apply_macro(macros[j].1.clone(), &macros[i]);
        }
        term = apply_macro(term, &macros[i]);
    }

    term
}

fn apply_macro<'a>(term: AstNode<'a>, macro_def: &MacroDef<'a>) -> AstNode<'a> {
    term.substitute(&macro_def.0, &macro_def.1)
}


#[test]
fn test_parse() {
    use AstNode::*;

    assert_eq!(parse(&tokenize("abc")).unwrap(), Var("abc".into()));
    assert_eq!(parse(&tokenize("(abc)")).unwrap(), Var("abc".into()));
    assert_eq!(parse(&tokenize("a (abc)")).unwrap(),
               App(Box::new(Var("a".into())), Box::new(Var("abc".into()))));
    assert_eq!(parse(&tokenize("\\a. a")).unwrap(),
               Abs("a".into(), Box::new(Var("a".into()))));
    assert_eq!(parse(&tokenize("x \\a. a")).unwrap(),
               App(Box::new(Var("x".into())),
                   Box::new(Abs("a".into(), Box::new(Var("a".into()))))));
    assert_eq!(parse(&tokenize("(\\x. a x) (\\b. b) c")).unwrap(),
               App(Box::new(App(Box::new(Abs("x".into(),
                                             Box::new(App(Box::new(Var("a".into())),
                                                          Box::new(Var("x".into())))))),
                                Box::new(Abs("b".into(), Box::new(Var("b".into())))))),
                   Box::new(Var("c".into()))));
    assert_eq!(parse(&tokenize("\\a. \\b. a b")).unwrap(),
               Abs("a".into(), Box::new(Abs("b".into(),
                                                Box::new(App(Box::new(Var("a".into())),
                                                             Box::new(Var("b".into()))))))));

    assert_eq!(parse(&tokenize("ID = \\x.x; a b")).unwrap(),
               App(Var("a".into()).into(), Var("b".into()).into()));
    assert_eq!(parse(&tokenize("ID = \\x.x; ID b")).unwrap(),
               App(Abs("x".into(), Var("x".into()).into()).into(), Var("b".into()).into()));
    assert_eq!(parse(&tokenize("TRU = \\t.\\f.t; TWO = TRU 2; TWO")).unwrap(),
               App(Abs("t".into(), Abs("f".into(), Var("t".into()).into()).into()).into(),
                   Var("2".into()).into()));

    assert!(parse(&tokenize(")")).is_err());
    assert!(parse(&tokenize("abc(")).is_err());
    assert!(parse(&tokenize("\\a a")).is_err());
    assert!(parse(&tokenize("(\\a. a() \\b. b)")).is_err());
    assert!(parse(&tokenize("(\\a. a) \\b. b c.")).is_err());
    assert!(parse(&tokenize("\\x.x;")).is_err());
}