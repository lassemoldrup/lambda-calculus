use hashbrown::HashMap;

use super::lexer::*;
use std::error::Error;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::Index;
use std::rc::Rc;

type Ident = usize;

#[derive(Default)]
struct Idents<'id> {
    num_to_string: Vec<&'id str>,
    string_to_num: HashMap<&'id str, Ident>,
}

impl<'id> Idents<'id> {
    fn new() -> Self {
        Self::default()
    }

    fn get_ident(&mut self, s: &'id str) -> Ident {
        let string_to_num = &mut self.string_to_num;
        let num_to_string = &mut self.num_to_string;
        *string_to_num.entry(s).or_insert_with(|| {
            num_to_string.push(s);
            num_to_string.len() - 1
        })
    }
}

impl<'id> Index<usize> for Idents<'id> {
    type Output = &'id str;

    fn index(&self, index: usize) -> &Self::Output {
        &self.num_to_string[index]
    }
}

type MacroMap = HashMap<usize, AstNode>;

pub struct Ast<'id> {
    root: AstNode,
    idents: Idents<'id>,
    macros: MacroMap,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum AstNode {
    Var(Ident),
    Abs(Ident, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
}

impl AstNode {
    fn display<'ctx>(&'ctx self, idents: &'ctx Idents) -> AstNodeDisplay {
        AstNodeDisplay { node: self, idents }
    }
}

pub struct AstNodeDisplay<'ctx, 'id> {
    node: &'ctx AstNode,
    idents: &'ctx Idents<'id>,
}

impl<'ctx, 'id> Display for AstNodeDisplay<'ctx, 'id> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.node {
            AstNode::Var(x) => write!(f, "{}", self.idents[*x]),
            AstNode::Abs(x, t) => {
                write!(f, "λ{}.{}", self.idents[*x], t.display(self.idents))
            }
            AstNode::App(t1, t2) => {
                match t1.as_ref() {
                    AstNode::Abs(_, _) => write!(f, "({})", t1.display(self.idents))?,
                    _ => write!(f, "{}", t1.display(self.idents))?,
                }
                match t2.as_ref() {
                    AstNode::Var(_) => write!(f, "{}", t2.display(self.idents)),
                    _ => write!(f, "({})", t2.display(self.idents)),
                }
            }
        }
    }
}

impl<'a> Display for Ast<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.root.display(&self.idents))
    }
}

#[derive(Clone)]
pub struct ParseError(String);

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Parse error: {}", self.0)
    }
}

impl Error for ParseError {}

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

pub fn parse<'id>(program: &'id str) -> Result<Ast<'id>> {
    let mut idents = Idents::new();
    let mut token_trees = TokenTrees {
        tokenizer: Tokenizer::new(program),
    };
    for _ in 0..50 {
        dbg!((&mut token_trees).collect::<Vec<_>>());
    }
    Err("".into())
    // let mut token_trees = TokenTrees {
    //     tokenizer: Tokenizer::new(program),
    // };
    // let macros = parse_macros(&mut token_trees, &mut idents)?;
    // let (root, _) = parse_term(token_trees, &mut idents)?;

    // Ok(Ast {
    //     root,
    //     idents,
    //     macros,
    // })
}

fn parse_macros<'idents, 'id>(
    token_trees: &mut TokenTrees<'id>,
    idents: &'idents mut Idents<'id>,
) -> Result<MacroMap> {
    let mut macros = MacroMap::new();
    let mut token_trees = token_trees.peekable();

    while let Some(result) = token_trees.peek() {
        let name = match result {
            Ok(TokenTree::Macro(name)) => *name,
            Ok(_) => break,
            Err(err) => return Err(err.clone()),
        };
        token_trees.next();

        let ident = idents.get_ident(name);
        if macros.contains_key(&ident) {
            return Err(format!("Macro '{}' is already defined", name).into());
        }

        let (term, _) = parse_term(&mut token_trees, idents)?;
        macros.insert(ident, term);
    }

    Ok(macros)
}

#[derive(Debug)]
pub enum TokenTree<'a> {
    // x
    Id(&'a str),
    // λx.
    Abs(&'a str),
    // let x =
    Macro(&'a str),
    // (...)
    Tree,
}

pub struct TokenTrees<'a> {
    tokenizer: Tokenizer<'a>,
}

impl<'a> Iterator for TokenTrees<'a> {
    type Item = Result<TokenTree<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokenizer.next() {
            Some(Token::Id(id)) => Some(Ok(TokenTree::Id(id))),
            Some(Token::Lambda) => {
                let id = match self.tokenizer.next() {
                    Some(Token::Id(id)) => id,
                    _ => return Some(Err("Expected identifier after 'λ'".into())),
                };
                match self.tokenizer.next() {
                    Some(Token::Dot) => Some(Ok(TokenTree::Abs(id))),
                    _ => Some(Err(format!("Expected '.' after 'λ{id}'").into())),
                }
            }
            Some(Token::Let) => {
                let id = match self.tokenizer.next() {
                    Some(Token::Id(id)) => id,
                    _ => return Some(Err("Expected identifier after 'let'".into())),
                };
                match self.tokenizer.next() {
                    Some(Token::Eq) => Some(Ok(TokenTree::Macro(id))),
                    _ => Some(Err(format!("Expected '=' after 'let {id}'").into())),
                }
            }
            Some(Token::LParen) => Some(Ok(TokenTree::Tree)),
            Some(Token::RParen) | Some(Token::In) => None,
            Some(tok) => Some(Err(format!("Unexpected token: {:?}", tok).into())),
            None => None,
        }
    }
}

fn parse_term<'id, I>(mut token_trees: I, idents: &mut Idents<'id>) -> Result<(AstNode, I)>
where
    I: Iterator<Item = Result<TokenTree<'id>>>,
{
    match token_trees.next().transpose()? {
        None => Err("Unexpected end of input".into()),
        Some(TokenTree::Abs(id)) => {
            let (body, iter) = parse_term(token_trees, idents)?;
            Ok((AstNode::Abs(idents.get_ident(id), Rc::new(body)), iter))
        }
        Some(tt) => {
            let (base, token_trees) = parse_token_tree(tt, token_trees, idents)?;
            parse_app(base, token_trees, idents)
        }
    }
}

fn parse_token_tree<'id, I>(
    token_tree: TokenTree<'id>,
    token_trees: I,
    idents: &mut Idents<'id>,
) -> Result<(AstNode, I)>
where
    I: Iterator<Item = Result<TokenTree<'id>>>,
{
    match token_tree {
        TokenTree::Id(id) => Ok((AstNode::Var(idents.get_ident(id)), token_trees)),
        TokenTree::Abs(id) => Err(format!("Expected term after 'λ{id}.'").into()),
        TokenTree::Macro(_) => Err("Unexpected macro definition".into()),
        TokenTree::Tree => parse_term(token_trees, idents),
    }
}

fn parse_app<'id, I>(
    mut base: AstNode,
    mut token_trees: I,
    idents: &mut Idents<'id>,
) -> Result<(AstNode, I)>
where
    I: Iterator<Item = Result<TokenTree<'id>>>,
{
    while let Some(tt) = token_trees.next() {
        let (arg, tts) = parse_token_tree(tt?, token_trees, idents)?;
        token_trees = tts;
        base = AstNode::App(Rc::new(base), Rc::new(arg))
    }
    Ok((base, token_trees))
}

// #[test]
// fn test_parse() -> Result<()> {
//     use AstNode::*;

//     assert_eq!(parse(&tokenize("abc"))?, Var("abc".into()));
//     assert_eq!(parse(&tokenize("(abc)")).unwrap(), Var("abc".into()));
//     assert_eq!(
//         parse(&tokenize("a (abc)")).unwrap(),
//         App(Box::new(Var("a".into())), Box::new(Var("abc".into())))
//     );
//     assert_eq!(
//         parse(&tokenize("\\a. a")).unwrap(),
//         Abs("a".into(), Box::new(Var("a".into())))
//     );
//     assert_eq!(
//         parse(&tokenize("x \\a. a")).unwrap(),
//         App(
//             Box::new(Var("x".into())),
//             Box::new(Abs("a".into(), Box::new(Var("a".into()))))
//         )
//     );
//     assert_eq!(
//         parse(&tokenize("(\\x. a x) (\\b. b) c")).unwrap(),
//         App(
//             Box::new(App(
//                 Box::new(Abs(
//                     "x".into(),
//                     Box::new(App(Box::new(Var("a".into())), Box::new(Var("x".into()))))
//                 )),
//                 Box::new(Abs("b".into(), Box::new(Var("b".into()))))
//             )),
//             Box::new(Var("c".into()))
//         )
//     );
//     assert_eq!(
//         parse(&tokenize("\\a. \\b. a b")).unwrap(),
//         Abs(
//             "a".into(),
//             Box::new(Abs(
//                 "b".into(),
//                 Box::new(App(Box::new(Var("a".into())), Box::new(Var("b".into()))))
//             ))
//         )
//     );

//     assert_eq!(
//         parse(&tokenize("ID = \\x.x; a b")).unwrap(),
//         App(Var("a".into()).into(), Var("b".into()).into())
//     );
//     assert_eq!(
//         parse(&tokenize("ID = \\x.x; ID b")).unwrap(),
//         App(
//             Abs("x".into(), Var("x".into()).into()).into(),
//             Var("b".into()).into()
//         )
//     );
//     assert_eq!(
//         parse(&tokenize("TRU = \\t.\\f.t; TWO = TRU 2; TWO")).unwrap(),
//         App(
//             Abs("t".into(), Abs("f".into(), Var("t".into()).into()).into()).into(),
//             Var("2".into()).into()
//         )
//     );

//     assert!(parse(&tokenize(")")).is_err());
//     assert!(parse(&tokenize("abc(")).is_err());
//     assert!(parse(&tokenize("\\a a")).is_err());
//     assert!(parse(&tokenize("(\\a. a() \\b. b)")).is_err());
//     assert!(parse(&tokenize("(\\a. a) \\b. b c.")).is_err());
//     assert!(parse(&tokenize("\\x.x;")).is_err());

//     Ok(())
// }
