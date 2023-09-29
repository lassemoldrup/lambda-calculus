use hashbrown::HashMap;

use super::lexer::*;
use std::error::Error;
use std::fmt::{self, Debug, Display, Formatter};
use std::iter::Peekable;
use std::ops::Index;
use std::rc::Rc;

type Ident = usize;

#[derive(Default)]
struct Idents<'prog> {
    num_to_string: Vec<&'prog str>,
    string_to_num: HashMap<&'prog str, Ident>,
}

impl<'prog> Idents<'prog> {
    fn new() -> Self {
        Self::default()
    }

    fn get_ident(&mut self, s: &'prog str) -> Ident {
        let string_to_num = &mut self.string_to_num;
        let num_to_string = &mut self.num_to_string;
        *string_to_num.entry(s).or_insert_with(|| {
            num_to_string.push(s);
            num_to_string.len() - 1
        })
    }
}

impl<'prog> Index<usize> for Idents<'prog> {
    type Output = &'prog str;

    fn index(&self, index: usize) -> &Self::Output {
        &self.num_to_string[index]
    }
}

type MacroMap = HashMap<usize, Rc<AstNode>>;

pub struct Ast<'prog> {
    root: Rc<AstNode>,
    idents: Idents<'prog>,
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

    fn expand_macros(&self, macros: &MacroMap, idents: &Idents) -> Result<Rc<AstNode>> {
        match self {
            AstNode::Var(x) => Ok(macros
                .get(x)
                .map(Rc::clone)
                .unwrap_or_else(|| Rc::new(AstNode::Var(*x)))),
            AstNode::Abs(x, _) if macros.contains_key(x) => {
                Err(format!("Parameter shadowing macro '{}'", idents[*x]).into())
            }
            AstNode::Abs(x, t) => Ok(Rc::new(AstNode::Abs(*x, t.expand_macros(macros, idents)?))),
            AstNode::App(t1, t2) => Ok(Rc::new(AstNode::App(
                t1.expand_macros(macros, idents)?,
                t2.expand_macros(macros, idents)?,
            ))),
        }
    }
}

pub struct AstNodeDisplay<'ctx, 'prog> {
    node: &'ctx AstNode,
    idents: &'ctx Idents<'prog>,
}

impl Display for AstNodeDisplay<'_, '_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.node {
            AstNode::Var(x) => write!(f, "{}", self.idents[*x]),
            AstNode::Abs(x, t) => {
                write!(f, "λ{}. {}", self.idents[*x], t.display(self.idents))
            }
            AstNode::App(t1, t2) => {
                match t1.as_ref() {
                    AstNode::Abs(_, _) => write!(f, "({})", t1.display(self.idents))?,
                    _ => write!(f, "{}", t1.display(self.idents))?,
                }
                match t2.as_ref() {
                    AstNode::Var(_) => write!(f, " {}", t2.display(self.idents)),
                    _ => write!(f, " ({})", t2.display(self.idents)),
                }
            }
        }
    }
}

impl Display for Ast<'_> {
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

pub struct Parser<'prog> {
    tokenizer: Peekable<Tokenizer<'prog>>,
    idents: Idents<'prog>,
    paren_level: usize,
    in_macro: bool,
}

impl<'prog> Parser<'prog> {
    pub fn new(program: &'prog str) -> Self {
        Self {
            tokenizer: Tokenizer::new(program).peekable(),
            idents: Idents::new(),
            paren_level: 0,
            in_macro: false,
        }
    }

    pub fn parse(mut self) -> Result<Ast<'prog>> {
        let macros = self.parse_macros()?;
        let root = self.parse_term()?.expand_macros(&macros, &self.idents)?;

        Ok(Ast {
            root,
            idents: self.idents,
        })
    }

    fn parse_macros(&mut self) -> Result<MacroMap> {
        let mut macros = MacroMap::new();

        while let Some(Token::Let) = self.tokenizer.peek() {
            self.tokenizer.next();
            let Some(Token::Id(name)) = self.tokenizer.next() else {
                return Err("Expected identifier after 'let'".into());
            };

            let ident = self.idents.get_ident(name);
            if macros.contains_key(&ident) {
                return Err(format!("Macro '{name}' is already defined").into());
            }

            if self.tokenizer.next() != Some(Token::Eq) {
                return Err(format!("Expected '=' after 'let {name}'").into());
            }

            self.in_macro = true;
            let term = self.parse_term()?.expand_macros(&macros, &self.idents)?;
            if self.tokenizer.next() != Some(Token::In) {
                return Err("Missing term after macro, did you forget 'in'?".into());
            }
            self.in_macro = false;
            macros.insert(ident, term);
        }

        Ok(macros)
    }

    fn parse_term(&mut self) -> Result<AstNode> {
        let Some(mut term) = self.parse_one()? else {
            return Err("Empty term".into());
        };
        while let Some(arg) = self.parse_one()? {
            term = AstNode::App(Rc::new(term), Rc::new(arg));
        }
        Ok(term)
    }

    fn parse_one(&mut self) -> Result<Option<AstNode>> {
        match self.tokenizer.peek() {
            Some(Token::Id(id)) => {
                let id = self.idents.get_ident(id);
                self.tokenizer.next();
                Ok(Some(AstNode::Var(id)))
            }
            Some(Token::Lambda) => {
                self.tokenizer.next();
                let id = match self.tokenizer.next() {
                    Some(Token::Id(id)) => self.idents.get_ident(id),
                    _ => return Err("Expected identifier after 'λ'".into()),
                };
                if self.tokenizer.next() != Some(Token::Dot) {
                    return Err(format!("Expected '.' after 'λ{id}'").into());
                }
                let body = self.parse_term()?;
                Ok(Some(AstNode::Abs(id, Rc::new(body))))
            }
            Some(Token::LParen) => {
                self.paren_level += 1;
                // Consume '('.
                self.tokenizer.next();
                let res = self.parse_term();
                self.paren_level -= 1;
                // Consume ')'.
                if self.tokenizer.next() != Some(Token::RParen) {
                    return Err(format!("Expected ')'").into());
                }
                res.map(Some)
            }
            Some(Token::RParen) if self.paren_level > 0 => Ok(None),
            Some(Token::In) if self.in_macro && self.paren_level == 0 => Ok(None),
            Some(tk) => Err(format!("Unexpected token: {tk:?}").into()),
            None => Ok(None),
        }
    }
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
