use std::borrow::Cow;

use crate::parser::AstNode;

impl<'a> AstNode<'a> {
    pub fn eval_normal_order(self) -> Self {
        use AstNode::*;

        match self {
            Var(_) => self,
            Abs(id, body) => Abs(id, body.eval_normal_order().into()),
            App(fun, arg) => match *fun {
                Abs(id, body) => body.substitute(&id, &arg).eval_normal_order(),
                _ => {
                    let lambda = fun.eval_call_by_name();
                    match lambda {
                        Abs(_, _) => App(lambda.into(), arg).eval_normal_order(),
                        _ => App(lambda.into(), arg.eval_normal_order().into()),
                    }
                }
            }
        }
    }

    pub fn eval_call_by_name(self) -> Self {
        use AstNode::*;

        match self {
            App(fun, arg) => {
                let lambda = fun.eval_call_by_name();
                match lambda {
                    Abs(id, body) => body.substitute(&id, &arg).eval_call_by_name(),
                    _ => App(lambda.into(), arg.eval_call_by_name().into()),
                }
            },
            _ => self,
        }
    }

    // Beta reduction
    pub fn substitute(self, id: &str, term: &Self) -> Self {
        use AstNode::*;

        match self {
            Var(x) if x == id => term.clone(),
            Var(_) => self,
            Abs(ref x, _) if x == id => self,
            Abs(ref x, _) if term.contains(&x) => { // TODO: Fix this
                let cloned_x = x.clone();
                self.rename(&cloned_x).substitute(id, term)
            },
            Abs(x, body) => Abs(x, body.substitute(id, term).into()),
            App(fun, arg) => App(fun.substitute(id, term).into(), arg.substitute(id, term).into()),
        }
    }

    // Alpha reduction
    fn rename(self, id: &str) -> Self {
        use AstNode::*;

        match self {
            Var(x) if x == id => Var(primed(x)),
            Var(_) => self,
            Abs(x, body) if x == id => Abs(primed(x), body.rename(id).into()),
            Abs(x, body) => Abs(x, body.rename(id).into()),
            App(fun, arg) => App(fun.rename(id).into(), arg.rename(id).into()),
        }
    }

    // self has id in free vars
    fn contains(&self, id: &str) -> bool {
        use AstNode::*;

        match self {
            Var(x) => x == id,
            Abs(x, body) => x != id && body.contains(id),
            App(fun, arg) => fun.contains(id) || arg.contains(id),
        }
    }
}

fn primed(mut x: Cow<str>) -> Cow<str> {
    x.to_mut().push('\'');
    x
}
