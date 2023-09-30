use std::rc::Rc;

use crate::parser::{Ast, AstNode, IdentRef, Idents};

impl Ast<'_> {
    pub fn eval_normal_order(&mut self) {
        self.root = self.root.clone().eval_normal_order(&mut self.idents);
    }
}

impl AstNode {
    fn eval_normal_order<'prog>(self: Rc<Self>, idents: &mut Idents<'prog>) -> Rc<Self> {
        match self.as_ref() {
            AstNode::Var(_) => self,
            AstNode::Abs(var, body) => {
                Rc::new(AstNode::Abs(*var, body.clone().eval_normal_order(idents)))
            }
            AstNode::App(fun, arg) => {
                let fun = fun.clone().eval_to_abs(idents);
                match fun.as_ref() {
                    AstNode::Abs(var, body) => body
                        .clone()
                        .replace(*var, arg.clone(), idents)
                        .eval_normal_order(idents),
                    _ => Rc::new(AstNode::App(fun, arg.clone().eval_normal_order(idents))),
                }
            }
        }
    }

    fn eval_to_abs<'prog>(self: Rc<Self>, idents: &mut Idents<'prog>) -> Rc<Self> {
        match self.as_ref() {
            AstNode::App(fun, arg) => {
                let fun = fun.clone().eval_to_abs(idents);
                match fun.as_ref() {
                    AstNode::Abs(var, body) => body
                        .clone()
                        .replace(*var, arg.clone(), idents)
                        .eval_to_abs(idents),
                    _ => Rc::new(AstNode::App(fun, arg.clone().eval_normal_order(idents))),
                }
            }
            _ => self,
        }
    }

    fn replace<'prog>(
        self: Rc<Self>,
        var: IdentRef,
        replacement: Rc<Self>,
        idents: &mut Idents<'prog>,
    ) -> Rc<Self> {
        match self.as_ref() {
            AstNode::Var(v) if *v == var => replacement,
            AstNode::Var(_) => self,
            AstNode::Abs(v, _) if *v == var => self,
            AstNode::Abs(v, body) if replacement.contains(var) => {
                let v_prime = idents.prime(*v);
                let var_node = Rc::new(AstNode::Var(v_prime));
                Rc::new(AstNode::Abs(
                    v_prime,
                    body.clone()
                        .replace(*v, var_node, idents)
                        .replace(var, replacement, idents),
                ))
            }
            AstNode::Abs(v, body) => Rc::new(AstNode::Abs(
                *v,
                body.clone().replace(var, replacement, idents),
            )),
            AstNode::App(fun, arg) => Rc::new(AstNode::App(
                fun.clone().replace(var, replacement.clone(), idents),
                arg.clone().replace(var, replacement, idents),
            )),
        }
    }

    fn contains(&self, var: IdentRef) -> bool {
        match self {
            AstNode::Var(v) => *v == var,
            AstNode::Abs(v, _) if *v == var => false,
            AstNode::Abs(_, body) => body.contains(var),
            AstNode::App(fun, arg) => fun.contains(var) || arg.contains(var),
        }
    }
}
