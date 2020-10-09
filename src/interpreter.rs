use crate::parser::AstNode;

pub enum Strategy {
    NormalOrder,
    CallByName,
}

impl AstNode {
    pub fn eval(self, strategy: Strategy) -> Self {
        match strategy {
            Strategy::NormalOrder => self.eval_normal_order(),
            Strategy::CallByName => self.eval_call_by_name(),
        }
    }
    
    fn eval_normal_order(self) -> Self {
        use AstNode::*;

        match self {
            Var(_) => self,
            Abs(id, body) => Abs(id, body.eval_normal_order().into()),
            App(fun, arg) => match *fun {
                Abs(id, body) => body.substitute(&id, &*arg).eval_normal_order(),
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

    fn eval_call_by_name(self) -> Self {
        use AstNode::*;

        match self {
            App(fun, arg) => {
                let lambda = fun.eval_call_by_name();
                match lambda {
                    Abs(id, body) => body.substitute(&id, &*arg).eval_call_by_name(),
                    _ => App(lambda.into(), arg.eval_call_by_name().into()),
                }
            },
            _ => self,
        }
    }

    // Beta reduction
    fn substitute(self, id: &String, term: &Self) -> Self {
        use AstNode::*;

        match self {
            Var(ref x) if x == id => term.clone(),
            Var(_) => self,
            Abs(ref x, _) if x == id => self,
            Abs(x, body) => Abs(x, body.substitute(id, term).into()),
            App(fun, arg) => App(fun.substitute(id, term).into(), arg.substitute(id, term).into()),
        }
    }
}