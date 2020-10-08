use crate::parser::AstNode;

impl AstNode {
    // Normal order
    pub fn eval(self) -> AstNode {
        use AstNode::*;

        match self.clone() {
            Var(_) => self,
            Abstraction(id, body) => Abstraction(id, Box::new(body.eval())),
            Application(fun, arg) => match *fun {
                Abstraction(id, body) => (*body).substitute(&id, &*arg),
                _ => {
                    let result = Application(Box::new(fun.clone().eval()), arg.clone());
                    if result == self {
                        Application(fun, Box::new(arg.eval()))
                    } else {
                        result.eval()
                    }
                }
            },
        }
    }

    // Beta reduction
    fn substitute(&self, id: &String, val: &AstNode) -> AstNode {
        use AstNode::*;

        match self {
            Var(x) => if x == id { val.clone() } else { self.clone() },
            Abstraction(x, _) if x == id => self.clone(),
            Abstraction(x, body) => Abstraction(x.clone(), Box::new(body.substitute(id, val))),
            Application(fun, arg) => Application(Box::new(fun.substitute(id, val)),
                                                 Box::new(arg.substitute(id, val))),
        }
    }
}