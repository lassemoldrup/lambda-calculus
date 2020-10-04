use crate::parser::AstNode;

// Normal order
pub fn eval(node: AstNode) -> AstNode {
    use AstNode::*;

    match node.clone() {
        Var(_) => node,
        Abstraction(id, body) => Abstraction(id, Box::new(eval(*body))),
        Application(fun, arg) => match *fun {
            Abstraction(id, body) => replace(&id, &*arg, &*body),
            _ => {
                let result = Application(Box::new(eval(*fun.clone())), arg.clone());
                if result == node {
                    Application(fun, Box::new(eval(*arg)))
                } else {
                    eval(result)
                }
            }
        },
    }
}

// Beta reduction
fn replace(id: &String, val: &AstNode, node: &AstNode) -> AstNode {
    use AstNode::*;

    match &node {
        Var(x) => if x == id { val.clone() } else { node.clone() },
        Abstraction(x, _) if x == id => node.clone(),
        Abstraction(x, body) => Abstraction(x.clone(), Box::new(replace(id, val, body))),
        Application(fun, arg) => Application(Box::new(replace(id, val, fun)),
                                             Box::new(replace(id, val, arg))),
    }
}