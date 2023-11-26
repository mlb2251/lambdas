use std::{fmt::{Display, Formatter, self}};

use crate::expr::*;


/// Takes a string, parses it into an Expr, then returns that string printed back out.
/// This normalizes the string eg all `lambda` gets converted to `lam` etc.
pub fn reparse(s: &str) -> String {
    let mut e = ExprSet::empty(Order::ChildFirst, false, false);
    let idx = e.parse_extend(s).unwrap();
    e.get(idx).to_string()
}


/// printing a single node prints the operator
impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(i, tag) => {
                write!(f, "${}", i)?;
                if *tag != -1 {
                    write!(f, "_{}", tag)?;
                }
                Ok(())
            },
            Self::Prim(p) => write!(f,"{}",p),
            Self::App(_,_) => write!(f,"app"),
            Self::Lam(_, tag) => {
                write!(f,"lam")?;
                if *tag != -1 {
                    write!(f, "_{}", tag)?;
                }
                Ok(())
            },
            Self::IVar(i) => write!(f,"#{}",i),
        }
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_local(e: Expr, left_of_app: bool, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            if e.idx == HOLE {
                return write!(f,"??");
            }

            match e.node() {
                Node::Var(_, _) | Node::IVar(_) | Node::Prim(_) => write!(f,"{}", e.node()),
                Node::App(fun,x) => {
                    // if you are the left side of an application, and you are an application, you dont need parens
                    if !left_of_app { write!(f,"(")? }
                    fmt_local(e.get(*fun), true, f)?;
                    write!(f," ")?;
                    fmt_local(e.get(*x), false, f)?;
                    if !left_of_app { write!(f,")") } else { Ok(()) }
                },
                Node::Lam(b, tag) => {
                    write!(f,"(lam")?;
                    if *tag != -1 {
                        write!(f, "_{}", tag)?;
                    }
                    write!(f," ")?;
                    fmt_local(e.get(*b), false, f)?;
                    write!(f,")")
                }
            }
        }
        fmt_local(*self, false, f)
    }
}

impl Display for ExprOwned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.immut())
    }
}

impl ExprSet {
    /// parses `s_init` as an Expr, inserting it into `self`. Uses .add() so spans
    /// and structural hashing are done automatically. Is order-aware.
    pub fn parse_extend(&mut self, s_init: &str) -> Result<Idx,String> {
        let init_len = self.nodes.len();

        let mut s = s_init.trim();

        let mut items: Vec<Idx> = vec![];
        let mut items_of_depth: Vec<usize> = vec![]; // offsets[i] gives the number of items at depth i
        items_of_depth.push(0); // the zero paren depth

        while !s.trim().is_empty() {
            s = s.trim();
            let next =  s.chars().last().unwrap();
            if next == '(' {
                s = &s[..s.len()-1];
                let num_items = items_of_depth.pop().ok_or_else(||format!("ExprSet parse error: mismatched parens in: {}",s_init))?;
                if num_items == 0 {
                    continue
                }

                
                // now num_items >= 1. The following loop will only happen if num_items >= 2.
                // apply the last item to the second to last, etc
                for _ in 0..num_items-1 {
                    // println!("built an app inside");
                    let f: Idx = items.pop().unwrap();
                    let x: Idx = items.pop().unwrap();
                    items.push(self.add(Node::App(f, x)))
                }
                // then we simply leave that final result pushed on
                if let Some(num_items) = items_of_depth.last_mut() {
                    *num_items += 1;
                } else {
                    return Err(format!("ExprSet parse error: mismatched parens in: {}",s_init));
                }
                continue
            }
            if next == ')' {
                s = &s[..s.len()-1];
                items_of_depth.push(0);
                continue
            }
            // parse a space-separated word
            // println!("parsing with s: `{}`", s);
            let start = {
                let mut i = s.len()-1;
                loop {
                    if i == 0 {
                        // println!("break at i==0");
                        break
                    }
                    let c = s.chars().nth(i-1).unwrap();
                    if c.is_whitespace() || c == '(' || c == ')' {
                        // println!("break at c: {}", c);
                        break
                    }
                    i -= 1;
                }
                // println!("i: {}", i);
                i
            };
            let item_str = &s[start..];
            // println!("item_str: {}", item_str);
            s = &s[..start];

            if item_str == "lam" || item_str == "lambda"
                || item_str.starts_with("lam_") || item_str.starts_with("lambda_") {
                // split on _ and parse the number
                let mut tag = -1;
                if item_str.contains('_') {
                    let mut split = item_str.split('_');
                    split.next().unwrap(); // strip "lam"
                    tag = split.next().unwrap().parse::<i32>().map_err(|e|e.to_string())?;
                    if tag < 0 {
                        return Err(format!("ExprSet parse error: lambda tag must be non-negative: {}", s_init))
                    }
                }
                // println!("remainder: {}",s);
                let mut eof = false;
                if let Some(c) = s.chars().last()  {
                    if c != '(' {
                        return Err(format!("ExprSet parse error: `lam` must always have an immediately preceding parenthesis like so `(lam` unless its at the start of the parsed string: {}",s_init))
                    }
                    s = &s[..s.len()-1]; // strip "("
                } else {
                    eof = true;
                };

                let num_items = items_of_depth.pop().ok_or_else(||format!("ExprSet parse error: mismatched parens in: {}",s_init))?;
                if num_items != 1 {
                    return Err(format!("ExprSet parse error: `lam` must always be applied to exactly one argument, like `(lam (foo bar))`: {}",s_init))
                }
                let b: Idx = items.pop().unwrap();
                items.push(self.add(Node::Lam(b, tag)));
                // println!("added lam");
                if eof {
                    if items.len() != 1 {
                        return Err(format!("ExprSet parse error: mismatched parens in: {}",s_init));
                    }
                    return Ok(items.pop().unwrap())
                }
                if let Some(num_items) = items_of_depth.last_mut() {
                    *num_items += 1;
                } else {
                    return Err(format!("ExprSet parse error: mismatched parens in: {}",s_init));
                }
                continue
            }

            let node = {
                if let Some(mut rest) = item_str.strip_prefix('$') {
                    // check if item_str has a _ in it
                    let mut tag = -1;
                    if rest.contains('_') {
                        let mut split = rest.split('_');
                        rest = split.next().unwrap();
                        tag = split.next().unwrap().parse::<i32>().map_err(|e|e.to_string())?;
                        if tag < 0 {
                            return Err(format!("ExprSet parse error: variable tag must be non-negative: {}", s_init))
                        }
                    }
                    Node::Var(rest.parse::<i32>().map_err(|e|e.to_string())?, tag)
                } else if let Some(rest) = item_str.strip_prefix('#') {
                    Node::IVar(rest.parse::<i32>().map_err(|e|e.to_string())?)
                } else {
                    Node::Prim(item_str.into())
                }
            };
            items.push(self.add(node));
            *items_of_depth.last_mut().unwrap() += 1;
        }

        if items.is_empty() {
            return Err("ExprSet parse error: input is empty string".to_string());
        }

        if items_of_depth.len() != 1 {
            return Err(format!("ExprSet parse error: mismatched parens in: {}",s_init));
        }

        let num_items = items_of_depth.pop().unwrap();
        // println!("items outside: {}", num_items);
        for _ in 0..num_items-1 {
            // println!("built an app outside");
            let f: Idx = items.pop().unwrap();
            let x: Idx = items.pop().unwrap();
            items.push(self.add(Node::App(f, x)))
        }
        if items.len() != 1 {
            return Err(format!("ExprSet parse error: mismatched parens in: {}",s_init));
        }

        if self.order == Order::ParentFirst {
            self.nodes[init_len..].reverse();
        }
        Ok(items.pop().unwrap())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn assert_parse(set: &mut ExprSet, in_s: &str, out_s: &str) {
        let e = set.parse_extend(in_s).unwrap();
        assert_eq!(set.get(e).to_string(), out_s.to_string());
    }

    #[test]
    fn test_parse() {
        let set = &mut ExprSet::empty(Order::ChildFirst, false, false);
        assert_parse(set, "(+ 2 3)", "(+ 2 3)");
        assert_parse(set, "(+ 2 3)", "(+ 2 3)");

        assert_parse(set, "3", "3");
        assert_parse(set, "foo", "foo");

        assert_parse(set, "(foo (bar baz))", "(foo (bar baz))");
        assert_parse(set, "((foo bar) baz)", "(foo bar baz)");

        assert_parse(set, "foo bar baz", "(foo bar baz)");

        assert_parse(set, "(lam b)", "(lam b)");

        assert_parse(set, "lam b", "(lam b)");
        assert_parse(set, "(foo (lam b) (lam c))", "(foo (lam b) (lam c))");
        assert_parse(set, "(lam (+ a b))", "(lam (+ a b))");
        assert_parse(set, "(lam (+ $0 b))", "(lam (+ $0 b))");
        assert_parse(set, "(lam (+ #0 b))", "(lam (+ #0 b))");

        let e = set.parse_extend("$3").unwrap();
        assert_eq!(set.get(e).node(), &Node::Var(3, -1));
        let e = set.parse_extend("#3").unwrap();
        assert_eq!(set.get(e).node(), &Node::IVar(3));

        let e = set.parse_extend("$3_1").unwrap();
        assert_eq!(set.get(e).node(), &Node::Var(3, 1));

        let e = set.parse_extend("$23_145").unwrap();
        assert_eq!(set.get(e).node(), &Node::Var(23, 145));

        let e = set.parse_extend("$23_0").unwrap();
        assert_eq!(set.get(e).node(), &Node::Var(23, 0));

        let e = set.parse_extend("(lam_123 (+ $0_1 $1_1))").unwrap();
        let (_, tag) = match set.get(e).node() {
            Node::Lam(body, tag) => (*body, *tag),
            x => panic!("expected lam, got {}", x)
        };
        assert_eq!(tag, 123);

        assert_parse(set, "(lam_123 (+ $0_1 $1_1))", "(lam_123 (+ $0_1 $1_1))");

        assert_parse(set, "(fix1 $0 (lam (lam (if (empty? $0) $0 (cons (+ 1 (car $0)) ($1 (cdr $0)))))))", "(fix1 $0 (lam (lam (if (empty? $0) $0 (cons (+ 1 (car $0)) ($1 (cdr $0)))))))")

    }
}