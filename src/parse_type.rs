
// use std::path::Path;

use crate::*;


/// this gets used by 
pub fn parse(s: &str) -> Result<SlowType, String> {
    let (ty, s_left) = parse_aux(s).map_err(|e| format!("{e}\n when parsing: {s}"))?;
        if !s_left.is_empty() {
            return Err(format!("Type parse() error: extra closeparen\n when parsing: {s}"))
        }
    Ok(ty)
}

/// parses `s` into a type until hitting either a closeparen that hasn't been opened
/// in `s`, or an end of string, and returns the type and the remaining string not including
/// the closeparen if there was one
fn parse_aux(mut s: &str) -> Result<(SlowType, &str), String> {
    let arrow = ARROW_SYM.as_ref();
    let mut res = vec![];

    fn finish(mut res: Vec<SlowType>) -> Result<SlowType, String> {
        if res.is_empty() {
            return Err("Type parse() error: unexpected empty parens or empty string in type".into())
        }
        if res.len() == 1 {
            // we automatically strip down ((int)) -> int, and likewise "foo -> bar" is the same as "(foo -> bar)" at the top level
            return Ok(res.pop().unwrap()) 
        }
        let head = res.remove(0);
        match head {
            SlowType::Var(_) => Err("Type parse() error: type variable is applied to args".into()),
            SlowType::Term(name, args) => {
                if !args.is_empty() {
                    Err("Type parse() error: Term type applied to args like ((list int) int)".into())
                } else {
                    Ok(SlowType::Term(name, res))
                }
            }
        }
    }

    loop {
        s = s.trim();

        if s.is_empty() || s.starts_with(')') {
            // s is empty or hit closeparen: return
            if !s.is_empty() {
                s = &s[1..];
            }
            return finish(res).map(|res| (res, s))
        }

        if s.starts_with('(') {
            // hit an openparen: recurse
            let (ty, s_new) = parse_aux(&s[1..])?;
            s = s_new;
            res.push(ty);
            continue
        }

        // no closeparen/openparen so must be a new token. Parse forward until hitting a space or end-of-string


        let (item, s_new) = s.split_at(s.find([' ', ')']).unwrap_or(s.len()));
        s = s_new;

        // check if it's a var like t0 t23 etc
        if let Some(rest) = item.strip_prefix('t') {
            if let Ok(i) = rest.parse::<usize>() {
                res.push(SlowType::Var(i));
                continue
            }
        }

        // check if it's an arrow type and if so parse the left and right sides
        if item == arrow {
            if res.is_empty() {
                return Err("Type parse() error: no args to the left of an arrow".into())
            }

            // arrows are a low prio operator so group everything before into one term
            let ty_left = finish(res).map_err(|s| format!("during arrow rearranging: {s}"))?;
            // parse everything to the right
            let (ty_right, s_new) = parse_aux(&s[1..])?;
            s = s_new;
            // construct the arrow
            return Ok((SlowType::Term(ARROW_SYM.clone(), vec![ty_left, ty_right]),s));
        }
        
        // parse it as a new atomic type
        res.push(SlowType::Term(item.into(), vec![]))

    }
}

