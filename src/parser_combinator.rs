pub type ParserResult<'a,Output> = Result<(&'a str, Output),&'a str>;

pub trait Parser<'a,Output> {
    fn parse(&self,input : &'a str) -> ParserResult<'a,Output>;
}

impl<'a,F,Output> Parser<'a,Output> for F 
where F : Fn(&'a str) -> ParserResult<Output>, {

    fn parse(&self,input : &'a str) -> ParserResult<'a,Output> {
        self(input)
    }

}
pub fn map<'a,P1,F,O1, O2> (parser : P1, map_fn : F) -> impl Parser<'a,O2> where
P1 : Parser<'a,O1>,
F : Fn(O1) -> O2, {

    move |input| {
        match parser.parse(input) {
            Ok((rest,o1)) => Ok((rest,map_fn(o1))) ,
            Err(e) => Err(e)
        }
    }
}
pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}



pub fn one_or_more<'a,P,O1>(parser : P) -> impl Parser<'a,Vec<O1>>
where 
    P : Parser<'a,O1>
{
    move |mut input| {
        let mut outputs = Vec::new();
        match parser.parse(input) {
            Ok((rest,o)) => {
                input = rest;
                outputs.push(o);
            },
            Err(e) => {
                return Err(e);
            }
        };
        while let Ok((rest,o)) = parser.parse(input) {
            input = rest;
            outputs.push(o);
        } 

        Ok((input,outputs))
    }
}
pub fn zero_or_more<'a,P,O1>(parser : P) -> impl Parser<'a,Vec<O1>>
where 
    P : Parser<'a,O1>
{
    move |mut input| {
        let mut outputs = Vec::new();
        while let Ok((rest,o)) = parser.parse(input) {
            input = rest;
            outputs.push(o);
        } 

        Ok((input,outputs))
    }
}


pub fn match_literal(expected : &'static str) -> impl Fn(&str) -> Result<(&str,()), &str>{
    move |input : &str| {
        match input.get(0..expected.len()) {
            Some(x) if x == expected => Ok((&input[expected.len()..],())),
            _ => Err(input)
        }
    }
}

pub fn pair<'a,P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a,(R1,R2)>
where
    P1: Parser<'a,R1>,
    P2: Parser<'a,R2>
{
    move |input| match parser1.parse(input) {
        Ok((next_input, result1)) => match parser2.parse(next_input) {
            Ok((final_input, result2)) => Ok((final_input, (result1, result2))),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

pub fn any_char(input : &str) -> ParserResult<char> {
    match input.chars().next() {
        Some(x) => Ok((&input[x.len_utf8()..],x)),
        None => Err(input)
    }
}

pub fn predicate<'a,O1,P,F>(p : P, pred : F) -> impl Parser<'a,O1>
where 
    P : Parser<'a,O1>,
    F : Fn(&O1) -> bool
    {
        move |input| {
            if let Ok((rest,x)) = p.parse(input) {
                if pred(&x) {
                    return Ok((rest,x));
                }
            }
            Err(input)
        }
    }

pub fn either<'a,P1,P2,O1> (parser1 : P1, parser2 : P2) -> impl Parser<'a,O1> 
where 
    P1 : Parser<'a,O1>,
    P2 : Parser<'a,O1>
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser_combinator::*;
#[test]
fn test_right() {
    let parse_gaurab = match_literal("gaurab");
    let parse_khatry = match_literal(" khatry");

    let new_parser = right(parse_gaurab,parse_khatry);
    assert_eq!(new_parser.parse("gaurab khatry"),Ok(("",())));
}

#[test]
fn test_one_more() {
    let parse_gaurab = match_literal("gaurab");
    let mult = one_or_more(parse_gaurab);

    assert_eq!(mult.parse("gaurabgaurabgaurabkhatry"),Ok(("khatry",vec![(),(),()])));
}

#[test]
fn test_predicate() {
    let parse_a = predicate(any_char, |c| *c == 'a');
    assert_eq!(parse_a.parse("aurab"),Ok(("urab",'a')));
    assert_eq!(parse_a.parse("gaurab"),Err("gaurab"));
}
}
