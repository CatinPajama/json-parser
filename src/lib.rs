use parser_combinator::predicate;

use crate::parser_combinator::{JsonNode, JsonValue, Parser};

pub mod parser_combinator {
    
    #[derive(Debug,PartialEq, Eq)]
    pub enum JsonValue {
        JsonNumber(i32),
        JsonString(String),
        JsonObject(Vec<JsonNode>),
    }
    
    #[derive(Debug,PartialEq,Eq)]
    pub struct JsonNode {
        pub key : String,
        pub value : JsonValue,
    }
 
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

    pub fn identity<R1 : Default> (input : &str) -> Result<(&str,R1),&str> {
        return Ok((input,R1::default()))
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
    pub fn space0<'a>() -> impl Parser<'a,Vec<char>> {
        zero_or_more(predicate(any_char,|c| c.is_whitespace()))
    }

    pub fn parse_json_string<'a>() -> impl Parser<'a,JsonValue> {
        map(right(
            match_literal("\""),
            left(
                zero_or_more(predicate(any_char,|c| *c != '\"')),
            match_literal("\""),
        )),|c| JsonValue::JsonString(c.into_iter().collect()))
    }
    
    pub fn parse_key<'a>() -> impl Parser<'a,String> {
        map(left(left(left(zero_or_more(predicate(any_char,|c| *c != ' ')),space0()),predicate(any_char,|c| *c == ':')),space0()),|c| c.into_iter().collect())
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

    pub fn parse_json_number<'a>() -> impl Parser<'a,JsonValue> {
        let a = map(one_or_more(predicate(any_char,|c : &char| c.is_numeric())),|c| (c.into_iter().collect::<String>()));
        map(a,|s| JsonValue::JsonNumber(s.parse::<i32>().unwrap()))
    }

    //pub fn parse_until<'a>(c : char) -> impl Parser<'a,String> {
        //map(zero_or_more())
    //}


    pub fn parse_value<'a>() -> impl Parser<'a,JsonValue> {
        move |input| {
            //either(either(parse_json_number(),parse_json_string()),parse_object()).parse(input)
            either(parse_object(),either(parse_json_number(),parse_json_string())).parse(input)
        }
    }

    pub fn parse_object<'a>() -> impl Parser<'a,JsonValue> {
        let a =  left(right(right(match_literal("{"),space0()),zero_or_more(parse_key_value())),match_literal("}"));
        map(a,|v| JsonValue::JsonObject(v.into_iter().map(|(key,val)|  JsonNode{key , value : val}).collect()))
    }
    

    // FIX , part 
    pub fn parse_key_value<'a>() -> impl Parser<'a,(String,JsonValue)> {
        //right(space0(),pair(parse_key(),left(parse_value(),left(match_literal(","),space0()))))
        //pair(parse_key(),parse_value())
        left(right(space0(),pair(parse_key(),left(parse_value(),zero_or_more(match_literal(","))))),space0())
    }
}

#[test]
fn test_right() {
    let parse_gaurab = parser_combinator::match_literal("gaurab");
    let parse_khatry = parser_combinator::match_literal(" khatry");

    let new_parser = parser_combinator::right(parse_gaurab,parse_khatry);
    assert_eq!(new_parser.parse("gaurab khatry"),Ok(("",())));
}

#[test]
fn test_one_more() {
    let parse_gaurab = parser_combinator::match_literal("gaurab");
    let mult = parser_combinator::one_or_more(parse_gaurab);

    assert_eq!(mult.parse("gaurabgaurabgaurabkhatry"),Ok(("khatry",vec![(),(),()])));
}

#[test]
fn test_predicate() {
    let parse_a = parser_combinator::predicate(parser_combinator::any_char, |c| *c == 'a');
    assert_eq!(parse_a.parse("aurab"),Ok(("urab",'a')));
    assert_eq!(parse_a.parse("gaurab"),Err("gaurab"));
}

#[test]
fn test_quote_string() {
    let p = parser_combinator::parse_json_string();
    assert_eq!(p.parse("\"gaurab\""),Ok(("",JsonValue::JsonString("gaurab".to_string()))));
}

#[test]
fn test_key() {
    let p = parser_combinator::parse_key();
    assert_eq!(p.parse("gaurab : "),Ok(("","gaurab".to_string())));
}

#[test]
fn test_object_value() {
    let p = parser_combinator::parse_object();
    let x = p.parse("{gaurab : 92}");
    assert_eq!(x,Ok(("",JsonValue::JsonObject(vec![JsonNode{key:"gaurab".to_string(),value : JsonValue::JsonNumber(92)}]))));
}


#[test]
fn test_key_value() {
    let p = parser_combinator::parse_key_value();
    let x = p.parse("man : {gaurab : 92}");
    assert_eq!(x,Ok(("",("man".to_string(),JsonValue::JsonObject(vec![JsonNode{key:"gaurab".to_string(),value : JsonValue::JsonNumber(92)}])))));
    //assert_eq!(x,Ok(("",("gaurab".to_string(),parser_combinator::JsonValue::JsonNumber(92)))));
    
    let p2 = parser_combinator::parse_object();
    let x2 = p2.parse("{gaurab : 92, kapil : {name : \"kapil\", age : 9}}");
    let l1 = JsonNode {key : "gaurab".to_string(), value : JsonValue::JsonNumber(92)};

    let l21 = JsonNode{key : "name".to_string(), value : JsonValue::JsonString("kapil".to_string())};
    let l22 = JsonNode{key : "age".to_string(), value : JsonValue::JsonNumber(9)};
    let l2 = JsonNode {key : "kapil".to_string(), value : JsonValue::JsonObject(vec![l21,l22])};

    // assert_eq!(x2,Ok(("",JsonValue::JsonObject(vec![l1,l2]))));
}


