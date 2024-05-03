mod parser_combinator;

pub mod json_parser {

    use crate::parser_combinator::*;

    #[derive(Debug,PartialEq, Eq)]
    pub enum JsonValue {
        JsonNumber(i32),
        JsonString(String),
        JsonObject(Vec<JsonNode>),
        JsonArray(Vec<JsonValue>)
    }
    
    #[derive(Debug,PartialEq,Eq)]
    pub struct JsonNode {
        pub key : String,
        pub value : JsonValue,
    }
    pub fn space0<'a>() -> impl Parser<'a,Vec<char>> {
        zero_or_more(predicate(any_char,|c| c.is_whitespace()))
    }

    pub fn parse_json_string<'a>() -> impl Parser<'a,JsonValue> {
        whitespace_wrap(
        map(right(
            match_literal("\""),
            left(
                zero_or_more(predicate(any_char,|c| *c != '\"')),
            match_literal("\""),
        )),|c| JsonValue::JsonString(c.into_iter().collect())))
    }
    
    pub fn parse_key<'a>() -> impl Parser<'a,String> {
        //map(left(left(zero_or_more(predicate(any_char,|c| *c != ' ')),space0()),predicate(any_char,|c| *c == ':')),|c| c.into_iter().collect())
        map(left(whitespace_wrap(zero_or_more(predicate(any_char,|c| *c != ' '))),predicate(any_char,|c| *c == ':')), |c| c.into_iter().collect())
    }

    pub fn parse_json_number<'a>() -> impl Parser<'a,JsonValue> {
        let a = map(one_or_more(predicate(any_char,|c : &char| c.is_numeric())),|c| (c.into_iter().collect::<String>()));
        whitespace_wrap(map(a,|s| JsonValue::JsonNumber(s.parse::<i32>().unwrap())))
    }

    pub fn parse_value<'a>() -> impl Parser<'a,JsonValue> {
        move |input| {
            let comb_json_parser = either(parse_json_object(),either(parse_json_number(),either(parse_json_array(),parse_json_string())));
            left(comb_json_parser,zero_or_more(match_literal(","))).parse(input)
        }
    }

    pub fn parse_json_array<'a>() -> impl Parser<'a,JsonValue> {
        map(whitespace_wrap(left(right(match_literal("["),zero_or_more(parse_value())),match_literal("]"))),|v| JsonValue::JsonArray(v))
    }

    pub fn parse_json_object<'a>() -> impl Parser<'a,JsonValue> {
        //let a =  left(right(right(match_literal("{"),space0()),zero_or_more(parse_key_value())),match_literal("}"));
        let a = whitespace_wrap(left(right(match_literal("{"),zero_or_more(parse_key_value())),match_literal("}")));
        map(a,|v| JsonValue::JsonObject(v.into_iter().map(|(key,val)|  JsonNode{key , value : val}).collect()))
    }
    
    pub fn whitespace_wrap<'a,P,O>(p : P) -> impl Parser<'a,O> 
    where 
        P : Parser<'a,O>
    {
        right(space0(),left(p,space0()))
    }

    // FIX , part 
    pub fn parse_key_value<'a>() -> impl Parser<'a,(String,JsonValue)> {
        //right(space0(),pair(parse_key(),left(parse_value(),left(match_literal(","),space0()))))
        //pair(parse_key(),parse_value())
        //left(right(space0(),pair(parse_key(),left(parse_value(),zero_or_more(match_literal(","))))),space0())
        right(space0(),pair(parse_key(),parse_value()))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser_combinator::*;
    use crate::json_parser::*;
    #[test]
    fn test_quote_string() {
        let p = parse_json_string();
        assert_eq!(p.parse("\"gaurab\""),Ok(("",JsonValue::JsonString("gaurab".to_string()))));
    }

    #[test]
    fn test_key() {
        let p = parse_key();
        assert_eq!(p.parse("gaurab : "),Ok((" ","gaurab".to_string())));
    }

    #[test]
    fn test_object_value() {
        let p = parse_json_object();
        let x = p.parse("{gaurab : 92}");
        assert_eq!(x,Ok(("",JsonValue::JsonObject(vec![JsonNode{key:"gaurab".to_string(),value : JsonValue::JsonNumber(92)}]))));
    }


    #[test]
    fn test_key_value() {
        let p = parse_key_value();
        let x = p.parse("man : {gaurab : 92}");
        assert_eq!(x,Ok(("",("man".to_string(),JsonValue::JsonObject(vec![JsonNode{key:"gaurab".to_string(),value : JsonValue::JsonNumber(92)}])))));

        let p2 =parse_json_object();
        let x2 = p2.parse("{gaurab : 92, kapil : {name : \"kapil\", mark : [1,2]}}");
        let l1 = JsonNode {key : "gaurab".to_string(), value : JsonValue::JsonNumber(92)};

        let l21 = JsonNode{key : "name".to_string(), value : JsonValue::JsonString("kapil".to_string())};
        let l22 = JsonNode{key : "mark".to_string(), value : JsonValue::JsonArray(vec![JsonValue::JsonNumber(1),JsonValue::JsonNumber(2)])};
        let l2 = JsonNode {key : "kapil".to_string(), value : JsonValue::JsonObject(vec![l21,l22])};

        assert_eq!(x2,Ok(("",JsonValue::JsonObject(vec![l1,l2]))));
    }
}
