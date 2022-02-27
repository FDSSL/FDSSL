use crate::syntax;

use syntax::Expr;
use syntax::BOp;
use syntax::UOp;
extern crate itertools;
use itertools::Itertools;

use std::collections::HashMap;

extern crate nom;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{char, alpha1, alphanumeric1, i32, multispace0, space0, space1, line_ending, not_line_ending, digit1};
use nom::{Needed};
use nom::{IResult};
use nom::branch::alt;
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::sequence::{preceded, delimited, terminated, separated_pair, tuple, pair};
use nom::number::complete::{float, double};
use nom::combinator::{map, verify, recognize, peek, opt, fail, not};


/// name parses any valid identifier, [_a-zA-Z][_a-zA-Z0-9]*
///
/// This function was /really/ based off the example from the nom documentation
fn name(i: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )
    )(i)
}

/// Parses a name into a String
fn name_str(i: &str) -> IResult<&str, String> {
    map(recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )
    ), |s: &str| s.to_string())(i)
}

/// Parses an int, corresponding to a 32-bit int in Rust.
/// Can be surrounded by spaces on either end
fn parse_int(input: &str) -> IResult<&str, Expr> {
    map(delimited(space0, i32, not(alt((alpha1, tag("_"))))), |i: i32| Expr::I(i))(input)
}

/// parse_float parses a 32-bit floating point value, preceded by 0+ spaces.
/// Floats are always denoted by a trailing 'f'
fn parse_float(i: &str) -> IResult<&str, Expr> {
    map(preceded(space0, terminated(float, tag("f"))), |v: f32| Expr::F(v))(i)
}

/// parse_double parses a 64-bit floating point value, preceded by 0+ spaces.
/// doubles are decimal precision numbers w/out a trailing f
fn parse_double(i: &str) -> IResult<&str, Expr> {
    // if OK, parse it
    // implicitly we don't need to check for 'f', since the float parser would have greedily taken it
    let mut parser = peek(preceded(space0 , preceded(opt(tag("-")), preceded(digit1, preceded(tag("."), parse_int)))));
    if parser(i).is_ok() {
        // proceed to parse a double
        map(preceded(space0, double), |v: f64| Expr::D(v))(i)
    } else {
        // fail out on this
        fail(i)
    }
}

/// Indicates whether a string is a keyword
fn is_keyword(s: &str) -> bool {
    let keywords = ["if","return","for","let","mut"];
    return keywords.contains(&s)
}

/// parse_ref parses a reference, which is defined by the `name` parser, see `name`
fn parse_ref(i: &str) -> IResult<&str, Expr> {
    map(preceded(space0, verify(name, |s : &str| !is_keyword(s))), |s: &str| Expr::Ref(s.to_string()))(i)
}

// Parses a Boolean value w/ optional leading space
fn parse_bool(input: &str) -> IResult<&str, Expr> {
    let vp1 = verify(preceded(space0, name), |s: &str| s == "true" || s == "false");
    map(vp1, |s: &str| Expr::B(s == "true"))(input)
}

/// parse_return parses a return expression.
///
/// The return expression takes an expression as an argument in the form `return expr`
// TODO Should we check for nested returns here?
// This almost makes me want to remove `return` again
fn parse_return(i: &str) -> IResult<&str, Expr> {
    map(
        preceded(
            delimited(space0, tag("return"), space1),
            parse_expr,
        ),
        |expr: Expr| Expr::Return(Box::new(expr))
    )(i)
}

/// ParsedType is a structural type enum used during parsing.
///
/// This enum is not meant to be used outside the parser and type checker
/// as it simply denotes the structure of the type rather than any type
/// space the type occupies.
#[derive(Debug, PartialEq)]
enum ParsedType {
    BaseType(String),
    Tuple(Vec<Box<ParsedType>>),
    Function(Box<ParsedType>, Box<ParsedType>),
}

/// parse_type_function parses the Function Type annotation.
///
/// This function parses type annotations of the form type -> type where `type`
/// is any type annotation. This allows the type annotation to nest other
/// structural types. e.g. t1 -> `t1 -> (t2, t2)`
fn parse_type_function(i: &str) -> IResult<&str, ParsedType> {
    map(
        tuple((
            parse_type,
            delimited(space1, tag("->"), space1),
            parse_type,
        )),
        |(t1, _, t2): (ParsedType, &str, ParsedType)| ParsedType::Function(Box::new(t1), Box::new(t2))
    )(i)
}

/// parse_type_function parses the Tuple Type annotation.
///
/// This function parses type annotations of the form (type, type+) where `type`
/// is any type annotation. This allows the type annotation to nest other
/// structural types. e.g. (t1, (t2, t3), t1 -> t2)
fn parse_type_tuple(i: &str) -> IResult<&str, ParsedType> {
    let mut parser =
        verify(
            delimited(
                terminated(tag("("), space0),
                separated_list1(
                    delimited(space0, tag(","), space0),
                    parse_type,
                ),
                preceded(space0, tag(")")),
            ),
            |elts: &Vec<ParsedType>| elts.len() > 1
        );
    map(
        parser,
        |elts: Vec<ParsedType>| ParsedType::Tuple(
            elts.into_iter().map(|e| Box::new(e)).collect()
        )
    )(i)
}

/// parse_type_base parses the Basic Type annotation.
///
/// This function parses the Basic Type annotation. This covers types including
/// `int`, `bool`, `float`, etc. These type annotations cannot be deconstructed,
/// so we simply save it.
// TODO perhaps type names should be more limited than reference names
fn parse_type_base(i: &str) -> IResult<&str, ParsedType> {
    map(name, |n: &str| ParsedType::BaseType(n.to_string()))(i)
}

/// parse_type is the root of the Type annotation parser.
///
/// This function parses type annotations for variable declarations. The type
/// annotations are segregated by structure, for the basic types like `int`, to
/// function types like `int -> int`, and the tuple type like `(int, bool)`.
/// This does not parse the `mut` keyword, as that denotes an attribute of the
/// variable rather than the type of the value it represents.
fn parse_type(i: &str) -> IResult<&str, ParsedType> {
    let (i, _) = space0(i)?;
    alt((
        parse_type_base,
        parse_type_tuple,
        parse_type_function,
    ))(i)
}
// Parses a comment, which is a valid element in our abstract syntax
// Comments are transformed from FDSSL to match the equivalent GLSL produced
fn parse_comment(input: &str) -> IResult<&str, Expr> {
    // match a '//' up to the end of the line
    // then, take a comment all the way to the end, ignoring the opener
    let comment = preceded(preceded(space0, tag("//")), not_line_ending);
    // map what we found into a valid comment
    map(comment, |s: &str| Expr::Comment(vec![s.into()]) )(input)
}

/// Parses a parenthetically nested expression
fn parse_nested_expr(input: &str) -> IResult<&str, Expr> {
    delimited(
        preceded(space0, char('(')),
        parse_expr,
        preceded(space0, char(')')),
    )(input)
}

/// Parses a vector w/ implicit numeric indices as a Boxed vector of Exprs
fn parse_vect(input: &str) -> IResult<&str, Expr> {
    let p = delimited(
        preceded(space0, char('(')),
        separated_list1(preceded(space0, tag(",")), parse_expr),
        preceded(space0, char(')'))
    );
    // match a vect, but be sure that it is NOT terminated w/ an opening curly brace
    // if this is the case, it would clasify as a parameterized abstraction
    // We may have gotten here because the abstraction was invalid, so we should continue to reject
    map(terminated(p,not(tuple((multispace0,char('{'))))), |v: Vec<Expr>| Expr::Vect(v))(input)
}

/// Parses a function application
fn parse_app(input: &str) -> IResult<&str, Expr> {
    let (input,fname) = preceded(space0, verify(name, |s: &str| !is_keyword(s)))(input)?;

    let p = delimited(
        preceded(space0, char('(')),
        separated_list0(preceded(space0, tag(",")), parse_expr),
        preceded(space0, char(')'))
    );

    map(p, |v: Vec<Expr>| Expr::App{fname: fname.to_string(), arguments: v})(input)
}

/// Parses scoped expressions, as part of a function body or parametrized expression
fn parse_scoped_exprs(input: &str) -> IResult<&str, Vec<Expr>> {
    // parse to the end of a line
    let parseToEOL  = preceded(space0, line_ending);
    // parse as many singular line separated exprs as we can find, works as long as there is 1 more expr to parse
    // using many1 to consume multiple linebreaks between exprs, if present
    let pblock      = many0(terminated(parse_expr, terminated(many1(parseToEOL), peek(parse_expr))));
    // parse the block, and then parse the leftover expr as well (we're guaranteed 1 leftover expr at this point, if it's a valid abstraction)
    let pbody       = tuple((pblock,parse_expr));
    let (input,(mut el,e)) = delimited(preceded(multispace0, terminated(char('{'), multispace0)), pbody, preceded(multispace0, char('}')))(input)?;
    // add the straggler expr to our Vec of exprs
    el.push(e);
    Ok((input, el))
}

/// Parses a branch
fn parse_branch(input: &str) -> IResult<&str, Expr> {
    let (input,_)       = preceded(space0, tag("if"))(input)?;
    let (input,cond)    = preceded(space0, parse_expr)(input)?;
    let (input,b1)      = parse_scoped_exprs(input)?;
    let (input,_)       = preceded(space0, tag("else"))(input)?;
    let (input,b2)      = parse_scoped_exprs(input)?;
    return Ok((input, Expr::Branch{condition: Box::new(cond), b1: b1, b2: b2}))
}

/// Parses a parameterized abstraction
/// This is used for function bodies
/// The parameters themselves lack a type here, as they are bound by the context they are assigned within
fn parse_abs(input: &str) -> IResult<&str, Expr> {
    // parse delimited args
    let (input,params)  = delimited(
        preceded(space0, char('(')),
        separated_list0(preceded(space0, tag(",")), name_str),
        preceded(space0, char(')'))
    )(input)?;
    let (input,exprs)   = parse_scoped_exprs(input)?;
    Ok((input, Expr::Abs{params: params, body: exprs}))
}

fn parse_unary_expr(input: &str) -> IResult<&str, Expr> {
    map(
        tuple((parse_unary_op,parse_expr)),
        |(o,e): (UOp, Expr)| Expr::UnaryOp{operator: o, e: Box::new(e)}
    )(input)
}

/// Parses a unary expression
fn parse_unary_op(input: &str) -> IResult<&str, UOp> {
    // recognize a unary operator
    let unary_pairs = [
        ("-",  UOp::Negative),
        ("!", UOp::Negate)
    ];
    let assoc = HashMap::from(unary_pairs);
    // put it ahead of a standard expr

    let keys: Vec<_> = unary_pairs.iter().map(|(s, _): &(&str,UOp)| tag(*s)).collect();
    let parse_uop: (_,_) = keys[0..].iter().collect_tuple().unwrap();

    let (i, out) = preceded(space0, alt(parse_uop))(input)?;
    Ok((i, assoc[out]))
}

/// Parses a branch
fn parse_forloop(input: &str) -> IResult<&str, Expr> {
    let (input,_)           = preceded(space0, tag("for"))(input)?;
    // parse 3 comma separate exprs
    let (input,(e1,_,e2,_,e3)) = delimited(
        preceded(space0, char('(')),
        tuple((
            parse_expr,
            preceded(space0,char(',')),
            parse_expr,
            preceded(space0,char(',')),
            parse_expr
        )),
        preceded(space0, char(')'))
    )(input)?;
    let (input, body)      = parse_scoped_exprs(input)?;
    Ok((input, Expr::For{
        init: Box::new(e1),
        cond: Box::new(e2),
        post: Box::new(e3),
        body: body
    }))
}

/// Parses a vector w/ named indices as a Boxed vector of Exprs
fn parse_named_vect(input: &str) -> IResult<&str, Expr> {
    let p = delimited(
        preceded(space0, char('(')),
        // name : expr
        separated_list1(preceded(space0, tag(",")), separated_pair(preceded(space0, name_str), preceded(space0, tag(":")), parse_expr)),
        preceded(space0, char(')'))
    );
    map(p, |s: Vec<(String,Expr)>| Expr::NamedVect(s))(input)
}
/// parse_binop parses all the binary operators in the language.
fn parse_binop(i: &str) -> IResult<&str, BOp> {
    let (i, _) = space0(i)?;
    // I wanted to use this list of tuple constructtion to make it easier to edit
    // the total number of binary operators. Unfortunately this shows up in the
    // type annotation when we collect into tuples, but that's relatively easy
    // to fix and not prone to mispellings.
    let tups = [
        ("+",  BOp::Add),
        ("-",  BOp::Sub),
        ("*",  BOp::Mul),
        ("/",  BOp::Div),
        ("%",  BOp::Mod),
        ("&&", BOp::And),
        ("||", BOp::Or),
        (".",  BOp::Compose),
        ("==", BOp::Eq),
        ("!=", BOp::Neq),
        (">=", BOp::Gte),
        ("<=", BOp::Lte),
        (">",  BOp::Gt),
        ("<",  BOp::Lt),
        ("&",  BOp::BitAnd),
        ("|",  BOp::BitOr),
        ("^",  BOp::BitXor),
    ];
    let assoc = HashMap::from(tups);

    // Collect the binop tokens into a vector so we can transform them into parsers.
    let keys: Vec<_> = tups.iter().map(|(s, _): &(&str,BOp)| tag(*s)).collect();

    // We need to break this up into two tuples because `collect_tuple` only collects
    // up to 12 element tuples, and we have 17 elements. The reason we need to collect
    // them into tuples is because `alt` only takes tuples. If we could collect up to
    // 17 into a tuple, then this wouldn't be needed.
    let l: (_,_,_,_,_,_,_,_,_) = keys[0..9].iter().collect_tuple().unwrap();
    let r: (_,_,_,_,_,_,_,_)   = keys[9..] .iter().collect_tuple().unwrap();

    // This pattern is recommended if you have >21 parsers to work with, but here
    // we are doing it because we can't collect into a tuple large enough to fit
    // all of the binop parsers
    let (i, out) = alt((alt(l), alt(r)))(i)?;
    Ok((i, assoc[out]))
}

fn parse_binexpr(i: &str) -> IResult<&str, Expr> {
    let (i, _) = space0(i)?;
    map(
        tuple((
            parse_expr,
            parse_binop,
            parse_expr,
        )),
        |(e1, o, e2): (Expr, BOp, Expr)| Expr::BinOp{operator: o, e1: Box::new(e1), e2: Box::new(e2)}
    )(i)
}


/// Parses a standalone expression
/// Represents all possible expansions for parsing exprs
fn parse_expr(input: &str) -> IResult<&str, Expr> {
    alt((
        parse_float,
        parse_double,
        parse_int,
        parse_unary_expr,
        parse_bool,
        parse_comment,
        parse_nested_expr,
        parse_named_vect,
        parse_abs,
        parse_vect,
        parse_return,
        parse_app,
        parse_branch,
        parse_forloop,
        parse_ref,
        // TODO parse forLoop:  for,(parse_expr x 3),{,parse_expr*,}
        //parse_bin_expr,
        //parse_unary_expr,
        // TODO parse binOp:    parse_expr,parse_binop,parse_expr
        // TODO parse def:      parse_ref,':',parse_type,'=',parse_expr
        // TODO parse mutDef:   parse_ref,':','mut',parse_type,'=',parse_expr
        // TODO NO parsing functions...we get them for free w/ def/mutDef
    ))(input)
}


pub fn program(i: &str) -> IResult<&str, &str> {
    function(i)
}

fn function(input: &str) -> IResult<&str, &str> {
    let (input, id) = name(input)?;

    let (input, _) = space1(input)?;

    let (input, params) = parens(input)?;

    let (_, params) = parameters(params)?;

    let (input, _) = delimited(space1, tag("->"), space1)(input)?;

    let (input, rets) = parens(input)?;

    let (_, rets) = separated_list0(tag(", "), name)(rets)?;

    let (input, _) = multispace0(input)?;

    let (input, block) = nested_parens(input, "{", "}")?;

    //let (input, params) = parameters(input);
    println!("Parameters");
    for (a, b) in params {
        println!("{}, {}", a, b);
    }
    println!("Return Values");
    for a in rets {
        println!("{}", a);
    }

    Ok((input, id))
}

fn is_braces(input: char) -> bool {
    match input {
        '{' => true,
        '}' => true,
        _ => false,
    }
}

// TODO This needs to support blocks within blocks
fn nested_parens<'a>(input: &'a str, start: &str, end: &str) -> IResult<&'a str, &'a str> {
    let mut level = 0;
    let chunks: Vec<&str> = Vec::new();
    // From what I understand from the documentation, this does not Err
    // https://docs.rs/nom/latest/nom/bytes/complete/fn.take_till.html
    loop {
        let (input, matched) = many1(alt((is_not(start), is_not(end))))(input)?;
        println!("{}", input);
        for e in matched {
            println!("{}", e);
        }
        if input == "" {
            break;
        }

        //chunks.push(matched);
        match input.chars().nth(0) {
            start => {
                level += 1;
                println!("Open");
            }
            end => {
                level -= 1;
                println!("close");
            }
        }

        if level == 0 {
            break;
        }
    }
    if level != 0 {
        Err(nom::Err::Incomplete(Needed::new(4)))
    } else {
        Ok((input, ""))
    }
}

fn parens(input: &str) -> IResult<&str, &str> {
    delimited(tag("("), is_not(")"), tag(")"))(input)
}

fn parameters(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    let param = separated_pair(name, tag(": "), name);

    separated_list0(tag(", "), param)(input)
}

/****************
TESTS
****************/

/// Tests parsing various ints
#[test]
fn test_parse_ints() {
    assert_eq!(parse_int("32"), Ok(("", Expr::I(32))), "Failed to parse 32");
    assert_eq!(parse_int("0"), Ok(("", Expr::I(0))), "Failed to parse 0");
    assert_eq!(
        parse_int("  1"),
        Ok(("", Expr::I(1))),
        "Failed to parse 1 w/ a space"
    );
    assert_eq!(
        parse_int(" -51  "),
        Ok(("  ", Expr::I(-51))),
        "Failed to parse a negative int w/ spaces"
    );
}

/// Used for testing, returns whether a parse failed
fn verify_parse(p: IResult<&str, Expr>) -> bool {
    match p {
        Ok(_)   => true, // valid parse
        Err(_)  => false // errored out
    }
}

/// Tests parsing various bools
#[test]
fn test_parse_bools() {
    assert_eq!(parse_bool("true"), Ok(("", Expr::B(true))), "Failed to parse 'true'");
    assert_eq!(parse_bool("false"), Ok(("", Expr::B(false))), "Failed to parse 'false'");
    assert_eq!(parse_bool("  true  "), Ok(("  ", Expr::B(true))), "Failed to parse 'true' w/ spaces");
    assert_eq!(parse_bool(" false "), Ok((" ", Expr::B(false))), "Failed to parse 'false' w/ spaces");

    assert_eq!(verify_parse(parse_bool("trues")), false, "Failed to reject bad 'true' value");
    assert_eq!(verify_parse(parse_bool("False")), false, "Failed to reject bad 'false' value");
    assert_eq!(verify_parse(parse_bool("astrues")), false, "Failed to reject bad 'true' value w/ leading text");
}

/// Tests parsing various floats
#[test]
fn test_parse_floats() {
    assert_eq!(parse_float("12.23f"), Ok(("", Expr::F(12.23))), "Failed to parse 12.23");
    assert_eq!(parse_float("  0.98f"), Ok(("", Expr::F(0.98))), "Failed to parse 0.98");
    assert_eq!(parse_float(" -5.2f  "), Ok(("  ", Expr::F(-5.2))), "Failed to parse -5.2");
    assert_eq!(parse_float(" 12").is_ok(), false, "Float parser failed. Parsed an integer, 12");
}

/// Tests parsing various doubles
#[test]
fn test_parse_doubles() {
    assert_eq!(parse_double("12.23"), Ok(("", Expr::D(12.23))), "Failed to parse 12.23");
    assert_eq!(parse_double("  0.98"), Ok(("", Expr::D(0.98))), "Failed to parse 0.98");
    assert_eq!(parse_double(" -5.2  "), Ok(("  ", Expr::D(-5.2))), "Failed to parse -5.2");
    assert_eq!(parse_double(" 12").is_ok(), false, "Double parser failed. Parsed an integer, 12");
    assert!(!parse_double("1.5f").is_ok(), "Double parser did not stop short of parsing a float.");
}

/// Tests parsing various refs
#[test]
fn test_parse_refs() {
    // parse a-z ref
    assert_eq!(parse_expr("a"), Ok(("", Expr::Ref("a".to_string()))), "Failed to parse lowercased ref");
    // parse upper case ref
    assert_eq!(parse_expr("XYZ"), Ok(("", Expr::Ref("XYZ".to_string()))), "Failed to parse uppercased ref");
    // parse mixed ref
    assert_eq!(parse_expr("aBc"), Ok(("", Expr::Ref("aBc".to_string()))), "Failed to parse mixed case ref");
    // parse ref w/ numbers
    assert_eq!(parse_expr("c7171"), Ok(("", Expr::Ref("c7171".to_string()))), "Failed to parse mixed ref w/ numbers");
    // parse ref w/ underscores
    assert_eq!(parse_expr("_c71_71"), Ok(("", Expr::Ref("_c71_71".to_string()))), "Failed to parse mixed ref w/ underscores");
    // verify you cannot parse with leading numbers
    assert!(!parse_expr("1_c71_71").is_ok(), "Failed to reject ref w/ leading digit");
    // verify you cannot parse w/ leading negative
    assert_eq!(parse_expr("-_c71_71"), Ok(("", Expr::UnaryOp{operator: UOp::Negative, e: Box::new(Expr::Ref("_c71_71".to_string()))})), "Failed to reject ref w/ leading dash");
    //assert_eq!(parse_expr("-_c71_71"), Ok(("", i(1))), "Failed to reject ref w/ leading dash");

    // verify you can't parse keywords as refs
    assert!(!parse_expr("if").is_ok(), "Failed to reject 'if' as reserved keyword, not a ref");
    assert!(!parse_expr("for").is_ok(), "Failed to reject 'for' as reserved keyword, not a ref");
}

/// Tests parsing a comment
#[test]
fn test_parse_comment() {
    assert_eq!(parse_expr("//"), Ok(("", Expr::Comment(vec!["".into()]))), "Failed to parse empty comment");
    assert_eq!(parse_expr("// this is a comment"), Ok(("", Expr::Comment(vec![" this is a comment".into()]))), "Failed to parse regular comment");
    assert_eq!(parse_expr(" // ok"), Ok(("", Expr::Comment(vec![" ok".into()]))), "Failed to parse comment w/ leading space");

    // just verify we reject non-comments
    assert!(!parse_comment("f // ok").is_ok(), "Failed to reject a non-comment");

    assert_eq!(parse_expr(" // ok\nand more"), Ok(("\nand more", Expr::Comment(vec![" ok".into()]))), "Didn't stop parsing comment at \n");
}


/// Tests parsing a nested expr w/ parens
#[test]
fn test_parse_nested_expr() {
    assert_eq!(parse_expr("(1)"), Ok(("", Expr::I(1))), "Failed to parse a nested int");
    assert_eq!(parse_expr(" ( true ) "), Ok((" ", Expr::B(true))), "Failed to parse a nested boolean");
    assert_eq!(parse_expr(" (( ((-5) ))) "), Ok((" ", Expr::I(-5))), "Failed to parse a deeply nested int");
    assert!(!parse_expr(" (( ((-5) )) ").is_ok(), "Failed to reject a badly nested expr!");
    assert!(!parse_expr("()").is_ok(), "Failed to reject empty parens w/ no expr");
}


/// Tests parsing various complete expr
#[test]
fn test_parse_expr() {
    // parse int
    assert_eq!(parse_expr("5"), Ok(("", Expr::I(5))), "Failed to parse int as Expr");
    // parse bool
    assert_eq!(parse_expr("false"), Ok(("", Expr::B(false))), "Failed to parse bool as Expr");
    // parse comment
    assert_eq!(parse_expr("//test"), Ok(("", Expr::Comment(vec!["test".into()]))), "Failed to parse comment as Expr");
    // parse nested expr
    assert_eq!(parse_expr("((32))"), Ok(("", Expr::I(32))), "Failed to parse nested expr as Expr");
    // parse vect
    assert_eq!(parse_expr("(1,2,3)"), Ok(("", Expr::Vect(vec![Expr::I(1),Expr::I(2),Expr::I(3)]))), "Failed to parse a simple vect of ints as an expr");
}

/// Tests parsing vectors with indices (no names)
#[test]
fn test_parse_vect() {
    assert_eq!(parse_expr("(1,2,3)"), Ok(("", Expr::Vect(vec![Expr::I(1),Expr::I(2),Expr::I(3)]))), "Failed to parse a simple vect of ints");
    assert_eq!(parse_expr("(1,true)"), Ok(("", Expr::Vect(vec![Expr::I(1),Expr::B(true)]))), "Failed to parse a mixed vect");
    assert_eq!(parse_expr("(3,)").is_ok(), false, "Recognized an invalid tuple statement");
    assert_eq!(parse_expr("(,3)").is_ok(), false, "Recognized another invalid tuple statement");
}

/// Tests parsing vectors with names (akin to structs)
#[test]
fn test_parse_named_vect() {
    // test parsing a named one of 1 item
    assert_eq!(parse_expr("(n:2)"), Ok(("", Expr::NamedVect(vec![("n".to_string(),Expr::I(2))]))), "Failed to parse a tiny named vect");
    // test parsing a named one of 2 items
    assert_eq!(parse_expr("(x:12, y:false)"), Ok(("", Expr::NamedVect(vec![("x".to_string(),Expr::I(12)), ("y".to_string(), Expr::B(false))]))), "Failed to parse a named vect w/ 2 items");
    // test parsing w/ one bad name
    assert_eq!(parse_expr("(x:1, -a:19)").is_ok(), false, "Failed to reject bad name for named vect");
    // test parsing w/ one missing a name
    assert_eq!(parse_expr("(x:1, a:19, 2)").is_ok(), false, "Failed to reject w/ missing name for named vect");
    // test parsing w/ extra comma
    assert_eq!(parse_expr("(a:1, b:19, c:2,)").is_ok(), false, "Failed to reject w/ extra comma at end of named vect");
    // test parsing where name & value are same (ambiguous case that is resolved in type checker w/ an error)
    assert_eq!(parse_expr("(a:a, b:2)"), Ok(("", Expr::NamedVect(vec![("a".to_string(),Expr::Ref("a".to_string())), ("b".to_string(), Expr::I(2))]))), "Failed to accept parsing ambiguous name case");
}

/// Tests parsing a return statement
#[test]
fn test_parse_return() {
    assert_eq!(parse_expr("return true"), Ok(("", Expr::Return(Box::new(Expr::B(true))))), "Failed to parse 'return true");
    assert_eq!(parse_expr("return 1"), Ok(("", Expr::Return(Box::new(Expr::I(1))))), "Failed to parse 'return 1");
    assert_eq!(parse_expr("return (1)"), Ok(("", Expr::Return(Box::new(Expr::I(1))))), "Failed to parse 'return true");
    // constitues a ref instead
    //assert_eq!(parse_expr("returnsdf").is_ok(), false, "Failed to reject 'returnsdf'");
}

/// Tests parsing an immutable def
#[test]
fn test_parse_def() {
    // TODO .....
    assert!(false, "Immutable Def tests not implemented yet!");
}

#[test]
fn test_parse_type_base() {
    assert_eq!(parse_type_base("int"), Ok(("", ParsedType::BaseType("int".to_string()))), "Failed to parse base type.");
}

#[test]
fn test_parse_type_function() {
    assert_eq!(parse_type_function("int -> int"),
               Ok(("",
                   ParsedType::Function(
                       Box::new(ParsedType::BaseType("int".to_string())),
                       Box::new(ParsedType::BaseType("int".to_string())),
                   ))
               ),
               "Failed to parse function type.");
}


#[test]
fn test_parse_type_tuple() {
    assert_eq!(parse_type_tuple("(int, int)"),
               Ok(("",
                   ParsedType::Tuple(
                       vec![
                           Box::new(ParsedType::BaseType("int".to_string())),
                           Box::new(ParsedType::BaseType("int".to_string())),
                       ]
                    )

               )),
               "Failed to parse tuple type"
    );
    assert_eq!(parse_type_tuple("(int, int, bool)"),
               Ok(("",
                   ParsedType::Tuple(
                       vec![
                           Box::new(ParsedType::BaseType("int".to_string())),
                           Box::new(ParsedType::BaseType("int".to_string())),
                           Box::new(ParsedType::BaseType("bool".to_string())),
                       ]
                    )

               )),
               "Failed to parse tuple type"
    );
}

#[test]
fn test_parse_type() {
    assert!(false, "Complex type test not implemented");
}
/// Tests parsing a mutable def
#[test]
fn test_parse_defmut() {
    // TODO .....
    assert!(false, "Mutable Def tests not implemented yet!");
}

// Used to generate a mock expr object for testing
fn app(name: &str, args: Vec<Expr>) -> Expr {
    Expr::App{fname: name.to_string(), arguments: args}
}

fn i(v: i32) -> Expr {
    Expr::I(v)
}

/// Tests parsing func application
#[test]
fn test_parse_app() {
    assert_eq!(parse_expr("a()") , Ok(("", app("a", vec![]))), "Failed to parse basic app");
    assert_eq!(parse_expr("another_name(1,false,2.0,3.3f)") , Ok(("", app("another_name", vec![i(1),Expr::B(false),Expr::D(2.0),Expr::F(3.3)]))), "Failed to parse more complex app");
    assert_eq!(parse_expr("_validFunc(32,f(1))") , Ok(("", app("_validFunc", vec![i(32),app("f", vec![i(1)])]))), "Failed to parse nested app");
}

/// Tests parsing a binop
#[test]
fn test_parse_binop(){
    assert_eq!(parse_binop("+"), Ok(("", BOp::Add)), "Failed to parse Add binop");
    assert_eq!(parse_binop("-"), Ok(("", BOp::Sub)), "Failed to parse Sub binop");
    assert_eq!(parse_binop("*"), Ok(("", BOp::Mul)), "Failed to parse Mul binop");
    assert_eq!(parse_binop("/"), Ok(("", BOp::Div)), "Failed to parse Div binop");
    assert_eq!(parse_binop("%"), Ok(("", BOp::Mod)), "Failed to parse Mod binop");
    assert_eq!(parse_binop("&&"), Ok(("", BOp::And)), "Failed to parse And binop");
    assert_eq!(parse_binop("||"), Ok(("", BOp::Or)), "Failed to parse Or binop");
    assert_eq!(parse_binop("."), Ok(("", BOp::Compose)), "Failed to parse Compose binop");
    assert_eq!(parse_binop("=="), Ok(("", BOp::Eq)), "Failed to parse Eq binop");
    assert_eq!(parse_binop("!="), Ok(("", BOp::Neq)), "Failed to parse Neq binop");
    assert_eq!(parse_binop(">"), Ok(("", BOp::Gt)), "Failed to parse Gt binop");
    assert_eq!(parse_binop(">="), Ok(("", BOp::Gte)), "Failed to parse Gte binop");
    assert_eq!(parse_binop("<"), Ok(("", BOp::Lt)), "Failed to parse Lt binop");
    assert_eq!(parse_binop("<="), Ok(("", BOp::Lte)), "Failed to parse Lte binop");
    assert_eq!(parse_binop("&"), Ok(("", BOp::BitAnd)), "Failed to parse BitAnd binop");
    assert_eq!(parse_binop("|"), Ok(("", BOp::BitOr)), "Failed to parse BitOr binop");
    assert_eq!(parse_binop("^"), Ok(("", BOp::BitXor)), "Failed to parse BitXor binop");
}

#[test]
fn test_parse_binexp() {
    assert_eq!(parse_binexpr("1 + 2").is_ok(),true, "Failed to parse binary expresssion");
    assert_eq!(parse_binexpr("1+ 2").is_ok(),true, "Failed to parse binary expresssion");
    assert_eq!(parse_binexpr("1 +2").is_ok(),true, "Failed to parse binary expresssion");
    assert_eq!(parse_binexpr("1+2").is_ok(),true, "Failed to parse binary expresssion");
}

/// Tests parsing a branch
#[test]
fn test_parse_branch() {
    let mut b = Expr::Branch{condition: Box::new(Expr::B(true)), b1: vec![i(1)], b2: vec![i(2)]};
    assert_eq!(parse_expr("if true { 1 } else { 2 }"), Ok(("", b)), "Failed to parse branch");
    let mut b = Expr::Branch{condition: Box::new(app("verify", vec![Expr::B(false),Expr::B(true)])), b1: vec![i(1),i(2),Expr::B(true)], b2: vec![i(2),Expr::B(true)]};
    assert_eq!(parse_expr("if verify(false,true) { 1\n2\ntrue } else { 2\ntrue }"), Ok(("", b)), "Failed to parse more complex branch");
    assert_eq!(parse_expr("if oops(1,2) { 5 } else { 55").is_ok(), false, "Failed to discard badly formatted branch");

    // TODO TESTING
    // let mut b = Expr::Branch{condition: Box::new(Expr::B(true)), b1: vec![i(1)], b2: vec![i(2)]};
    // assert_eq!(parse_expr("if oops(1,2) { 5 } else { 55"), Ok(("",b)), "Failed to discard badly formatted branch");
}

/// Tests parametrized abstractions
#[test]
fn test_parse_abs() {
    let mut a = Expr::Abs{params: vec![], body: vec![i(5)]};
    assert_eq!(parse_expr("() { 5 }") , Ok(("", a)), "Failed to parse a simple abstraction");

    let mut a = Expr::Abs{params: vec!["x".to_string(), "y".to_string()], body: vec![Expr::Ref("x".to_string()),Expr::Ref("y".to_string())]};
    assert_eq!(parse_expr("(x,y) { x \n y\n }") , Ok(("", a)), "Failed to parse a more complex abstraction");

    // test w/ no break between exprs (invalid)
    assert_eq!(parse_expr("(x,y) { x y }").is_ok(), false, "Failed to reject poorly formatted abstraction body");

    // test an abstraction w/ many linebreaks between exprs, and elsewhere too
    let mut a = Expr::Abs{params: vec!["x".to_string(), "y".to_string()], body: vec![Expr::Ref("x".to_string()),Expr::Ref("y".to_string())]};
    assert_eq!(parse_expr("(x,y) \n { \n x \n\n\n y }") , Ok(("", a)), "Failed to parse a more complex abstraction");
}

/// Tests parsing for loops
#[test]
fn test_parse_forloop() {
    let mut b = Expr::For{init: Box::new(i(1)), cond: Box::new(i(2)), post: Box::new(i(3)), body: vec![Expr::Ref("x".to_string())]};
    assert_eq!(parse_expr("for(1,2,3){ x }"), Ok(("", b)), "Failed to parse simple forloop");

    // test a for loop w/ multiple exprs
    let mut b = Expr::For{init: Box::new(Expr::B(true)), cond: Box::new(Expr::B(false)), post: Box::new(Expr::F(3.0)), body: vec![app("f",vec![i(1)]), app("f2", vec![]), app("f3", vec![])]};
    assert_eq!(parse_expr("for( true , false , 3.0f ) {\n\tf(1)\n\tf2()\n\tf3() }"), Ok(("", b)), "Failed to parse larger forloop");
}

/// Tests parsing of unary exprs
#[test]
fn test_parse_unaryexpr() {
    // verify numbers parse as regular negative, no extra stuff
    assert_eq!(parse_expr("-1") , Ok(("", i(-1))), "Failed to parse a negative int normally");

    // verify refs parse w/ unary ops
    assert_eq!(parse_expr("-a") , Ok(("", Expr::UnaryOp{operator: UOp::Negative, e: Box::new(Expr::Ref("a".to_string()))})), "Failed to parse a negative reference");

}

/// Tests parsing an vect access by name
#[test]
fn test_parse_access_name() {
    // TODO .....
    // x['y']
    assert!(false, "Access by Name tests not implemented yet!");
}

/// Tests parsing an vect access by index
#[test]
fn test_parse_access_index() {
    // TODO .....
    // x[0]
    assert!(false, "Access by Index tests not implemented yet!");
}

/// Tests parsing a func w/ body
#[test]
fn test_parse_func() {
    // TODO .....
    assert!(false, "Function parsing tests not implemented yet!");
}
