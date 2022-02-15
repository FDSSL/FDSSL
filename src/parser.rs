use crate::syntax;

use syntax::Expr;

extern crate nom;

use nom::bytes::complete::{is_not, tag};

use nom::character::complete::{char, alpha1, i32, multispace0, space0, space1, not_line_ending};

use nom::{Needed};
use nom::{IResult};

use nom::branch::alt;
use nom::multi::{many1, separated_list0};

use nom::sequence::{preceded, delimited, separated_pair};

use nom::combinator::{map, verify};

// pub struct Program {
//     main: Expr,
//     functions: Vec<Expr>
// }

/// Parses any valid identifier
fn name(i: &str) -> IResult<&str, &str> {
    alpha1(i)
  // match alpha1::<&str, ParseError>(i) {
  //     Ok(t) => return Ok(t),
  //     _     => return Err(Error(ParseError::Basic))
  // }
}

/// Parses an int, corresponding to a 32-bit int in Rust.
/// Can be surrounded by spaces on either end
fn parse_int(input: &str) -> IResult<&str, Expr> {
    map(preceded(space0, i32), |i: i32| Expr::I(i))(input)
}

/// Parses a Boolean value w/ optional leading space
fn parse_bool(input: &str) -> IResult<&str, Expr> {
    let vp1 = verify(preceded(space0, name), |s: &str| s == "true" || s == "false");
    return map(vp1, |s: &str| if s == "true" { Expr::B(true) } else { Expr::B(false) })(input);
}

/// Parses a comment, which is a valid element in our abstract syntax
/// Comments are transformed from FDSSL to match the equivalent GLSL produced
fn parse_comment(input: &str) -> IResult<&str, Expr> {
    // match a '//' up to the end of the line
    // then, take a comment all the way to the end, ignoring the opener
    let comment = preceded(preceded(space0, tag("//")), not_line_ending);
    // map what we found into a valid comment
    return map(comment, |s: &str| Expr::Comment(vec![s.into()]) )(input);
}

/// Parses a parenthetically nested expression
fn parse_nested_expr(input: &str) -> IResult<&str, Expr> {
    return delimited(
        preceded(space0, char('(')),
        parse_expr,
        preceded(space0, char(')')),
    )(input);
}

/// Parses a standalone expression
/// Represents all possible expansions for parsing exprs
fn parse_expr(input: &str) -> IResult<&str, Expr> {
    return alt((
        parse_int,
        parse_bool,
        parse_comment,
        parse_nested_expr
    ))(input);
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
    // TODO .....
    assert!(false, "Float tests not implemented yet!");
}

/// Tests parsing various doubles
#[test]
fn test_parse_doubles() {
    // TODO .....
    assert!(false, "Double tests not implemented yet!");
}

/// Tests parsing various refs
#[test]
fn test_parse_refs() {
    // TODO .....
    assert!(false, "Ref tests not implemented yet!");
}

/// Tests parsing a comment
#[test]
fn test_parse_comment() {
    assert_eq!(parse_comment("//"), Ok(("", Expr::Comment(vec!["".into()]))), "Failed to parse empty comment");
    assert_eq!(parse_comment("// this is a comment"), Ok(("", Expr::Comment(vec![" this is a comment".into()]))), "Failed to parse regular comment");
    assert_eq!(parse_comment(" // ok"), Ok(("", Expr::Comment(vec![" ok".into()]))), "Failed to parse comment w/ leading space");
    assert_eq!(verify_parse(parse_comment("f // ok")), false, "Failed to reject a non-comment");
    assert_eq!(parse_comment(" // ok\nand more"), Ok(("\nand more", Expr::Comment(vec![" ok".into()]))), "Didn't stop parsing comment at \n");
}


/// Tests parsing a nested expr w/ parens
#[test]
fn test_parse_nested_expr() {
    assert_eq!(parse_nested_expr("(1)"), Ok(("", Expr::I(1))), "Failed to parse a nested int");
    assert_eq!(parse_nested_expr(" ( true ) "), Ok((" ", Expr::B(true))), "Failed to parse a nested boolean");
    assert_eq!(parse_nested_expr(" (( ((-5) ))) "), Ok((" ", Expr::I(-5))), "Failed to parse a deeply nested int");
    assert_eq!(verify_parse(parse_nested_expr(" (( ((-5) )) ")), false, "Failed to reject a badly nested expr!");
    assert_eq!(verify_parse(parse_nested_expr("()")), false, "Failed to reject empty parens w/ no expr");
}


/// Tests parsing a type annotation (a name)
#[test]
fn test_parse_typeAnnotation() {
    // TODO .....
    // i : int = 2  <--- we want to recognize 'int'
    // v : ivect = (1,2,3) <-- integer vect, w/e
    assert!(false, "Type annotation tests not implemented yet!");
}

/// Tests parsing an expr
#[test]
fn test_parse_expr() {
    // TODO .....
    assert!(false, "General expr tests not implemented yet");
}

/// Tests parsing vectors
#[test]
fn test_parse_vect() {
    // TODO .....
    // v : ivect = (1,2,3)
    // special 'parseVect' parser, should use defParser (immutable)
    assert!(false, "Vector tests not implemented yet!");
}

/// Tests parsing a return statement
#[test]
fn test_parse_return() {
    // TODO .....
    // return <expr>, needs to use parseExpr with name 'return'
    assert!(false, "Return keyword tests not implemented yet!");
}

/// Tests parsing an immutable def
#[test]
fn test_parse_def() {
    // TODO .....
    assert!(false, "Immutable Def tests not implemented yet!");
}

/// Tests parsing a mutable def
#[test]
fn test_parse_defmut() {
    // TODO .....
    assert!(false, "Mutable Def tests not implemented yet!");
}

/// Tests parsing func application
#[test]
fn test_parse_app() {
    // TODO .....
    // name(expr1, expr2, ...)
    assert!(false, "Func. App tests not implemented yet!");
}

/// Tests parsing a binop
#[test]
fn test_parse_binop() {
    // TODO .....
    // +,-,/,*, etc...
    assert!(false, "BinOp tests not implemented yet!");
}

/// Tests parsing a branch
#[test]
fn test_parse_branch() {
    // TODO .....
    assert!(false, "Branch tests not implemented yet!");
}

/// Tests parsing a for loop
#[test]
fn test_parse_forloop() {
    // TODO .....
    assert!(false, "For loop tests not implemented yet!");
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
