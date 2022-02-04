//use syntax::Expr;
//mod syntax;
extern crate nom;
use nom::bytes::complete::{is_a, is_not, tag, take_until};
use nom::character::complete::{alpha1, digit1, i32, multispace0, one_of, space0, space1};
use nom::{IResult, Parser};

use nom::branch::alt;
use nom::error;
use nom::multi::{fold_many1, many0, many1, separated_list0};
use nom::sequence::{delimited, separated_pair, terminated, tuple};
use nom::{Err, Needed};
// pub struct Program {
//     main: Expr,
//     functions: Vec<Expr>
// }

macro_rules! name {
    () => {
        alpha1
    };
}

/// Parses an int, corresponding to a 32-bit int in Rust.
/// Can be surrounded by spaces on either end
fn parseInt(input: &str) -> IResult<&str, i32> {
    let (i, _) = space0(input)?;
    return i32(i);
}

pub fn program(i: &str) -> IResult<&str, &str> {
    function(i)
}

fn function(input: &str) -> IResult<&str, &str> {
    let (input, id) = name!()(input)?;

    let (input, _) = space1(input)?;

    let (input, params) = parens(input)?;

    let (_, params) = parameters(params)?;

    let (input, _) = delimited(space1, tag("->"), space1)(input)?;

    let (input, rets) = parens(input)?;

    let (_, rets) = separated_list0(tag(", "), name!())(rets)?;

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
    let param = separated_pair(name!(), tag(": "), name!());

    separated_list0(tag(", "), param)(input)
}

/****************
TESTS
****************/

/// Tests parsing various ints
#[test]
fn test_parse_ints() {
    assert_eq!(parseInt("32"), Ok(("", 32)), "Failed to parse 32");
    assert_eq!(parseInt("0"), Ok(("", 0)), "Failed to parse 0");
    assert_eq!(parseInt("  1"), Ok(("", 1)), "Failed to parse 1 w/ a space");
    assert_eq!(
        parseInt(" -51  "),
        Ok(("  ", -51)),
        "Failed to parse a negative int w/ spaces"
    );
}

/// Tests parsing various bools
#[test]
fn test_parse_bools() {
    // TODO .....
    assert!(false, "Bool tests not implemented yet!");
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
    // TODO .....
    assert!(false, "Comment parsing tests not implemented yet!");
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
