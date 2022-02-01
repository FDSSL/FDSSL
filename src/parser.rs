//use syntax::Expr;
//mod syntax;
extern crate nom;
use nom::{
    IResult,
    Parser,
};
use nom::character::complete::{
    alpha1,
    space1,
    space0,
    multispace0,
};
use nom::bytes::complete::{
    tag,
    is_not,
    take_until,
};

use nom::sequence::{
    delimited,
    separated_pair,
    tuple,
    terminated,
};
use nom::multi::{
    separated_list0,
    many0,
};

// pub struct Program {
//     main: Expr,
//     functions: Vec<Expr>
// }

macro_rules! name {
    () => {alpha1}
}

pub fn program(i: &str) -> IResult<&str, &str> {
    function(i)
}

fn function(input: &str) -> IResult<&str, &str>{
    let (input, id    ) = name!()(input)?;

    let (input, _     ) = space1(input)?;

    let (input, params) = parens(input)?;

    let (_    , params) = parameters(params)?;

    let (input, _     ) = delimited(space1, tag("->"), space1)(input)?;

    let (input, rets  ) = parens(input)?;

    let (_    , rets  ) = separated_list0(tag(", "), name!())(rets)?;

    let (input, _     ) = multispace0(input)?;

    let (input, block ) = body(input)?;

    //let (input, params) = parameters(input);
    println!("Parameters");
    for (a, b) in params {
        println!("{}, {}", a, b);
    }
    println!("Return Values");
    for a in rets {
        println!("{}", a);
    }

    Ok((input,id))
}
// TODO This needs to be explicit to support sub expressions
fn parens(input: &str) -> IResult<&str, &str> {
    delimited(tag("("), is_not(")"), tag(")"))(input)
}
// TODO This needs to support blocks within blocks
fn body(input: &str) -> IResult<&str, &str> {
    delimited(tag("{"), is_not("}"), tag("}"))(input)
}

fn parameters(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    let param = separated_pair(name!(), tag(": "), name!());

    separated_list0(
        tag(", "),
        param
    )(input)
}
