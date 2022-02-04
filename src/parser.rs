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
    one_of,
};
use nom::bytes::complete::{
    tag,
    is_not,
    is_a,
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
    many1,
    fold_many1,
};
use nom::error;
use nom::{Err, Needed};
use nom::branch::alt;
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

    let (input, block ) = nested_parens(input, "{", "}")?;

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


fn is_braces(input: char) -> bool {
    match input {
        '{' => true,
        '}' => true,
        _   => false,
    }
}

// TODO This needs to support blocks within blocks
fn nested_parens<'a>(input: &'a str, start: &str, end: &str) -> IResult<&'a str, &'a str> {
    let mut level = 0;
    let chunks: Vec<&str> = Vec::new();
    // From what I understand from the documentation, this does not Err
    // https://docs.rs/nom/latest/nom/bytes/complete/fn.take_till.html
    loop {
        let (input, matched) =  many1(alt((is_not(start), is_not(end))))(input)?;
        println!("{}", input);
        for e in matched {
            println!("{}", e);
        }
        if input == "" { break; }

        //chunks.push(matched);
        match input.chars().nth(0) {
            start => {
                level += 1;
                println!("Open");
            },
            end => {
                level -= 1;
                println!("close");
            },
        }

        if level == 0 { break; }
    }
    if level != 0 {
        Err(nom::Err::Incomplete(Needed::new(4)))
    }
    else {Ok((input, ""))}
}
fn parens(input: &str) -> IResult<&str, &str> {
    delimited(tag("("), is_not(")"), tag(")"))(input)
}
fn parameters(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    let param = separated_pair(name!(), tag(": "), name!());

    separated_list0(
        tag(", "),
        param
    )(input)
}
