use crate::syntax;

use syntax::Expr;
use syntax::ParsedType;
use syntax::AccessType;
use syntax::BOp;
use syntax::UOp;
extern crate itertools;
use itertools::Itertools;

extern crate nom;

use nom::bytes::complete::{tag};
use nom::character::complete::{char, alpha1, alphanumeric1, i32, multispace0, space0, space1, line_ending, not_line_ending, digit1};
use nom::{IResult};
use nom::character::is_alphabetic;
use nom::branch::alt;
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::sequence::{preceded, delimited, terminated, separated_pair, tuple, pair};
use nom::number::complete::{float, double};
use nom::combinator::{map, verify, recognize, peek, opt, fail, not, eof};


#[macro_export]
macro_rules! bop {
    ( $i:expr , $b:expr) => {
        {
            map(tag($i), |s: &str| $b)
        }
    };
}

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

/// Parses a reference, which is defined by the `name` parser, see `name`
fn parse_ref(i: &str) -> IResult<&str, Expr> {
    map(preceded(space0, verify(name, |s : &str| !is_keyword(s))), |s: &str| Expr::Ref(s.to_string()))(i)
}


/// Parses named access of a reference
fn parse_named_access(i: &str) -> IResult<&str, Expr> {
    // parse name, followed by '.' and then another name
    let (i,(n,_,a)) = tuple((
        preceded(space0, name),
        preceded(space0, tag(".")),
        preceded(space0, name)
    ))(i)?;
    Ok((i, Expr::Access(n.to_string(), AccessType::Name(a.to_string()))))
}


/// Parses indexed access of a reference
fn parse_index_access(i: &str) -> IResult<&str, Expr> {
    // parse name followed by an expr wrapped in square brackets
    let (i,(n,e)) = tuple((
        preceded(space0, name),
        delimited(
            preceded(space0, tag("[")),
            parse_expr,
            preceded(space0, tag("]"))
        )
    ))(i)?;
    Ok((i, Expr::Access(n.to_string(), AccessType::Idx(Box::new(e)))))
}


/// Parses an immutable definition
/// Constitutes a binding of a name to an expr
fn parse_def(input: &str) -> IResult<&str, Expr> {
    let (input, (_,name,_,_,_,t,_,_,expr)) = tuple((delimited(space0,tag("let"),space1), name_str, space0, char(':'), space0, parse_type, space0, char('='), parse_expr))(input)?;
    Ok((input, Expr::Def{name: name, typ: t, value: Box::new(expr)}))
}

/// Parses a mutable definition
/// Constitutes a dynamic binding of a name to an expr, which can be changed at runtime
fn parse_mutdef(input: &str) -> IResult<&str, Expr> {
    let (input, (_,name,_,_,_,t,_,_,expr)) = tuple((delimited(space0,tag("mut"),space1), name_str, space0, char(':'), space0, parse_type, space0, char('='), parse_expr))(input)?;
    Ok((input, Expr::DefMut{name: name, typ: t, value: Box::new(expr)}))
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

    let c : char = (*i).chars().nth(0).unwrap();

    if is_alphabetic(c as u8) || c == '_' {
        // parse a base type
        let (i,t) = parse_type_base(i)?;
        let (i,_) = space0(i)?;

        let c1 = (*i).chars().nth(0);
        let c2 = (*i).chars().nth(1);

        match (c1,c2) {
            (Some('-'),Some('>')) => {
                // parse again, and build function type w/ these 2 types
                let (i,t2) = preceded(tuple((space0,tag("->"))), parse_type)(i)?;
                Ok((
                    i,
                    ParsedType::Function(
                        Box::new(t),
                        Box::new(t2)
                    )
                ))
            }
            (_,_)                 => Ok((i,t))
        }

    } else if c == '(' {
        // parse a tuple type
        let (i,t) = parse_type_tuple(i)?;
        let (i,_) = space0(i)?;

        let c1 = (*i).chars().nth(0);
        let c2 = (*i).chars().nth(1);

        match (c1,c2) {
            (Some('-'),Some('>')) => {
                // parse again, and build function type w/ these 2 types
                let (i,t2) = preceded(tuple((space0,tag("->"))), parse_type)(i)?;
                Ok((
                    i,
                    ParsedType::Function(
                        Box::new(t),
                        Box::new(t2)
                    )
                ))
            }
            (_,_)                 => Ok((i,t))
        }

    } else {
        fail(i)
    }
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
    let parse_to_eol    = preceded(space0, line_ending);
    // parse as many singular line separated exprs as we can find, works as long as there is 1 more expr to parse
    // using many1 to consume multiple linebreaks between exprs, if present
    let pblock          = many0(terminated(parse_expr, terminated(many1(parse_to_eol), peek(parse_expr))));
    // parse the block, and then parse the leftover expr as well (we're guaranteed 1 leftover expr at this point, if it's a valid abstraction)
    let pbody           = tuple((pblock,parse_expr));
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
    preceded(space0, alt((
        map(tag("-"), |s: &str| UOp::Negative),
        map(tag("!"), |s: &str| UOp::Negate),
    )))(input)
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
    preceded(space0, alt((
        bop!("&&", BOp::And),
        bop!("||", BOp::Or),
        bop!("&",  BOp::BitAnd),
        bop!("|",  BOp::BitOr),
        bop!("+",  BOp::Add),
        bop!("-",  BOp::Sub),
        bop!("*",  BOp::Mul),
        bop!("/",  BOp::Div),
        bop!("%",  BOp::Mod),
        bop!(".",  BOp::Compose),
        bop!("==", BOp::Eq),
        bop!("!=", BOp::Neq),
        bop!(">=", BOp::Gte),
        bop!("<=", BOp::Lte),
        bop!(">",  BOp::Gt),
        bop!("<",  BOp::Lt),
        bop!("^",  BOp::BitXor),
    )))(i)

}


/// Parses a standalone expression
/// Represents all possible expansions for parsing exprs
fn parse_expr(input: &str) -> IResult<&str, Expr> {
    let (i,e) = alt((
        parse_float,
        parse_double,
        parse_int,

        parse_unary_expr,
        parse_bool,
        parse_comment,
        parse_named_vect,

        // Abs vs. Nested Exprs are position sensitive
        // Nested Exprs must be checked after Abs, or abstractions will
        // be mangled before they can be checked
        parse_abs,
        parse_nested_expr,
        parse_vect,

        parse_return,
        parse_app,
        parse_branch,
        parse_forloop,

        parse_def,
        parse_mutdef,
        parse_named_access,
        parse_index_access,
        parse_ref,
    ))(input)?;

    // Check to handle a potential binary expr afterwards
    let p_bin = preceded(space0, parse_binop)(i);

    if p_bin.is_ok() {
        // decompose as a bin op
        let (i,op) = p_bin?;
        // parse another expr to append
        let (i,e2) = parse_expr(i)?;

        Ok((i,Expr::BinOp{operator: op, e1: Box::new(e), e2: Box::new(e2)}))

    } else {
        // no bin op, return as is
        Ok((i,e))
    }
}

/// Parses an FDSSL program, returning a vector of exprs
pub fn program(i: &str) -> IResult<&str, Vec<Expr>> {
    // parse 1 or more exprs, then new line, until end of program

    // parse expr terminated by ...
    // many1(terminated(parse_expr, alt((
    //     // immediate EOF, allows us to enter a total accept state
    //     tuple((space0, eof)),
    //     // many spaces & line breaks followed by EOF
    //     // also allows us to enter a total accept state
    //     tuple((many1(tuple((space0, line_ending))), eof)),
    //     // normal end of line
    //     tuple((space0, line_ending))
    // ))))(i)


    /*
    1st try to parse many exprs followed by
    2nd try to parse to a line break
    3rd try to
    */

    terminated(
        many1(terminated(parse_expr, tuple((space0, line_ending)))),
        tuple((multispace0, eof))
        //many0(tuple((space0, line_ending)))
    )(i)
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
    // TODO dropping this test since we implicitly check for floats before doubles, this case won't come up (@montymxb)
    //assert!(!parse_double("1.5f").is_ok(), "Double parser did not stop short of parsing a float.");
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
    //assert_eq!(parse_expr("1_c71_71"), Ok(("", Expr::Ref("_c71_71".to_string()))), "Failed to reject ref w/ leading digit");
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

/// Helper function to build a simple parsed type for testing
fn ptype(name: &str) -> ParsedType {
    ParsedType::BaseType(name.to_string())
}

/// Used during testing to build an immutable def
fn def(name: &str, typ: ParsedType, val: Expr) -> Expr {
    Expr::Def{
        name: name.to_string(),
        typ: typ,
        value: Box::new(val)
    }
}

/// Used during testing to build a mutable def
fn mdef(name: &str, typ: ParsedType, val: Expr) -> Expr {
    Expr::DefMut{
        name: name.to_string(),
        typ: typ,
        value: Box::new(val)
    }
}

/// Tests parsing an immutable def
#[test]
fn test_parse_def() {
    assert_eq!(
        parse_expr("let a : int = 2"),
        Ok(("", def("a", ptype("int"), i(2)))),
        "Failed to parse simple def"
    );

}

#[test]
fn test_parse_type_base() {
    assert_eq!(parse_type_base("int"), Ok(("", ptype("int"))), "Failed to parse base type.");
}

#[test]
fn test_parse_type_function() {
    // test a simple function type
    assert_eq!(parse_type("int -> int"),
               Ok(("",
                   ParsedType::Function(
                       Box::new(ptype("int")),
                       Box::new(ptype("int")),
                   ))
               ),
               "Failed to parse function type.");
    // test a complex function type
    assert_eq!(
        parse_type("(int,bool) -> (A, B -> (C,D -> F))"),
        Ok(("", ParsedType::Function(
            Box::new(ParsedType::Tuple(vec![Box::new(ptype("int")), Box::new(ptype("bool"))])),
            Box::new(ParsedType::Tuple(vec![Box::new(ptype("A")), Box::new(ParsedType::Function(
                Box::new(ptype("B")),
                Box::new(ParsedType::Tuple(vec![Box::new(ptype("C")), Box::new(ParsedType::Function(
                    Box::new(ptype("D")),
                    Box::new(ptype("F"))))]
                ))
            ))]))
        ))),
        "Failed to parse basic type"
    );
}


#[test]
fn test_parse_type_tuple() {
    assert_eq!(parse_type_tuple("(int, int)"),
               Ok(("",
                   ParsedType::Tuple(
                       vec![
                           Box::new(ptype("int")),
                           Box::new(ptype("int")),
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


/// Tests parsing a mutable def
#[test]
fn test_parse_defmut() {
    assert_eq!(
        parse_expr("mut a : int = 2"),
        Ok(("", mdef("a", ptype("int"), i(2)))),
        "Failed to parse simple def"
    );
}

// Used to generate a mock expr object for testing
fn app(name: &str, args: Vec<Expr>) -> Expr {
    Expr::App{fname: name.to_string(), arguments: args}
}

// Used to quickly wrap an integer for use
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
    assert_eq!(
        parse_expr("1 + 2"),
        Ok(("", Expr::BinOp{
            operator: BOp::Add,
            e1: Box::new(Expr::I(1)),
            e2: Box::new(Expr::I(2))
        })),
        "Failed to parse binary expresssion");
    assert_eq!(parse_expr("1+ 2").is_ok(),true, "Failed to parse binary expresssion");
    assert_eq!(parse_expr("1 +2").is_ok(),true, "Failed to parse binary expresssion");
    assert_eq!(parse_expr("1+2").is_ok(),true, "Failed to parse binary expresssion");
}

/// Tests parsing a branch
#[test]
fn test_parse_branch() {
    let mut b = Expr::Branch{condition: Box::new(Expr::B(true)), b1: vec![i(1)], b2: vec![i(2)]};
    assert_eq!(parse_expr("if true { 1 } else { 2 }"), Ok(("", b)), "Failed to parse branch");
    let mut b = Expr::Branch{condition: Box::new(app("verify", vec![Expr::B(false),Expr::B(true)])), b1: vec![i(1),i(2),Expr::B(true)], b2: vec![i(2),Expr::B(true)]};
    assert_eq!(parse_expr("if verify(false,true) { 1\n2\ntrue } else { 2\ntrue }"), Ok(("", b)), "Failed to parse more complex branch");
    assert_eq!(parse_expr("if oops(1,2) { 5 } else { 55").is_ok(), false, "Failed to discard badly formatted branch");
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
    // simple access
    assert_eq!(parse_expr("x.y"), Ok(("", Expr::Access("x".to_string(), AccessType::Name("y".to_string())))), "Failed to parse 1st simple named access.");
    assert_eq!(parse_expr("x._y"), Ok(("", Expr::Access("x".to_string(), AccessType::Name("_y".to_string())))), "Failed to parse 2nd simple named access.");
    assert_eq!(parse_expr("_X._Y"), Ok(("", Expr::Access("_X".to_string(), AccessType::Name("_Y".to_string())))), "Failed to parse 3rd simple named access.");
}

/// Tests parsing an vect access by index
#[test]
fn test_parse_access_index() {
    assert_eq!(parse_expr("x[0]"), Ok(("", Expr::Access("x".to_string(), AccessType::Idx(Box::new(i(0)))))), "Failed to parse literal int for indexed access.");
    assert_eq!(parse_expr("x[_y]"), Ok(("", Expr::Access("x".to_string(), AccessType::Idx(Box::new(Expr::Ref("_y".to_string())))))), "Failed to parse ref for indexed access.");
    assert_eq!(parse_expr("xY_z[_y + 1]"), Ok(("", Expr::Access(
        "xY_z".to_string(),
        AccessType::Idx(Box::new(Expr::BinOp{
            operator: BOp::Add,
            e1:       Box::new(Expr::Ref("_y".to_string())),
            e2:       Box::new(Expr::I(1))
        }))))), "Failed to parse binary expr for indexed access.");
}

/// Tests parsing a func w/ body
#[test]
fn test_parse_func() {

    // test func w/ no params
    assert_eq!(
        parse_expr("let f1 : bool = () { true }"),
        Ok((
            "",
            Expr::Def{
                name:   "f1".to_string(),
                typ:    ptype("bool"),
                value:  Box::new(Expr::Abs{
                    params: vec![],
                    body:   vec![Expr::B(true)]
                })
            }
        )),
        "Failed to parse f1 boolean function w/ no args"
    );

    assert_eq!(
        parse_expr("let id : int -> int = (x) { x }"),
        Ok((
            "",
            Expr::Def{
                name:   "id".to_string(),
                typ:    ParsedType::Function(Box::new(ptype("int")), Box::new(ptype("int"))),
                value:  Box::new(Expr::Abs{
                    params: vec!["x".to_string()],
                    body:   vec![Expr::Ref("x".to_string())]
                })
            }
        )),
        "Failed to parse id 'int' function"
    );

    // add test
    assert_eq!(
        parse_expr("let add : (int,int) -> int = (_x,y) {\n_x + y\n}"),
        Ok((
            "",
            Expr::Def{
                name:   "add".to_string(),
                // (int,int) -> int
                typ:    ParsedType::Function(
                    Box::new(ParsedType::Tuple(
                        vec![Box::new(ptype("int")),
                        Box::new(ptype("int"))])),
                    Box::new(ptype("int"))),
                // _x + y
                value:  Box::new(Expr::Abs{
                    params: vec!["_x".to_string(), "y".to_string()],
                    body:   vec![Expr::BinOp{
                        operator: BOp::Add,
                        e1:       Box::new(Expr::Ref("_x".to_string())),
                        e2:       Box::new(Expr::Ref("y".to_string())),
                    }]
                })
            }
        )),
        "Failed to parse inc function"
    )
}
