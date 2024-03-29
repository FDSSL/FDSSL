use std::fmt;

#[derive(Debug,PartialEq)]
pub enum Type {
    Uint,
    Int,
    Bool,
    Float,
    Double,
    Tuple(Box<Type>, Box<Type>),
    Struct(String),
    Function(Box<Type>, Box<Type>),
    Opaque(String),
}

/// ParsedType is a structural type enum used during parsing.
///
/// This enum is not meant to be used outside the parser and type checker
/// as it simply denotes the structure of the type rather than any type
/// space the type occupies.
#[derive(Debug, PartialEq, Clone)]
pub enum ParsedType {
    BaseType(String),
    Tuple(Vec<ParsedType>),
    NamedTuple(Vec<(String, Box<ParsedType>)>), // stores indexed types for named tuples
    Function(Box<ParsedType>, Box<ParsedType>),
}

/// User friendly dipslyaing of parsed types in TypeChecker errors
impl fmt::Display for ParsedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self {
            ParsedType::BaseType(s)     => write!(f, "{}", s),
            ParsedType::Tuple(v)        => {
                write!(f, "(");
                for (i,t) in v.into_iter().enumerate() {
                    write!(f, "{}", t);
                    if i < (v.len()-1) {
                        write!(f, ", ");
                    }
                }
                write!(f, ")")
            },
            ParsedType::Function(t1,t2) => write!(f, "{} -> {}", format!("{}",t1), format!("{}",t2)),
            ParsedType::NamedTuple(v)   => {
                write!(f, "(");
                for (i,t) in v.into_iter().enumerate() {
                    write!(f, "{}:{}", t.0, *t.1);
                    if i < (v.len()-1) {
                        write!(f, ", ");
                    }
                }
                write!(f, ")")
                // write!(f, "({})", format!("{:?}",v))
            }
        }
    }
}

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum BOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Compose,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    BitAnd,
    BitOr,
    BitXor,
}

impl fmt::Display for BOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BOp::Add => "+",
            BOp::Sub => "-",
            BOp::Mul => "*",
            BOp::Div => "/",
            BOp::Mod => "%",
            BOp::And => "&&",
            BOp::Or => "||",
            BOp::Compose => ".",
            BOp::Eq => "==",
            BOp::Neq => "!=",
            BOp::Gt => ">",
            BOp::Gte => ">=",
            BOp::Lt => "<",
            BOp::Lte => "<=",
            BOp::BitAnd => "&",
            BOp::BitOr => "|",
            BOp::BitXor => "^",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum UOp {
    Negative,
    Negate
}

impl fmt::Display for UOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            UOp::Negative   => write!(f, "-"),
            UOp::Negate     => write!(f, "!")
        }
    }
}

#[derive(Debug,PartialEq)]
pub struct Parameter {
    name: String,
    datatype: Type,
}

#[derive(Debug,PartialEq)]
pub enum AccessType {
    Name(String),
    Idx(Box<Expr>)
}

#[derive(Debug,PartialEq)]
pub enum Expr {
    I(i32),
    B(bool),
    F(f32),
    D(f64),
    Ref(String),
    Return(Box<Expr>),
    Vect(Vec<Expr>),
    NamedVect(Vec<(String,Expr)>),
    // Vect {
    //     value: Vec<Box<Expr>>,
    // },
    Def {
        name: String,
        typ: ParsedType,
        value: Box<Expr>,

    },
    DefMut {
        name: String,
        typ: ParsedType,
        value: Box<Expr>,
    },
    App {
        fname: String,
        arguments: Vec<Expr>,
    },
    UnaryOp {
        operator: UOp,
        e: Box<Expr>
    },
    BinOp {
        operator: BOp,
        e1: Box<Expr>,
        e2: Box<Expr>,
    },
    Update {
        target: String,
        value: Box<Expr>,
    },
    Branch {
        condition: Box<Expr>,
        b1: Vec<Expr>,
        b2: Vec<Expr>,
    },
    For {
        init: Box<Expr>,
        cond: Box<Expr>,
        post: Box<Expr>,
        //variable: Option<String>,
        body: Vec<Expr>
    },
    Access(String, AccessType),
    Comment(Vec<String>),
    // parameterized abstraction, where each param has an explicit type
    Abs {
        params: Vec<(String,ParsedType)>,
        body: Vec<Expr>
    }
}

// program is a vector of expressions
pub type Program = Vec<Expr>;
