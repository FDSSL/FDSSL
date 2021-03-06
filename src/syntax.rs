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
#[derive(Debug, PartialEq)]
pub enum ParsedType {
    BaseType(String),
    Tuple(Vec<Box<ParsedType>>),
    Function(Box<ParsedType>, Box<ParsedType>),
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

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum UOp {
    Negative,
    Negate
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
    // parameterized abstraction
    Abs {
        params: Vec<String>,
        body: Vec<Expr>
    }
}
