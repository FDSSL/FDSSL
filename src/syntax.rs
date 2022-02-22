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

#[derive(Debug,PartialEq)]
pub struct Parameter {
    name: String,
    datatype: Type,
}

#[derive(Debug,PartialEq)]
pub enum AccessType {
    Name(String),
    Idx(usize),
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
        value: Box<Expr>,

    },
    DefMut {
        name: String,
        value: Box<Expr>,
    },
    App {
        fname: String,
        arguments: Vec<Expr>,
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
        condition: Box<Expr>,
        variable: Option<String>,
        body: Vec<Expr>,
    },
    Access(String, AccessType),
    Comment(Vec<String>),
    Func {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Vec<Parameter>,
        body: Vec<Expr>,
    },
}
