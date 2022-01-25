pub enum Type {
    Uint,
    Int,
    Bool,
    Float,
    Double,
    Array(Box<Type>),
    Struct(String),
    Function,
    Opaque(String),
}

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

pub struct Parameter {
    name: String,
    datatype: Type
}

pub enum AccessType {
    Name(String),
    Idx(usize),
}

pub enum Expr {
    I(usize),
    B(bool),
    F(f32),
    D(f64),
    Ref(String),
    Return(Box<Expr>),
    Vect {
        is_matrix: bool,
        datatype: Type,
        value: Vec<Box<Expr>>,
    },
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
        arguments: Vec<Box<Expr>>,
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
        b1: Vec<Box<Expr>>,
        b2: Vec<Box<Expr>>,
    },
    For {
        condition: Box<Expr>,
        variable: Option<String>,
        body: Vec<Box<Expr>>,
    },
    Access(String, AccessType),
    Comment(Vec<String>),
    Func {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Vec<Parameter>,
        body: Vec<Box<Expr>>,
    },
}
