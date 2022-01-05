pub enum DiscreteType {
    Int,
    Bool,
    Float,
}

pub enum Type {
    Vector(usize, DiscreteType),
    Function,
    Shader,
}

pub enum ShaderType {
    Vector,
    Fragment,
    Tesselation,
    // And more!
}

pub enum BOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
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

pub struct Block {
    pub body: Vec<Box<Expr>>
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
    Vect {
        is_matrix: bool,
        datatype: DiscreteType,
        value: Block,
    },
    Def {
        name: String,
        value: Box<Expr>,
    },
    DefMut {
        name: String,
        value: Box<Expr>,
    },
    Ref(String),
    App {
        fname: String,
        arguments: Block,
    },
    BinOp {
        operator: BOp,
        e1: Box<Expr>,
        e2: Box<Expr>,
    },
    Compose(String, String),
    Update{
        target: String,
        value: Box<Expr>,
    },
    Branch {
        condition: Box<Expr>,
        b1: Block,
        b2: Block,
    },
    For {
        condition: Box<Expr>,
        variable: Option<String>,
        body: Block,
    },
    Access(String, AccessType),
    Comment(Vec<String>),
    Func {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Block,
    },
    Shader {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Vec<Parameter>,
        body: Block,
    }
}
