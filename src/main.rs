mod syntax;
use syntax::Expr::{DefMut, Vect, I};
use syntax::Type::{Int, Array};


fn main() {
    let x = DefMut {
        name: "x".to_string(),
        value: Box::new(
            Vect {
                is_matrix: false,
                datatype: Array(Box::new(Int)),
                value: vec!{
                    Box::new(I(1)), Box::new(I(2))
                }
            }
        ),
    };
}
