mod syntax;
use syntax::Expr::*;
use syntax::DiscreteType::Int;
use syntax::Block;

fn main() {
    let x = Def {
        mutable: false,
        name: "x",
        value: Box::new(
            Vect {
                is_matrix: false,
                datatype: Int,
                value: Block{
                    body: vec!{
                        Box::new(I(1)), Box::new(I(2))
                    }
                }
            }
        );
}
