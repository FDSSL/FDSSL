mod syntax;
mod parser;
mod typechecker;

use parser::program;
use typechecker::tc_program;

fn main() {
    let prog = "\
    let swap : (int, int) -> (int, int) = (x : int, y : int) { \n\
        (y,x)\n\
    }\n";
    let res = match program(prog) {
        Ok((_, a)) => a,
        _          => vec![]
    };
    println!("\n\nPARSED PROGRAM: {:?}\n\n", res);
    println!("TYPECHECKED PROGRAM: {:?}\n\n", tc_program(res));

    let r1 = program("(x:1, y:2)");
    let r2 = program("1+1\n");
    println!("{:?}", r2);

//     let x = DefMut {
//         name: "x".to_string(),
//         value: Box::new(
//             Vect {
//                 is_matrix: false,
//                 datatype: Array(Box::new(Int)),
//                 value: vec!{
//                     Box::new(I(1)), Box::new(I(2))
//                 }
//             }
//         ),
//     };
}
