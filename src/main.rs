mod syntax;
mod parser;

use parser::program;

fn main() {
    let prog = "\
    let swap : (a, b) -> (b, a) = (x,y) { \
        (y,x)\
    }\n";
    let res = match program(prog) {
        Ok((_, a)) => a,
        _          => vec![]
    };
    println!("{:?}", res);

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
