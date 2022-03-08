mod syntax;
mod parser;

use parser::program;

fn main() {
    println!("Trying here?");
    // let prog = "\
    // laksjd (a: tru, b: a) -> (a, b) { \
    // kjsd {{{{}{pd{kdfj}kjd} }}}} {} {}";
    // let res = match program(prog) {
    //     Ok((_, a)) => a,
    //     _          => vec![]
    // };
    // println!("{:?}", res);

    //let r2 = program("(x:1, y:2)");
    let r2 = program("1\n1\n2\n");

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
