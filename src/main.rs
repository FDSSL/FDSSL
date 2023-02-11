mod syntax;
mod parser;
mod typechecker;

use parser::program;
use typechecker::tc_program;


/// Verify a program through parsing & Typechecking
fn verify(prog: &str) {
    println!("Verifying program: \n\n{}", prog);
    match program(prog) {
        Ok((_, parsed_prog)) => {
            println!("* program parsed successfully");
            match tc_program(parsed_prog) {
                Ok((_, s)) => {
                    println!("* program typechecked successfully");
                },
                Err(e)  => println!("!! Failed to TC program: {:?}", e)
            }
        },
        Err(e) => println!("!! Failed to parse program: {:?}", e)
    }
}

fn main() {
    let prog = "\
    let swap : (int, int) -> (int, int) = (x : int, y : int) { \n\
    \t(y,x)\n\
    }\n\
    \n\
    let apply : (int -> int, int) -> int = (f: int -> int, v: int) {\n\
    \tf(v)\n\
    }\n\
    \n\
    let double: int -> int = (x: int) {\n\
    \tx*2\n\
    }\n\
    \n\
    apply(double,8)\n\
    ";
    verify(prog);
    
    let res = match program(prog) {
        Ok((_, a)) => a,
        _          => vec![]
    };
    println!("\n\nPARSED PROGRAM: {:?}\n\n", res);
    println!("TYPECHECKED PROGRAM: {:?}\n\n", tc_program(res));

    let r1 = program("(x:1, y:2)");
    let r2 = program("1+1\n");
    println!("{:?}", r2);

    // let x = DefMut {
    //     name: "x".to_string(),
    //     value: Box::new(
    //         Vect {
    //             is_matrix: false,
    //             datatype: Array(Box::new(Int)),
    //             value: vec!{
    //                 Box::new(I(1)), Box::new(I(2))
    //             }
    //         }
    //     ),
    // };
}
