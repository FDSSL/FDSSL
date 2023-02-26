mod syntax;
mod parser;
mod typechecker;
mod generator;

use parser::program;
use typechecker::tc_program;
use generator::generate;

/// Verify a program through parsing & Typechecking
fn verify(prog: &str) {
    println!("Verifying program: \n\n{}", prog);
    match program(prog) {
        Ok((_, parsed_prog)) => {
            println!("* program parsed successfully");
            match tc_program(&parsed_prog) {
                Ok((_, _s)) => {
                    println!("* program typechecked successfully");
                },
                Err(e)  => println!("!! Failed to TC program: {:?}", e)
            }
        },
        Err(e) => println!("!! Failed to parse program: {:?}", e)
    }
}

fn main() {

    // Easy to test shaders with something like
    // https://shdr.bkcore.com/
    // or some other site, there's plenty of these lying around

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
    println!("TYPECHECKED PROGRAM: {:?}\n\n", tc_program(&res));

    let _r1 = program("(x:1, y:2)");
    let r2 = program("1+1\n");
    println!("{:?}", r2);

    // program that only sets color
    let prog2 = "// some comment\nlet color: (float,float,float,float) = (1.0f, 0.5f, 0.5f, 1.0f)\n";
    let gen = generate_glsl_from_fdssl(prog2.to_string());
    println!("\nGenerated GLSL:\n{}",gen);

    let gen2 = generate_glsl_from_fdssl(prog.to_string());
    println!("\nGenerated GLSL 2:\n{}",gen2);

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

// Helper to parse, typecheck, and generate code from FDSSL
fn generate_glsl_from_fdssl(prog: String) -> String {
    let parse_result = program(&prog);
    assert!(parse_result.is_ok(), "Failed to parse with error: {}", parse_result.unwrap_err());
    let (_,ast) = parse_result.unwrap();

    let tc_result = tc_program(&ast);
    assert!(tc_result.is_ok(), "Failed to typecheck with error: {}", tc_result.unwrap_err());

    return generate(ast);
}

/// General Tests...

#[test]
fn test_simple_frag_generates() {
    let prog = "// test comment\nlet color: (float,float,float,float) = (1.0f,0.5f,0.5f,1.0f)\n".to_string();
    let generated_result = generate_glsl_from_fdssl(prog);
    println!("Simple Frag program:\n{}", generated_result);
}