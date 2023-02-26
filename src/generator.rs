/*
    Generator to produce GLSL code from FDSSL
 */

use crate::syntax::{self, Expr, ParsedType};

use itertools::Itertools;
use syntax::Program;

use glsl::parser::Parse as _;
use glsl::syntax::ShaderStage;

/**
 * Structure that captures a generated GLSL program's output.
 * Split into several sections, allowing us to generate code once from FDSSL,
 * while still placing generated output into the appropriate locations for GLSL output
 */
#[derive(Debug)]
struct GeneratedProgram {
    // section for relevant information before checking other details
    headerSection: Vec<String>,
    // section for defining types 
    typeSection: String,
    // section for defining functions
    funcSection: String,
    // sections for main code
    mainSection: String
}

pub fn generate(prog: Program) -> String {

    // start with our base generated program
    let mut generated_program: GeneratedProgram = GeneratedProgram {
        headerSection: vec![
            "precision highp float;".to_string(),
            "precision highp int;".to_string()
        ],
        typeSection: "".to_string(),
        funcSection: "".to_string(),
        mainSection: "".to_string()
    };

    // for every expr, pass in our generated program, and retrieve the final result to work with at the end
    let mut result = prog.into_iter().map(|expr| generate_expr(&mut generated_program, &expr));
    // crudely wrap it, as a general approach
    // let glsl_program: String = generated_program.headerSection.join("\n");
    let glsl_program = "
void main() {\n".to_owned() + &result.join("\n") + "\n}\n";
    println!("{:#?}", generated_program);

    // TODO, may make more sense to produce a simple IR as part of generation
    // Would first walk the tree to find information about:
    // Types that we need to account for in advance (structs, functions, etc)
    // Special names that are present (color & position for example)
    // Collect loose exprs into scopes that are not functions (like global/main)

    

    // TODO collect loose non-function exprs into a collective group, which will go under 'main'
    // TODO for lambdas (anonymous functions), those should all be collected into an env, which I can then use to reference to either inline them, or associate a call to them

    // first, write up a basic shader that we think an FDSSL program should compile to
    // compile that with this library, and then get the AST, that's what we can use to map into
    // let vertShader = "precision highp float;
    // precision highp int;
    
    // attribute vec3 position;
    // uniform mat4 modelViewMatrix;
    // uniform mat4 projectionMatrix;
    // varying vec2 vXY;
    // uniform float time;
    
    // void main() {
    //   vXY = vec2(position.x, position.y);
    //   vXY.x = vXY.x * time * 0.1;
    //   gl_Position = projectionMatrix * modelViewMatrix  * vec4(position, 1.0);
    // }";
    // let fragShader = "precision highp float;
    // precision highp int;
    
    // uniform float time;
    
    // varying vec2 vXY;
    
    // void main() {
    //   float x = vXY.x + time * 3.3;
    //   float y = vXY.y + time * 3.7;
    
    //   float r = cos(x);
    //   float g = sin(y);
    //   float b = r + g;
    
    //   gl_FragColor = vec4(r,g,b,1.0);
    // }";

    // just assume we're only producing a generic shader for now

    // VERIFY this shader
    println!("{}", glsl_program);
    let parsed_glsl_program = ShaderStage::parse(glsl_program.clone());
    assert!(
        parsed_glsl_program.is_ok(),
        "Failed to verify resulting GLSL program: {}",
        parsed_glsl_program.unwrap_err().info
    );
    return glsl_program;
}

fn generate_expr(gp: &mut GeneratedProgram, expr: &Expr) -> String {
    match expr {
        Expr::I(i)  => i.to_string(),
        Expr::B(b)  => b.to_string(),
        Expr::F(f)  => f.to_string(),
        Expr::D(d)  => d.to_string(),
        Expr::Ref(r)    => r.clone(),
        Expr::Return(be)    => "return ".to_string() + &generate_expr(gp, be).to_string(),
        Expr::Vect(v)   => "vec".to_owned() + &v.len().to_string() + "(" + &v.into_iter().map(|e| generate_expr(gp, e)).join(",") + ")",
        Expr::Def { name, typ, value }  => {
            // TODO replace 'void' below with the proper 'output' type from 'typ' (which is either a function type, or something that only returns a regular type)
            return match &**value {
                Expr::Abs { params: _, body: _ } => {
                    let return_type = generate_function_return_type(gp, typ);
                    let out = vec![return_type.to_string(), name.to_string(), generate_expr(gp, value)].join(" ") + ";";
                    // lift this whole expression up into the global scope, and not locally here
                    gp.funcSection.push_str(out.clone().as_str());
                    // don't return anything directly here
                    return "".to_string();
                },
                // fallback to passing the expression along as expected
                _   => vec![generate_type(gp, typ), name.to_string(), "=".to_string(), generate_expr(gp, value)].join(" ") + ";"
            };
        },
        Expr::DefMut { name, typ, value }  => {
            let out = vec![generate_type(gp, typ), name.to_string(), "=".to_string(), generate_expr(gp, value)].join(" ") + ";";
            return match &**value {
                Expr::Abs { params: _, body: _ } => {
                    // lift this whole expression up into the global scope, and not locally here
                    gp.funcSection.push_str(out.clone().as_str());
                    // don't return anything directly here
                    return "".to_string();
                },
                // fallback to passing the expression along as expected
                _   => out
            };
        },
        Expr::UnaryOp { operator, e }   => format!("{}", operator) + &generate_expr(gp, e),
        Expr::BinOp { operator, e1, e2 } => generate_expr(gp, e1) + &format!("{}", operator) + &generate_expr(gp, e2),
        Expr::Update { target, value } => target.to_owned() + &" = ".to_string() + &generate_expr(gp, value) + ";",
        Expr::Comment(v) => v.into_iter().map(|c| "//".to_owned() + &c).join("\n"),
        Expr::Abs { params, body } => {
            // treat as a function
            return "(".to_owned() + &params.into_iter().map(|(n,t)| generate_type(gp,t) + " " + n).join(",") + ") {\n" + &body.into_iter().map(|e| generate_expr(gp,e)).join(";") + "\n}\n";
        },
        Expr::App { fname, arguments } => {
            return fname.to_owned() + "(" + &arguments.into_iter().map(|arg| generate_expr(gp, arg)).join(",") + ");";
        }
        e => todo!("Encountered unhandled Expr during Generation: {:?}", e),
    }
}

fn generate_function_return_type(gp: &mut GeneratedProgram, typ: &ParsedType) -> String {
    return generate_type(gp, typ);
}

fn generate_type(gp: &mut GeneratedProgram, typ: &ParsedType) -> String {
    match typ {
        ParsedType::BaseType(bt)    => bt.to_string(),
        ParsedType::Tuple(v)    =>  {
            // check if all types are the same
            let isHomogenous = v.into_iter().any(|t| *t != v[0]);
            if isHomogenous {
                // express using a vec of the fixed length

                return match v[0] {
                    ParsedType::BaseType("int") => "ivec".to_owned() + &v.len().to_string(),
                    ParsedType::BaseType("float") => "vec".to_owned() + &v.len().to_string(),
                    ParsedType::BaseType("double") => "vec".to_owned() + &v.len().to_string()
                }
            } else {
                // express as a struct type and add the name as a lookup in the future into our GeneratedProgram state
                // TODO need to add a feature that captures a named lookup (i.e., future references to this type get the name, not the type itself here)
            }
        },
        ParsedType::Function(t1,t2) => {
            // do nothing, an abstraction will fill in the type as needed
            return "\n".to_string();
        },
        _ => todo!("Unable to generate type: {:?}", typ)
        // ParsedType::Tuple(v)    =>  //"vec".to_owned() + &v.len().to_string(), // TODO needs to be reworked for different 'vec' kinds
        // ParsedType::NamedTuple(ne) => "TUPLE_TYPE".to_string(), // TODO needs to be reworked to create a struct
        // ParsedType::Function(t1,t2) => "FUNCTION_TYPE".to_string() // TODO needs to be reworked to create a function? Have to think about this one
    }
}