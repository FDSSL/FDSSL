/*
    Generator to produce GLSL code from FDSSL
 */

use std::fmt::format;

use crate::syntax::{self, Expr, ParsedType};

use itertools::Itertools;
use syntax::Program;

use glsl::parser::Parse as _;
use glsl::syntax::ShaderStage;

pub fn generate(prog: Program) -> String {
    let mut result = prog.into_iter().map(|expr| generate_expr(&expr));
    // crudely wrap it, as a general approach
    let glsl_program = "
precision highp float;
precision highp int;

void main() {\n".to_owned() + &result.join("\n") + "\n}\n";

    // TODO, may make more sense to produce a simple IR as part of generation
    // Would first walk the tree to find information about:
    // Types that we need to account for in advance (structs, functions, etc)
    // Special names that are present (color & position for example)
    // Collect loose exprs into scopes that are not functions (like global/main)

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
    let parsed_glsl_program = ShaderStage::parse(glsl_program.clone());
    assert!(
        parsed_glsl_program.is_ok(),
        "Failed to verify resulting GLSL program: {}",
        parsed_glsl_program.unwrap_err().info
    );
    return glsl_program;
}

fn generate_expr(expr: &Expr) -> String {
    match expr {
        Expr::I(i)  => i.to_string(),
        Expr::B(b)  => b.to_string(),
        Expr::F(f)  => f.to_string(),
        Expr::D(d)  => d.to_string(),
        Expr::Ref(r)    => r.clone(),
        Expr::Return(be)    => "return ".to_string() + &generate_expr(be).to_string(),
        Expr::Vect(v)   => "vec".to_owned() + &v.len().to_string() + "(" + &v.into_iter().map(|e| generate_expr(e)).join(",") + ")",
        Expr::Def { name, typ, value }  => vec![generate_type(typ), name.to_string(), "=".to_string(), generate_expr(value)].join(" ") + ";",
        Expr::DefMut { name, typ, value }  => vec![generate_type(typ), name.to_string(), "=".to_string(), generate_expr(value)].join(" ") + ";",
        Expr::UnaryOp { operator, e }   => format!("{}", operator) + &generate_expr(e),
        Expr::BinOp { operator, e1, e2 } => generate_expr(e1) + &format!("{}", operator) + &generate_expr(e2),
        Expr::Update { target, value } => target.to_owned() + &" = ".to_string() + &generate_expr(value) + ";",
        Expr::Comment(v) => v.into_iter().map(|c| "//".to_owned() + &c).join("\n"),
        e => "// TODO this expr not implemented yet...".to_string(),
    }
}

fn generate_type(typ: &ParsedType) -> String {
    match typ {
        ParsedType::BaseType(bt)    => bt.to_string(),
        ParsedType::Tuple(v)    => "vec".to_owned() + &v.len().to_string(), // TODO needs to be reworked for different 'vec' kinds
        ParsedType::NamedTuple(ne) => "TUPLE_TYPE".to_string(), // TODO needs to be reworked to create a struct
        ParsedType::Function(t1,t2) => "FUNCTION_TYPE".to_string() // TODO needs to be reworked to create a function? Have to think about this one
    }
}