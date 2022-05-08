/*

Alright, plan of action, I wrote out a bunch of 'typing'
rules for FDSSL on the train. I can find them and sketch them out here,
check if they make sense still.

The typechecker will take an FDSSL program, which needs to be added to the Syntax.

type Program = [Expr] ala Haskell, but write it out in Rust here.

...And it will produce a Typechecked program, which is different from a regular program.
Can only be emitted by the typechecker, and then our compiler will only accept a Typechecked pprogram.

Parser produces Program
TC takes Program, produces TC'd Program
Compiler takes TC'd Program, produces GLSL AST

So, to reason about it, I know in Haskell we could use a Typechecker monad.
The purpose of that is to manage the effects, env., etc. as we are typechecking.
Since we're using Rust, we don't need to do that necessarily, but it wouldn't be a bad idea
The idea is a 'TypeChecked a' container that is produced w/ the result of typechecking an Expr.
Like what types does it introduce, effects, etc., w/e.
A typechecked program is the result of a series of well-typed expressions
And that's what our compiler would take and then output as a GLSL AST

As another haskell example

tc :: a -> TCEnv -> Either TCError (type,TCEnv)

Typechecking a given thing 'a' (an Expr of some form)
in a typechecked environment will produce either a
typechecker error OR a type & a new typechecker env to use.

....
DEFS up front

Typing Environment
    Γ (Gamma) == our set of FDSSL typing assumptions

Typing Assumpion
    e : T == FDSSL term 'e' has type 'T'

Typing Relation
    Γ ⊢ e : T == term 'e' has type 'T' in Gamma (says 'e' is well-typed under the context of Gamma)
....

*/

use crate::syntax;

use syntax::Expr;
use syntax::BOp;
use syntax::UOp;
use syntax::Program;
use syntax::ParsedType;
use syntax::ParsedType::Tuple;
use syntax::ParsedType::NamedTuple;
use syntax::ParsedType::Function;
use syntax::ParsedType::BaseType;
use syntax::AccessType::Name;

use std::collections::{HashMap, HashSet};


// Type env, contains bindings of 'things' to types...these can be Exprs or Names
// w/ a HashMap our insertions & lookups pretty bad at O(n), but that's in the worst possible case, on average we should be seeing O(1)
type TCEnv = HashMap<String,ParsedType>;

// A positive type checked result
pub type TypeChecked = (ParsedType,TCEnv);

// A negative type checked result
// Simple type check error, for now, should have locatily at some point
type TCError = String;

// Result from typechecking, either a valid typechecked result or an error
pub type TCResult = Result<TypeChecked, TCError>;

//
//
// 1. Rewrote the 'TypeChecked' items above
// 2. Make sure that we define something for EACH kind of Expr
// 3. I.e. we should be able to handle specific expressions,
//      type check them, and return their Type + TypeEnv (type of pass)
// 4.
//

/// Marks a value of given type w/ a valid type
fn tc_pass(p: ParsedType, t: TCEnv) -> TCResult {
    Ok((p,t))
}

/// Fails the typechecker w/ an error message
fn tc_fail(e: String) -> TCResult {
    Err(e)
}

/// Looks up a parsed type in the TC env, potentially failing
fn tc_lookup(n: &str, env: &TCEnv) -> Result<ParsedType,TCError> {
    match env.get(n) {
        Some(v) => Ok((*v).clone()),
        None => Err(format!("Non-existant binding '{}' referenced, perhaps you meant to declare it first with 'let' or 'mut'?", n)),
    }
}

/// Attempts to typecheck a program
pub fn tc_program(p: Program) -> TCResult {
    let env : TCEnv = HashMap::new();
    tc_body(p, env)
}

/// Typechecks a vector of Exprs w/ a given environment
/// Verifies all of them before returning the result of the last Expr
/// Expects a body of 1 or more Exprs to verify
fn tc_body(body: Vec<Expr>, env: TCEnv) -> TCResult {
    if body.is_empty() {
        // must have something to work with
        tc_fail("Unable to typecheck an empty body!".to_string())

    } else {
        body.into_iter().fold(
            Ok((BaseType("".to_string()), env)),
            |acc, elt| {
                // Looks like `bind` huh?
                if let Ok((_, env)) = acc {
                    tc_expr(elt, env)
                }
                else {
                    acc
                }
            }
        )

    }
}


/// Returns the immediate arity of a given type w/ a depth of 0
/// All types return 1 except for tuples
fn type_arity(t: ParsedType) -> usize {
    match t {
        Tuple(v)    => v.len(), // get arity of this tuple, disregarding nesting of other tuples
        _           => 1 // all others are 1
    }
}

/// Helper to make a function type
fn mk_func_typ(t1: ParsedType, t2: ParsedType) -> ParsedType {
    Function(Box::new(t1), Box::new(t2))
}


/// Helper to make a base type from an &str
fn typ(s: &str) -> ParsedType {
    BaseType(s.to_string())
}

/// Helper to produce a homogenous binary function type
fn mk_bin_func(t: ParsedType) -> ParsedType {
    mk_func_typ(Tuple(vec![t.clone(),t.clone()]), t)
}

/// Returns the type that corresponds to a given BinOp
/// TODO, needs args types to determine overloaded arithmetic types
fn bop_type(bop: syntax::BOp, a1t: &ParsedType, a2t: &ParsedType) -> Result<ParsedType, TCError> {

    let args_match = *a1t == *a2t;
    let mut types = HashSet::new();
    types.insert("int".to_string());
    types.insert("float".to_string());
    types.insert("double".to_string());
    types.insert("bool".to_string());

    match (a1t, a2t) {
        (BaseType(b1), BaseType(b2)) => {
            if ! types.contains(b1) {
                Err(format!("Type {} is not supported by binary operations", b1))
            }
            else if ! types.contains(b2) {
                Err(format!("Type {} is not supported by binary operations", b2))
            }
            else {
                let (arg1, arg2) = ((*a1t).clone(), (*a2t).clone());
                match (bop, args_match, b1.as_str(), b2.as_str()) {
                    (syntax::BOp::Add, true, _, _) => Ok(mk_bin_func(arg1)),
                    (syntax::BOp::Sub, true, _, _) => Ok(mk_bin_func(arg1)),
                    (syntax::BOp::Mul, true, _, _) => Ok(mk_bin_func(arg1)),
                    (syntax::BOp::Div, true, _, _) => Ok(mk_bin_func(arg1)),
                    (syntax::BOp::Eq, true, _, _) => Ok(mk_func_typ(Tuple(vec![arg1, arg2]), typ("bool"))),
                    (syntax::BOp::Neq, true, _, _) => Ok(mk_func_typ(Tuple(vec![arg1, arg2]), typ("bool"))),
                    (syntax::BOp::Mod, _, "bool", "int") => Err("bool is not supported as an argument for the mod operator".to_string()),
                    (syntax::BOp::Mod, _, _, "int") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::And, true, _, "bool") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::Or, true, _, "bool")  => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::BitAnd, true, _, "int") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::BitOr, true, _, "int") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::BitXor, true, _, "int") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::Gt, true, _, "bool") => Err(format!("bool not supported with {} operator", bop)),
                    (syntax::BOp::Gte, true, _, "bool") => Err(format!("bool not supported with {} operator", bop)),
                    (syntax::BOp::Lte, true, _, "bool") => Err(format!("bool not supported with {} operator", bop)),
                    (syntax::BOp::Lt, true, _, "bool") => Err(format!("bool not supported with {} operator", bop)),
                    (syntax::BOp::Gt, true, _, _) => Ok(mk_func_typ(Tuple(vec![arg1,arg2]), typ("bool"))),
                    (syntax::BOp::Gte, true, _, _) => Ok(mk_func_typ(Tuple(vec![arg1,arg2]), typ("bool"))),
                    (syntax::BOp::Lte, true, _, _) => Ok(mk_func_typ(Tuple(vec![arg1,arg2]), typ("bool"))),
                    (syntax::BOp::Lt, true, _, _) => Ok(mk_func_typ(Tuple(vec![arg1,arg2]), typ("bool"))),
                    _ => Err(format!("Operator does not support argument types {} {}", *a1t, *a2t)),
                }
            }
        }
        (Function(args1, ret1), Function(args2, ret2)) => {
            if bop == BOp::Compose {
                if **ret2 == **args1 {
                    Ok(mk_func_typ((**args2).clone(), (**ret1).clone()))
                }
                else {
                    Err(format!("Return type {} is not the same as argument type {} in function composition", **ret2, **args1))
                }
            }
            else {
                Err("Only the composition operator can take function arguments".to_string())
            }
        },
        _ => Err("asd".to_string())
    }
}

/// Returns the type that corresponds to a given Unary op
/// TODO, needs args type to determine overloaded arithmetic types
fn uop_type(uop: UOp, argType: ParsedType) -> Result<ParsedType, TCError> {
    let atc = argType.clone();
    let atc2 = argType.clone();
    match argType {
        BaseType(n) => {
            match (uop, n.as_str()) {
                (Negative, "int")      => Ok(mk_func_typ(atc, atc2)),
                (Negative, "float")    => Ok(mk_func_typ(atc, atc2)),
                (Negative, "double")   => Ok(mk_func_typ(atc, atc2)),
        
                (Negate, "bool")   => Ok(mk_func_typ(atc, atc2)),

                _ => Err(format!("Invalid matching of unary op {uop} with an argument type {n}"))
            }
        },
        _ => Err("Cannot perform unary operation on anything other than a Base type!".to_string())
    }
}

/*

## TC(BaseType)
e : T ∈ Γ
---------
Γ ⊢ e : T

IF
    term 'e' has type 'T' in Gamma
THEN
    Gamma implies term 'e' has type 'T' (base case, entries are well-typed)


## TC(Constants) ... applies to int, double, float, bool, etc.
constant(c) ∈ T
---------------
Γ ⊢ c : T

IF
    'c' is a constant of 'T'
THEN
    'c' has type 'T' in Gamma
*/

// Typecheck general expressions
fn tc_expr(e: Expr, mut env: TCEnv) -> TCResult {
    let res = match e {

        // Int
        Expr::I(_) => {
            tc_pass(
                ParsedType::BaseType("int".to_string()),
                env
            )
        },

        // Float
        Expr::F(_) => {
            tc_pass(
                ParsedType::BaseType("float".to_string()),
                env
            )
        },

        // Double
        Expr::D(_) => {
            tc_pass(
                ParsedType::BaseType("double".to_string()),
                env
            )
        },

        // Boolean
        Expr::B(_) => {
            tc_pass(
                ParsedType::BaseType("bool".to_string()),
                env
            )
        },

        // Ref tc
        Expr::Ref(r) => {
            match env.get(&r) {
                Some(t) => tc_pass(t.clone(), env),
                None    => tc_fail(format!("Undefined reference '{r}'!"))
            }
        },

        /*
        ## TC(Def)
        n ∉ Γ <-- loosen this constraint, we allow shadowing
        Γ ⊢ e : T
        -----------------------
        Γ ⊢ `let n : T = e` : T

        IF
            term 'n' is not an element of Gamma (n is unbound)
            and Gamma entails term 'e' has type 'T'
        THEN
            Gamma entails 'let n : T = e' has type 'T'
        */
        Expr::Def{name: n, typ: t, value: e} => {
            let (t1, mut env1) = tc_expr(*e, env)?;

            if t != t1 {
                // fail if the types don't match
                return tc_fail(format!("Definition of '{n}' has type '{t}', but was assigned to a value of type '{t1}'"));

            } else {
                // valid, add this name & type combo to our env & continue
                env1.insert(n,t.clone());
                return tc_pass(
                    t,
                    env1
                );
            }
        },

        /*
        ## TC(DefMut)
        n ∉ Γ <-- loosen this constraint, we allow shadowing
        Γ ⊢ e : T
        --------------------------
        Γ ⊢ `mut name : T = e` : T

        IF
            term 'n' is not an element of Gamma (n is unbound)
            Gamma entails term 'e' has type 'T'
        THEN
            Gamma entails 'mut name : T = e' has type 'T'
            (however, updates to mut bindings are OK, just not w/ 'mut' again)
        */
        // At the moment this is very much the same as constant bindings
        // TODO, to distinguish this during updates,
        //     we may need a 'mut' or 'let' modifier for bindings ?, or we'll handle this elsewhere...I'll come back to this
        Expr::DefMut{name: n, typ: t, value: e} => {
            let (t1,mut env1) = tc_expr(*e, env)?;

            if t != t1 {
                // fail if the types don't match
                return tc_fail(format!("Definition of '{n}' has type '{t}', but was assigned to a value of type '{t1}'"));

            } else {
                // valid, add this name & type combo to our env & continue
                env1.insert(n,t.clone());
                return tc_pass(
                    t,
                    env1
                )
            }
        },

        /*
        ## TC(Update)
        Γ ⊢ n : T
        Γ ⊢ e : T
        ----------------
        Γ ⊢ `n = e` : T

        IF
            If Gamma implies term 'n' has type 'T'
            AND Gamma implies term 'e' has type 'T'
        THEN
            Gamma implies `n = e` has type 'T'
        */
        Expr::Update{target: n, value: v} => {
            let t1 = tc_lookup(&n, &env)?;
            let (t2,env) = tc_expr(*v, env)?;
            if t1 == t2 {
                tc_pass(t1,env)
            } else {
                tc_fail(format!("Definition of '{n}' has type '{t1}', but was assigned to a value of type '{t2}'"))
            }
        },

        /*
        ## TC(Parameters) ~~~ This one is coming from the types given to the abstraction by its context
        Γ ⊢ T0, ..., Tn
        ----------------------------
        Γ ⊢ (n0 : T0, ... , nn : Tn)

        IF
            Gamma implies types T0 through Tn are valids
        THEN
            A parameter listing of those same types is valid


        ## TC(Abs)
        Γ,p : T1 ⊢ e : T2
        --------------------------
        Γ ⊢ `(p) { e }` : T1 -> T2

        IF
            Gamma extended by parameter listing 'p' with type 'T1' implies term 'e' has type 'T2'
        THEN
            Gamma alone implies `p {e}` has type `T1 -> T2`

        */
        // Tricky, in order to typecheck abstractions they have to have a type declaration separate from their calling context?
        // Yeah need to think about that a bit more.
        Expr::Abs{params: p, body: b} => {

            let envOrig = env.clone();

            // get param types, and bind them in the env so we can check the body
            let paramTypes : Vec<ParsedType> = p.into_iter().map(|(n,t)| {
                // bind this pair in the env
                env.insert(n,t.clone());
                // return just the parsed type
                t
            }).collect();

            // get type of the body (return type), ignoring effect on env
            let (tb,_) = tc_body(b,env)?;

            // reset the env back
            // TODO (@montymxb) this copy / restore approach won't hold up well for large environments, may be best to just check which keys were present before, and keep them as they were
            env = envOrig;

            match paramTypes.len() {
                0 => {
                    // simple type w/ no params
                    return tc_pass(
                        tb,
                        env);

                },
                _ => {
                    // function type to construct
                    return tc_pass(
                        Function(Box::new(ParsedType::Tuple(paramTypes)), Box::new(tb)),
                        env);
                }
            }
        },

        /*
        ## TC(App)
        Γ ⊢ f : T1 -> T2
        Γ ⊢ e : T1
        -----------------
        Γ ⊢ f(e) : T2

        IF
            Gamma implies term 'f' has type 'T1 -> T2'
            AND Gamma implies term 'e' has type 'T1'
        THEN
            Gamma implies `f(e)` (f applied to e) has type 'T2'
        */
        // Function application
        Expr::App{fname: f, arguments: args} => {
            // get func type for app
            let t1 = tc_lookup(&f, &env)?;
            // typecheck each arg, and produce a list of arg types
            let mut argTypeList = vec![];
            for e in args {
                let (t,env2) = tc_expr(e,env)?;
                argTypeList.push(t);
                env = env2;
            }

            let arg_type : ParsedType;

            if argTypeList.len() == 1 {
                // just a base type
                //argType = argTypeList.pop();
                match argTypeList.pop() {
                    Some(at)    => arg_type = at,
                    None        => return Err("Failed to produce valid arg list".to_string())
                }

            } else {
                // tuple type
                arg_type = ParsedType::Tuple(argTypeList);

            }

            let t1c = t1.clone();
            match t1 {
                Function(ft1,ft2)   => {
                    if *ft1 == arg_type {
                        // types of args match param types
                        // produce resultant type
                        tc_pass(
                            *ft2,
                            env
                        )

                    } else {
                        // mismatch
                        tc_fail(format!("Function '{f}' of type '{t1c}' applied to args of type '{arg_type}'. Consider changing the args to match the function type, or change the type of the function to fix this."))
                    }

                },
                _  => {
                    // auto fail, no parameters for the type of this function!
                    tc_fail(format!("Function '{f}' of type '{t1}' is applied to arguments, but has no parameters in its type. Remove the arguments to fix this problem, or change the type of the function to accept arguments."))

                }
            }
        }

        /*
        ## TC(BinOp), homogenous
        Γ ⊢ f : (T,T) -> T
        Γ ⊢ e1 = e2 : T
        -----------------------
        Γ ⊢ e1 f e2 : T

        IF
            Gamma implies term 'f' has type '(T,T) -> T'
            AND Gamma implies terms 'e1' and 'e2' share the same type 'T'
        THEN
            Gamma implies `e1 f e2` has type 'T'
            (NOTE: This is the restrictive case, it does NOT allow for binary ops w/ heterogenous types)
        */
        // BinOp
        Expr::BinOp{operator: b, e1: e11, e2: e22} => {
            // can use '?' here to help unwrap conditional value, using maybe or Either?
            let (t1,e1) = tc_expr(*e11, env)?;
            let (t2,e2) = tc_expr(*e22, e1)?;
            // get param type & result type for this BinOp
            let btyp = bop_type(b, &t1, &t2)?;

            match btyp {
                Function(tp, tr)    => {
                    // compare w/ args
                    if Tuple(vec![t1,t2]) == *tp {
                        tc_pass(
                            *tr,
                            e2
                        )
                    } else {
                        // TODO not a very descriptive error message, needs to be improved
                        tc_fail("Bad binop!".to_string())
                    }
                },
                _ => tc_fail("BinOps may only be function types, '{b}' was not!".to_string())
            }
        },

        /*
        ## TC(Unary)
        Γ ⊢ u : T1 -> T2
        Γ ⊢ e : T1
        -----------------------
        Γ ⊢ u e : T2

        WLOG, same as TC(App) above, but w/ no parens
        */
        Expr::UnaryOp{operator: b, e: e} => {
            // TODO setup to get type of uop and match w/ expr type
            let (t2,env1) = tc_expr(*e, env)?;
            let t2c = t2.clone();
            let uopTyp = uop_type(b, t2)?;


            match uopTyp {
                Function(f1,f2) => {
                    if *f1 == t2c {
                       // match
                        return tc_pass(
                            *f2,
                            env1
                        );

                    } else {
                        // mismatch
                        tc_fail(format!("Unary operator '{b}' was expecting an argument of type '{f1}' but got an argument of type '{t2c}' instead"))

                    }
                },
                _ => tc_fail(format!("Unary operator '{b}' was not a function type as expected, was instead '{uopTyp}'"))
            }
        },


        /*
        ## TC(Branch)
        Γ,c : Bool ⊢ e1 = e2 : T
        -------------------------------
        Γ ⊢ `if(c) {e1} else {e2}` : T

        IF
            Gamma extended by term 'c' with type 'Bool' implies terms 'e1' and 'e2' share the same type 'T'
        THEN
            Gamma implies `if(c) {e1} else {e2}` has type 'T'
        */
        // BRANCH
        Expr::Branch{condition: c, b1: b1, b2: b2} => {
            let (t1,env1) = tc_expr(*c, env)?;
            if t1 != BaseType("bool".to_string()) {
                return tc_fail(format!("'If' was expecting an expression of type 'bool', but got an expression of type '{t1}' instead"));

            } else {
                // verify the types of b1 & b2 match
                let (tb1,env2) = tc_body(b1,env1)?;
                let (tb2,env3) = tc_body(b2,env2)?;

                if tb1 == tb2 {
                    // body types match
                    return tc_pass(
                        tb1,
                        env3
                    );

                } else {
                    // body types do NOT match, fail
                    return tc_fail(format!("'If' branches were expected to have the same type, but have differing types of '{tb1}' and '{tb2}' instead"));

                }
            }
        },


        /*
        ## TC(AccessInx)
        Γ ⊢ e : IVec(T1, ..., Tn)
        Γ ⊢ i : Int
        i ∈ e
        -------------------------  Ti ∈ IVec(T1, ..., Tn)
        Γ ⊢ e[i] : Ti

        IF
            Gamma implies term 'e' has type 'IVec(T1, ..., Tn)'
            AND Gamma implies term 'i' has type 'Int'
            AND 'i' is an element of term 'e'
            AND side condition Ti is an element of IVec(T1, ..., Tn) holds
        THEN
            Gamma implies `e[i]` has type 'Ti'


        ## TC(AccessNam)
        Γ ⊢ e : NVec(T1, ..., Tn)
        i isProp e
        ----------------------------------- Ti ∈ NVec(T1, ..., Tn)
        Γ ⊢ e.i : Ti

        IF
            Gamma implies term 'e' has type 'NVec(T1, ..., Tn)'
            AND 'i' is an property of term 'e'
            AND side condition Ti is an element of NVec(T1, ..., Tn) holds
        THEN
            Gamma implies property i of e, `e.i`, has type 'Ti'
        */
        // ACCESS by INDEX or NAME
        Expr::Access(n, at) => {
            match at {
                // access by name
                Name(prop) => {
                    // return the property type for this item's name
                    let t = tc_lookup(&n, &env)?;
                    match t {
                        NamedTuple(namedTypes)  => {
                            for (name,typ) in namedTypes {
                                if *name == prop {
                                    // immediately produce this type w/out any other work
                                    return tc_pass(*typ, env);
                                }
                            }
                            tc_fail(format!("Property '{}' on tuple '{}' is not defined", prop, n))
                        },
                        // any other as a fail case
                        _ => tc_fail(format!("Incorrect access of tuple '{}'!", n))
                    }
                },
                _ => tc_fail(format!("Invalid tuple index case for '{n}', not fully implemented yet!"))
            }
        },

        // Vector of expressions
        Expr::Vect(v) => {
            let mut tup_type = vec![];

            for e in v {
                let (et,env1) = tc_expr(e, env)?;
                tup_type.push(et);
                env = env1;
            }

            // box it up as the final type
            tc_pass(ParsedType::Tuple(tup_type), env)
        },


        // fill in the rest here, and just call out the relevant handler
        _ => tc_fail(format!("Unrecognized expression '{:?}'", e))
    };
    return res;
}

/*

#
# Typing Rules
#


??? TC(For)
.... current implementation is not capable of being type checked, does not guarantee a value ....
.... should we consider a 'do while', given we can just guarantee a value then?
 */
