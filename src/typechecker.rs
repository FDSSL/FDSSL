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

use std::collections::HashMap;

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
    return Ok((p,t));
}

/// Fails the typechecker w/ an error message
fn tc_fail(e: &str) -> TCResult {
    return Err(e.to_string());
}

/// Looks up a parsed type in the TC env, potentially failing
fn tc_lookup(n: &str, env: TCEnv) -> Result<ParsedType,TCError> {
    match env.get(n) {
        Some(p) => Ok(*p),
        None    => Err(format!("Non-existant binding '{n}' referenced, perhaps you meant to declare it first with 'let' or 'mut'?"))
    }
}

/// Attempts to typecheck a program
pub fn tc_program(i: Program) -> TCResult {
    return tc_fail("NEEDS TO BE IMPLEMENTED STILL!");
}

/// Typechecks a vector of Exprs w/ a given environment
/// Verifies all of them before returning the result of the last Expr
/// Expects a body of 1 or more Exprs to verify
fn tc_body(body: Vec<Expr>, env: TCEnv) -> TCResult {
    if body.len() <= 0 {
        // must have something to work with
        tc_fail("Unable to typecheck an empty body!")

    } else {
        // typecheck each expr to make sure no errors occur normally
        let result : TCResult = Err("".to_string());
        for expr in body {
            result = tc_expr(expr, env);
            match result {
                // update env on valid check
                Ok((_,t))   => env = t,
                // propagate any error up
                Err(s)      => return Err(s)
            };
        }
        // only return the result of the last check
        result

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

/// Helper to make a tuple type
fn mk_tup_typ(typs: Vec<ParsedType>) -> ParsedType {
    Tuple(typs.into_iter().map(|t| Box::new(t)).collect())
}

/// Helper to make a base type from an &str
fn typ(s: &str) -> ParsedType {
    BaseType(s.to_string())
}

/// Helper to produce a homogenous binary function type
fn mk_bin_func(t: ParsedType) -> ParsedType {
    mk_func_typ(mk_tup_typ(vec![t,t]), t)
}

/// Returns the type that corresponds to a given BinOp
/// TODO, needs args types to determine overloaded arithmetic types
fn bop_type(bop: BOp, a1t: ParsedType, a2t: ParsedType) ->  Result<ParsedType,TCError> {

    // extract type names of base types, nothing else should go here
    let (n1,n2) = match (a1t,a2t) {
        (BaseType(b1),BaseType(b2)) => (b1.as_str(), b2.as_str())
    };

    match (bop, n1, n2, a1t == a2t) {
        (Add, "int", "int", _)       => Ok(mk_bin_func(a1t)),
        (Add, "float", "float", _)   => Ok(mk_bin_func(a1t)),
        (Add, "double", "double", _) => Ok(mk_bin_func(a1t)),

        (Sub, "int", "int", _)       => Ok(mk_bin_func(a1t)),
        (Sub, "float", "float", _)   => Ok(mk_bin_func(a1t)),
        (Sub, "double", "double", _) => Ok(mk_bin_func(a1t)),

        (Mul, "int", "int", _)       => Ok(mk_bin_func(a1t)),
        (Mul, "float", "float", _)   => Ok(mk_bin_func(a1t)),
        (Mul, "double", "double", _) => Ok(mk_bin_func(a1t)),

        (Div, "int", "int", _)       => Ok(mk_bin_func(a1t)),
        (Div, "float", "float", _)   => Ok(mk_bin_func(a1t)),
        (Div, "double", "double", _) => Ok(mk_bin_func(a1t)),

        // modolu only w/ ints
        (Mod, "int", "int", _)    => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), a1t)),
        (Mod, "float", "int", _)  => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), a1t)),
        (Mod, "double", "int", _) => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), a1t)),

        (And, "bool", "bool", _) => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), a1t)),
        (Or, "bool", "bool", _)  => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), a1t)),

        // TODO Compose is not quite done yet
        (Compose, _, _, _)  => {
            match(a1t, a2t) {
                (Function(a1,a2), Function(b1,b2))  => {
                    if *a2 == *b1 {
                        Ok(mk_func_typ(*a1, *b2))
                    } else {
                        // better error here
                        Err("Can only compose function types!".to_string())
                    }
                }
            }
        },

        // eq for any 2 types
        (Eq, _, _, true)   => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),
        (Neq, _, _, true)  => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),

        (Gt, "int", "int", _)         => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),
        (Gt, "float", "float", _)     => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),
        (Gt, "double", "double", _)   => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),

        (Gte, "int", "int", _)         => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),
        (Gte, "float", "float", _)     => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),
        (Gte, "double", "double", _)   => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),

        (Lt, "int", "int", _)         => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),
        (Lt, "float", "float", _)     => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),
        (Lt, "double", "double", _)   => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),

        (Lte, "int", "int", _)         => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),
        (Lte, "float", "float", _)     => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),
        (Lte, "double", "double", _)   => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), typ("bool"))),

        (BitAnd, "int", "int", _)   => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), a1t)),
        (BitOr, "int", "int", _)    => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), a1t)),
        (BitXor, "int", "int", _)   => Ok(mk_func_typ(mk_tup_typ(vec![a1t,a2t]), a1t))
    }
}

/// Returns the type that corresponds to a given Unary op
/// TODO, needs args type to determine overloaded arithmetic types
fn uop_type(uop: UOp, argType: ParsedType) -> ParsedType {

    let argName = match argType {
        BaseType(n) => n.as_str()
    };

    match (uop, argName) {
        (Negative, "int")      => mk_func_typ(argType, argType),
        (Negative, "float")    => mk_func_typ(argType, argType),
        (Negative, "double")   => mk_func_typ(argType, argType),

        (Negate, "bool")   => mk_func_typ(argType, argType)
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
fn tc_expr(e: Expr, env: TCEnv) -> TCResult {
    let res = match e {

        // Int
        Expr::I(i) => {
            tc_pass(
                ParsedType::BaseType("int".to_string()),
                env
            )
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
            let (t1,env) = tc_expr(*e, env)?;

            if t != t1 {
                // fail if the types don't match
                return tc_fail(format!("Definition of '{n}' has type '{t}', but was assigned to a value of type '{t1}'").as_str());

            } else {
                // valid, add this name & type combo to our env & continue
                env.insert(n,t);
                return tc_pass(
                    t,
                    env
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
            let (t1,env) = tc_expr(*e, env)?;

            if t != t1 {
                // fail if the types don't match
                return tc_fail(format!("Definition of '{n}' has type '{t}', but was assigned to a value of type '{t1}'").as_str());

            } else {
                // valid, add this name & type combo to our env & continue
                env.insert(n,t);
                return tc_pass(
                    t,
                    env
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
            let t1 = tc_lookup(&n, env)?;
            let (t2,env) = tc_expr(*v, env)?;
            if t1 == t2 {
                return tc_pass(t1,env);
            } else {
                return tc_fail(format!("Definition of '{n}' has type '{t1}', but was assigned to a value of type '{t2}'").as_str());
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
        Expr::Abs{params: p, typ: t, body: b} => {
            let (tb,env1) = tc_body(b,env)?;
            match t {
                Function(t1,t2) => {
                    if type_arity(*t1) != p.len() {
                        // bad dimensionality, i.e. number of params does not match those that can be resolved to a type
                        return tc_fail(format!("Type '{t1}' does not match the number of parameters given to this abstraction.").as_str());

                    } else if *t2 == tb {
                        // result type in function matches type of body, ok!
                        return tc_pass(
                            t,
                            env1
                        );

                    } else {
                        // type mismatch
                        return tc_fail(format!("Abstraction has type '{t}', but has a body of type '{tb}'. Consider changing what type the body produces, or the type of the abstraction to fix this.").as_str());

                    }
                },
                anyOtherType    => {
                    // simple case, not a function type (no params)
                    if p.len() != 0 {
                        // can't pass args to a function that doesn't have parameters!
                        return tc_fail(format!("Abstraction has parameters, but it's type, '{t}' does not take parameters. Consider changing the type or removing the parameters to fix this.").as_str());

                    } else if t == anyOtherType {
                        // match
                        return tc_pass(
                            anyOtherType,
                            env1
                        )
                    } else {
                        // mismatch
                        return tc_fail(format!("Abstraction has type '{t}', but has a body of type '{anyOtherType}'. Consider changing what type the body produces, or the type of the abstraction to fix this.").as_str());

                    }
                }
            };
            // This needs more work, the number of params needs to match
            // the dimensionality of the type provided
            // when typ is NOT a function type, params should be empty, and match w/ type of body
            // when typ IS a function type, params should not be empty, and they should match the dimensionality
            // of the type provided, and the 2nd part of typ should match the typ of body
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
            let t1 = tc_lookup(&f, env)?;
            // typecheck each arg, and produce a list of arg types
            let argTypeList = vec![];
            for e in args {
                let (t,env2) = tc_expr(e,env)?;
                argTypeList.push(t);
                env = env2;
            }

            let argType : ParsedType = ParsedType::BaseType("".to_string());

            if argTypeList.len() == 1 {
                // just a base type
                argType = argTypeList[0];

            } else {
                // tuple type
                argType = ParsedType::Tuple(argTypeList.into_iter().map(|t| Box::new(t)).collect());

            }

            match t1 {
                Function(ft1,ft2)   => {
                    if *ft1 == argType {
                        // types of args match param types
                        // produce resultant type
                        return tc_pass(
                            *ft2,
                            env
                        );

                    } else {
                        // mismatch
                        return tc_fail(format!("Function '{f}' of type '{t1}' applied to args of type '{argType}'. Consider changing the args to match the function type, or change the type of the function to fix this.").as_str());
                    }

                },
                anyOtherType        => {
                    // auto fail, no parameters for the type of this function!
                    return tc_fail(format!("Function '{f}' of type '{t1}' is applied to arguments, but has no parameters in its type. Remove the arguments to fix this problem, or change the type of the function to accept arguments.").as_str());

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
            let btyp = bop_type(b, t1, t2)?;

            match btyp {
                Function(tp, tr)    => {
                    // compare w/ args
                    if Tuple(vec![Box::new(t1),Box::new(t2)]) == *tp {
                        tc_pass(
                            *tr,
                            e2
                        )
                    } else {
                        // TODO not a very descriptive error message, needs to be improved
                        tc_fail("Bad binop!")
                    }
                }
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
            let uopTyp = uop_type(b, t2);

            match uopTyp {
                Function(f1,f2) => {
                    if *f1 == t2 {
                       // match
                        return tc_pass(
                            *f2,
                            env1
                        );

                    } else {
                        // mismatch
                        return tc_fail(format!("Unary operator '{b}' was expecting an argument of type '{f1}' but got an argument of type '{t2}' instead").as_str());

                    }
                }
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
                return tc_fail(format!("'If' was expecting an expression of type 'bool', but got an expression of type '{t1}' instead").as_str());

            } else {
                // verify the types of b1 & b2 match
                let (tb1,env2) = tc_body(b1,env1)?;
                let (tb2,env3) = tc_body(b1,env2)?;

                if tb1 == tb2 {
                    // body types match
                    return tc_pass(
                        tb1,
                        env3
                    );

                } else {
                    // body types do NOT match, fail
                    return tc_fail(format!("'If' branches were expected to have the same type, but have differing types of '{tb1}' and '{tb2}' instead").as_str());

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
                    let t = tc_lookup(&n, env)?;
                    match t {
                        NamedTuple(namedTypes)  => {
                            for (name,typ) in namedTypes {
                                if name == prop {
                                    // immediately produce this type w/out any other work
                                    return tc_pass(*typ, env);
                                }
                            }
                            tc_fail(format!("Property '{}' on tuple '{}' is not defined", prop, n).as_str())
                        }
                    }
                }
                // access by index
                // TODO removed for now, given the need to pre-evaluate in order to determine what type is produced from a heterogeneous tuple
                // Idx(be)  => {
                //     // lookup type of ref & accessor
                //     let t1 = tc_lookup(&n, env)?;
                //     let (t2,env1) = tc_expr(*be,env)?;

                //     if t2 != BaseType("int") {
                //         // bad index
                //         tc_fail("Indexed access of '" + n + "' expected an int, but got an expr of type '" + t2 + "' instead");

                //     } else {
                //         // tc the expr at the provided location
                //         match t1 {
                //             Tuple(typevect) => {
                //                 // return and unbox that entry
                //                 tc_pass(
                //                     typevect[], // TODO index cannot be determined without evaluating first, need a value to determine what kind of value is produced here for heterogeneous vectors
                //                     env1
                //                 );
                //             },
                //             _               => tc_fail("Expected index of tuple type, but instead attempted to index type '" + t1 + "'")
                //         }
                //     }
                // }
            }
        },


        // fill in the rest here, and just call out the relevant handler
        _ => tc_fail("Unrecognized expression!")
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
