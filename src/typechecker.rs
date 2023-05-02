/*

TODO @montymxb: Alright, plan of action, I wrote out a bunch of 'typing'
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
use itertools::Itertools;
use std::collections::{HashMap, HashSet};


// Type env, contains bindings of 'things' to types...these can be Exprs or Names
// w/ a HashMap our insertions & lookups pretty bad at O(n), but that's in the worst possible case, on average we should be seeing O(1)
// type TCEnv = HashMap<String,ParsedType>;
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TCEnv {
    env: HashMap<String, ParsedType>,
    acc: HashSet<ParsedType>,
}

impl TCEnv {
    pub fn insert(&mut self, n: String, t: ParsedType) -> Option<ParsedType> {
        self.env.insert(n, t)

    }
    pub fn get(&self, n: &str) -> Option<&ParsedType> {
        self.env.get(&n.to_string())
    }
    pub fn new() -> TCEnv {
        TCEnv { env: HashMap::new(), acc: HashSet::new() }
    }
    pub fn accumulate(&self, env: TCEnv) {
        self.acc.extend(env.acc.into_iter())
    }
}


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
        None => Err(format!("Non-existant binding '{n}' referenced, perhaps you meant to declare it first with 'let' or 'mut'?")),
    }
}

/// Attempts to typecheck a program
pub fn tc_program(p: Program) -> TCResult {
    let env = TCEnv { env: HashMap::new(), acc: HashSet::new()};
    tc_body(p, &env)
}

/// Typechecks a vector of Exprs w/ a given environment
/// Verifies all of them before returning the result of the last Expr
/// Expects a body of 1 or more Exprs to verify
fn tc_body(body: Vec<Expr>, env: &TCEnv) -> TCResult {
    if body.is_empty() {
        // must have something to work with
        tc_fail("Unable to typecheck an empty body!".to_string())

    } else {
        body.into_iter().fold(
            Ok((BaseType("".to_string()), env.clone())),
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

/// Returns whether a binary operator produces a numeric value
fn op_num_type(bop: &syntax::BOp) -> bool {
    matches!(bop,
        syntax::BOp::Mod |
        syntax::BOp::Gt  |
        syntax::BOp::Gte |
        syntax::BOp::Lte |
        syntax::BOp::Lt  |
        syntax::BOp::Add |
        syntax::BOp::Sub |
        syntax::BOp::Mul |
        syntax::BOp::Div
    )
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
                Err(format!("Type {b1} is not supported by binary operations"))
            }
            else if ! types.contains(b2) {
                Err(format!("Type {b2} is not supported by binary operations"))
            }
            else if (b1 == "bool" || b2 == "bool") && op_num_type(&bop) {
                Err(format!("Operator {bop} does not support bool as an argument"))
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
                    (syntax::BOp::Mod, _, _, "int") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::And, true, _, "bool") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::Or, true, _, "bool")  => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::BitAnd, true, _, "int") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::BitOr, true, _, "int") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
                    (syntax::BOp::BitXor, true, _, "int") => Ok(mk_func_typ(Tuple(vec![arg1.clone(),arg2]), arg1)),
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
fn uop_type(uop: UOp, arg: ParsedType) -> Result<ParsedType, TCError> {
    let arg1 = arg.clone();
    let arg2 = arg.clone();
    match arg {
        BaseType(n) => {
            match (uop, n.as_str()) {
                (Negative, "int")      => Ok(mk_func_typ(arg1, arg2)),
                (Negative, "float")    => Ok(mk_func_typ(arg1, arg2)),
                (Negative, "double")   => Ok(mk_func_typ(arg1, arg2)),
        
                (Negate, "bool")   => Ok(mk_func_typ(arg1, arg2)),

                _ => Err(format!("Invalid marghing of unary op {uop} with an argument type {n}"))
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
    match e {

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
                tc_fail(format!("Definition of '{n}' has type '{t}', but was assigned to a value of type '{t1}'"))

            } else {
                // valid, add this name & type combo to our env & continue
                env1.insert(n,t.clone());
                tc_pass(
                    t,
                    env1
                )
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
                tc_fail(format!("Definition of '{n}' has type '{t}', but was assigned to a value of type '{t1}'"))

            } else {
                // valid, add this name & type combo to our env & continue
                env1.insert(n,t.clone());
                tc_pass(t, env1)
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

            let old_binding : Vec<(&String, ParsedType)> = p.iter().filter_map(
                |(n,t)| {
                    let oldval = env.insert(n.clone(), t.clone())?;
                    Some((n, oldval))
                }
            ).collect();

            // get type of the body (return type), ignoring effect on env
            let (tb,_) = tc_body(b, &env)?;

            // reset the env back
            // Do not believe clippy. The iterator is collected above to insert the parameters into env.
            old_binding.into_iter().for_each(|(n,t)| {
                env.insert(n.clone(), t);
            });

            let p : Vec<ParsedType> = p.into_iter().map(|(_,t)| t).collect();

            match p.len() {
                0 => {
                    // simple type w/ no params
                    tc_pass(tb, env)
                },
                1 => {
                    // single arg, no tuple needed for this function type
                    tc_pass(
                        Function(Box::new(p[0].clone()), Box::new(tb)),
                        env
                    )
                },
                _ => {
                    // function type to construct
                    tc_pass(
                        Function(Box::new(ParsedType::Tuple(p)), Box::new(tb)),
                        env
                    )
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
            // verify that this name exists
            let (argType, returnType) = match tc_lookup(&f, &env) {
                Ok(Function(argType, returnType)) => Ok((argType, returnType)),
                Ok(_) => Err(format!("Variable {f} is not a function.")),
                Err(a) => Err(a),
            }?;


            if argType.arity() != args.len() {
                return tc_fail(format!("Incorrect number of arguments passed to function {f}"));
            }


            let typecheckedArgs = args.into_iter().fold(
                Ok((BaseType("".to_string()), env.clone())),
                |acc, elt| {
                    if let Ok((_, accEnv)) = acc {
                        env.accumulate(accEnv);
                        tc_expr(elt, env)
                    }
                    else {
                        acc
                    }
                }
            );


            let l2 = args.len();

            // compute argument types
            // TODO @montymxb argument evaluation should update the environment instead of just cloning it
            let mut mutEnv = env.clone();

            let typecheckedArgsLen = typecheckedArgs.as_slice().len();

            // check that the lengths match up
            if typecheckedArgsLen != l2 {
                return tc_fail(format!("Incorrect number of arguments supplied to function {}", f));
            }

            // verify that all of these are success (on any failures, we should just exit out)
            for argResult in typecheckedArgs.as_slice() {
                if argResult.is_err() {
                    return tc_fail(format!("Failed to typecheck argument of function {}", f));
                }
            }

            // before checking look to lift the supplied args into a tuple
            // this will let us easily check the two types in a single go
            let argTypesComputed = typecheckedArgs.into_iter().filter(|atc| atc.is_ok()).map(|atc| {
                let (p,_) = atc.unwrap();
                return p;
            }).collect_vec();
            let mut argPTComputed : ParsedType;
            if typecheckedArgsLen > 1 {
                // unpack & wrap in a tuple
                argPTComputed = Tuple(argTypesComputed);
            } else {
                // just the single value
                argPTComputed = argTypesComputed[0].clone();

            }
            
            // type check the args
            if argPTComputed == *argType {
                // valid, return the function's return type
                return tc_pass(*retType, env);
            }
            else {
                // fail, type mismatch!
                // TODO, improve message
                return tc_fail("Function typecheck failed".to_string());
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
                        tc_pass(
                            *f2,
                            env1
                        )

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
                let (tb1,env2) = tc_body(b1, &env1)?;
                let (tb2,env3) = tc_body(b2, &env2)?;

                if tb1 == tb2 {
                    // body types match
                    tc_pass(
                        tb1,
                        env3
                    )

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
    }
}


/*

#
# Typing Rules
#


??? TC(For)
.... current implementation is not capable of being type checked, does not guarantee a value ....
.... should we consider a 'do while', given we can just guarantee a value then?
 */


//
//
// TYPECHECKER TESTS
//
//


/// Tests typechecker env lookup behavior
#[test]
fn test_tc_lookup() {
    let mut tcEnv: TCEnv = TCEnv::new();
    let bt: ParsedType = typ("SomeString");
    tcEnv.insert("key".to_string(), bt.clone());

    // try a good lookup
    let r1 = tc_lookup("key", &tcEnv);
    assert_eq!(
        r1,
        Ok(bt),
        "Failed to lookup name in env during TC"
    );

    // try a bad lookup
    let r2 = tc_lookup("badKey", &tcEnv);
    assert_eq!(
        r2,
        Err(
            "Non-existant binding 'badKey' referenced, perhaps you meant to declare it first with 'let' or 'mut'?".to_string()
        ),
        "Failed to reject bad lookup"
    )
}

#[test]
fn test_tc_body() {
    let tcEnv = TCEnv::new();

    // typecheck an empty body, should fail
    let body: Vec<Expr> = vec![];
    let r1: TCResult = tc_body(body, &tcEnv);
    assert_eq!(
        r1,
        Err("Unable to typecheck an empty body!".to_string()),
        "Should have rejected an empty program body"
    );
}

#[test]
fn test_type_arity() {
    // base type
    let a1: usize = type_arity(BaseType("".to_string()));
    // tuple
    let a2: usize = type_arity(Tuple(vec![
        typ("a"),
        typ("b"),
        typ("c")
    ]));
    // named tuple
    let a3: usize = type_arity(NamedTuple(vec![
        ("".to_string(), Box::new(typ("e")))
    ]));
    // function type
    let a4: usize = type_arity(Function(
        Box::new(typ("a")),
        Box::new(typ("b"))
    ));

    assert_eq!(a1, 1, "Arity was wrong for a BaseType");
    assert_eq!(a2, 3, "Arity was wrong for a Tuple");
    assert_eq!(a3, 1, "Arity was wrong for a NamedTuple");
    assert_eq!(a4, 1, "Arity was wrong for a Function");
}


#[test]
fn test_op_num_type() {
    // verify basic binary ops are correctly classified as numeric (or not)
    assert_eq!(op_num_type(&BOp::Mod), true, "Failed to identify Mod as numeric");
    assert_eq!(op_num_type(&BOp::Add), true, "Failed to identify Add as numeric");
    assert_eq!(op_num_type(&BOp::Sub), true, "Failed to identify Sub as numeric");
    assert_eq!(op_num_type(&BOp::Mul), true, "Failed to identify Mul as numeric");
    assert_eq!(op_num_type(&BOp::Div), true, "Failed to identify Div as numeric");

    assert_eq!(op_num_type(&BOp::Gt), true, "Failed to identify Gt as numeric");
    assert_eq!(op_num_type(&BOp::Gte), true, "Failed to identify Gte as numeric");
    assert_eq!(op_num_type(&BOp::Lt), true, "Failed to identify Lt as numeric");
    assert_eq!(op_num_type(&BOp::Lte), true, "Failed to identify Lte as numeric");

    // some non numerics
    assert_eq!(op_num_type(&BOp::Or), false, "Failed to identify Or as non-numeric");
    assert_eq!(op_num_type(&BOp::And), false, "Failed to identify And as non-numeric");
    assert_eq!(op_num_type(&BOp::Eq), false, "Failed to identify Eq as non-numeric");
    assert_eq!(op_num_type(&BOp::Neq), false, "Failed to identify Neq as non-numeric");
}

/// Pulled from std (lib), seems we don't have this in our version?
/// https://docs.rs/assert_ok/latest/src/assert_ok/lib.rs.html#1-23 (should be in latest version right?)
/// Assert that a [`Result`] is [`Ok`]
///
/// If the provided expresion evaulates to [`Ok`], then the
/// macro returns the value contained within the [`Ok`]. If
/// the [`Result`] is an [`Err`] then the macro will [`panic`]
/// with a message that includes the expression and the error.
#[macro_export]
macro_rules! assert_ok {
    ( $x:expr ) => {
        match $x {
            std::result::Result::Ok(v) => v,
            std::result::Result::Err(e) => {
                panic!("Error calling {}: {:?}", stringify!($x), e);
            }
        }
    };
}

/// Typecheck various expressions
#[test]
fn test_tc_expr() {
    let mut tcEnv: TCEnv = TCEnv::new();
    tcEnv.insert("ref".to_string(), typ("int"));
    tcEnv.insert("add".to_string(), mk_bin_func(typ("int")));

    assert_eq!(tc_expr(Expr::I(32), tcEnv.clone()), Ok((typ("int"), tcEnv.clone())));
    assert_eq!(tc_expr(Expr::F(32.32), tcEnv.clone()), Ok((typ("float"), tcEnv.clone())));
    assert_eq!(tc_expr(Expr::D(32.32), tcEnv.clone()), Ok((typ("double"), tcEnv.clone())));
    assert_eq!(tc_expr(Expr::B(true), tcEnv.clone()), Ok((typ("bool"), tcEnv.clone())));
    assert_eq!(tc_expr(Expr::Ref("ref".to_string()), tcEnv.clone()), Ok((typ("int"), tcEnv.clone())));
    
    // verify func app
    let app: Expr = Expr::App { fname: "add".to_string(), arguments: vec![
        Expr::I(3),
        Expr::I(7)
    ]};
    assert_eq!(tc_expr(app, tcEnv.clone()), Ok((typ("int"), tcEnv.clone())));

    // TODO @montymxb add test for do/while (instead of for), we can desugar that as needed I think
    // TODO @montymxb test for Abstraction would be good
    // TODO @montymxb test for branch would be good here too

}
