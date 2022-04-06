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
use syntax::Program;
use syntax::ParsedType;

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
        Some(p) => p,
        None    => tc_fail("Non-existant binding '" + n + "' referenced, perhaps you meant to declare it first with 'let' or 'mut'?")
    }
}

/// Attempts to typecheck a program
pub fn tc_program(i: Program) -> TCResult {
    return tc_fail("Some crap");
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
                tc_fail("Definition of '" + n = "' has type '" + t + "', but was assigned to a value of type '" + t1 + "'");

            } else {
                // valid, add this name & type combo to our env & continue
                tc_pass(
                    t,
                    env.insert(n,t)
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
        /// At the moment this is very much the same as constant bindings
        /// TODO, to distinguish this during updates,
        ///     we may need a 'mut' or 'let' modifier for bindings ?, or we'll handle this elsewhere...I'll come back to this
        Expr::DefMut{name: n, typ: t, value: e} => {
            let (t1,env) = tc_expr(*e, env)?;

            if t != t1 {
                // fail if the types don't match
                tc_fail("Definition of '" + n = "' has type '" + t + "', but was assigned to a value of type '" + t1 + "'");

            } else {
                // valid, add this name & type combo to our env & continue
                tc_pass(
                    t,
                    env.insert(n,t)
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
            let t1 = tc_lookup(n)?;
            let (t2,env) = tc_expr(*v, env);
            if t1 == t2 {
                tc_pass(t1,env);
            } else {
                tc_fail("Definition of '" + n = "' has type '" + t1 + "', but was assigned to a value of type '" + t2 + "'");
            }
        },

        /*
        ## TC(Parameters)
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
        /// Tricky, in order to typecheck abstractions they have to have a type declaration separate from their calling context?
        /// Yeah need to think about that a bit more.
        Expr::Abs{params: p, body: b} => {
            // TODO, needs to haves types added to its AST representation, otherwise we simply can't get this to work
            // We will have to modify the parser to get that to work as expected
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
        /// Function application
        Expr::App{fname: f, arguments: args} => {
            let t1 = tc_lookup(f, env)?;
            let t2List = // ... map lookups across all args, giving their values
            // and then match them here, they should be equal
        }

        /// BinOp
        Expr::BinOp{operator: b, e1: e11, e2: e22} => {
            // can use '?' here to help unwrap conditional value, using maybe or Either?
            let (t1,e1) = tc_expr(*e11)?;
            let (t2,e2) = tc_expr(*e22)?;
            if t1 == t2 {
                tc_pass(
                    t1,
                    HashMap::new()
                )
            } else {
                tc_fail("Bad binop!")
            }
        },
        // fill in the rest here, and just call out the relevant handler
        _ => tc_fail("Unrecognized expression!")
    };
    return res;
}

// function that maps some type into a generic type-checked type
// tc :: a -> TCEnv -> Either TCError (type,TCEnv)
//fn tc

/*

#
# Typing Rules
#


## TC(Unary)
Γ ⊢ u : T1 -> T2
Γ ⊢ e : T1
-----------------------
Γ ⊢ u e : T2

WLOG, same as TC(App) above, but w/ no parens


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


## TC(Branch)
Γ,c : Bool ⊢ e1 = e2 : T
-------------------------------
Γ ⊢ `if(c) {e1} else {e2}` : T

IF
    Gamma extended by term 'c' with type 'Bool' implies terms 'e1' and 'e2' share the same type 'T'
THEN
    Gamma implies `if(c) {e1} else {e2}` has type 'T'


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


??? TC(For)
.... current implementation is not capable of being type checked, does not guarantee a value ....
.... should we consider a 'do while', given we can just guarantee a value then?
 */
