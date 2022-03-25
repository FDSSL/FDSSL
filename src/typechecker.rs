use crate::typechecker;

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

#
# Typing Rules
#

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


## TC(Def)
n ∉ Γ
Γ ⊢ e : T
-----------------------
Γ ⊢ `let n : T = e` : T

IF
    term 'n' is not an element of Gamma (n is unbound)
    and Gamma entails term 'e' has type 'T'
THEN
    Gamma entails 'let n : T = e' has type 'T'


## TC(DefMut)
n ∉ Γ
Γ ⊢ e : T
--------------------------
Γ ⊢ `mut name : T = e` : T

IF
    term 'n' is not an element of Gamma (n is unbound)
    Gamma entails term 'e' has type 'T'
THEN
    Gamma entails 'mut name : T = e' has type 'T'
    (however, updates to mut bindings are OK, just not w/ 'mut' again)


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
