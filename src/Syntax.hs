{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Data.Matrix
import Data.List
import qualified Data.Set as S

-- env is a list of functions
type Env = [Func]

class Composable a where
 comp :: a -> a -> a

-- each shader environment is an env and an Expr
data VertShader = VertShader Env TExpr
  deriving Show

uniq :: Ord a => [a] -> [a]
uniq ls = S.toList $ S.fromList ls

instance Composable VertShader where
  comp (VertShader e t) (VertShader e' t') = VertShader (uniq $ e ++ e') (Seq t t')

data FragShader = FragShader Env TExpr
  deriving Show

instance Composable FragShader where
  comp (FragShader e t) (FragShader e' t') = FragShader (uniq $ e ++ e') (Seq t t')

-- program is a Global Env + Attr Env + Vertex Shader + Fragment Shader
data Prog = Prog Env Env VertShader FragShader
  deriving Show

-- | Unary Operators
data UOp =
  Neg   | -- integer negation
  Flip  | -- flip the bits
  Not     -- logical negation
  deriving Show

-- | Binary Operators
data BOp =
  Add | -- +
  Sub | -- -
  Mul | -- *
  Div | -- /
  Mod | -- %

  -- logic bin ops
  And |
  Or  |

  -- relational ops
  Eq  | -- ==
  Neq | -- !=
  Gte | -- >=
  Gt  | -- >
  Lte | -- <=
  Lt  | -- <

  -- Bit-wise Ops
  BitAnd |
  BitOr  |
  BitXor

instance Show BOp where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "
  show Mod = " % "

  show And  = " && "
  show Or   = " || "
  show Eq   = " == "
  show Neq  = " != "
  show Gte  = " >= "
  show Gt   = " > "
  show Lte  = " <= "
  show Lt   = " < "

  show BitAnd = " & "
  show BitOr  = " | "
  show BitXor = " ^ "


data Type =
  TI |
  TB |
  TF |
  TV2 |
  TV3 |
  TV4 |
  TMat4 |
  TArray

instance Show Type where
  show TI = "int"
  show TB = "bool"
  show TF = "float"
  show TV2 = "vec2"
  show TV3 = "vec3"
  show TV4 = "vec4"
  show TMat4 = "mat4"
  show _ = error "Some type not implemented yet in show instance for 'Type' in Syntax.hs"

-- top-level exprs, similar to statements
data TExpr =
  Let Type String Expr TExpr    | -- let binding
  Update String Expr (Maybe TExpr) | -- updating of an env variable (effectful, and may be the last statement)
  Branch Expr TExpr TExpr       | -- branching
  For Int (Maybe String) TExpr TExpr  | -- produces a value from evaluating expr 'int' times, e.x. let x = 1+x \n let x = 1+x \n ... let x = 1+x \n x (optional index name can be given after do
  SComment String TExpr         | -- single-line comment
  BComment String TExpr         | -- block comment
  Seq TExpr TExpr | -- sequence 2 top-level exprs (statements), used to join statements together across shaders together (composition)
  Return Expr                     -- final expr to return... since these always show up in functions

instance Show TExpr where
  show (Let t n e e') = show t ++ " " ++ n ++ " = " ++ show e ++ ";\n" ++ show e'
  show (Update n e (Just e')) = n ++ " = " ++ show e ++ ";\n" ++ show e'
  show (Update n e Nothing) = n ++ " = " ++ show e ++ ";"
  show (Branch c t e) = "if (" ++ show c ++ ") {\n" ++ show t ++ "\n} else {\n" ++ show e ++ "\n}\n"
  show (For i (Just n) e t) = "for (int " ++ n ++ " = 0; n < " ++ show i ++ "; "++n++"="++n++"+1) {\n" ++ show e ++ "\n}\n" ++ show t
  show (For i Nothing e t) = "for (int fdssl_cntr = 0; fdssl_cntr < " ++ show i ++ "; fdssl_cntr=fdssl_cntr+1) {\n" ++ show e ++ "\n}\n" ++ show t
  show (SComment s e) = "// " ++ s ++ "\n" ++ show e
  show (BComment s e) = "/* " ++ s ++ " */\n" ++ show e
  show (Seq t t') = show t ++ "\n" ++ show t'
  show (Return e) = "return " ++ show e ++ ";"

-- Expressions
data Expr =
  U | -- undefined value, used to fill in for uniforms & attributes passed in where we cannot know what they will be in advance
  I Int  |    -- literal int
  B Bool |    -- bool
  F Float |   -- float
  D Double |  -- double
  V2 (Expr,Expr) |            -- vec2
  V3 (Expr,Expr,Expr) |       -- vec3
  V4 (Expr,Expr,Expr,Expr) |  -- vec4
  Mat4 (Matrix Expr) |        -- mat4, as any size for now
  Array [Expr] |          -- lists, which correspond to arrays
  Ref String            | -- reference (corresponds to let bound var OR function w/ no args (global ref))
  App String [Expr]     | -- prefix app
  BinOp BOp Expr Expr   | -- infix app
  AccessN String String | -- structure access by name
  AccessI String Int      -- structure access by index

showParams :: [Expr] -> String
showParams ls = concat (intersperse "," (map show ls))

instance Show Expr where
  show U     = show ""
  show (I i) = show i
  show (B b) = if b then "true" else "false"
  show (F f) = show f ++ "f"
  show (D d) = show d
  show (V2 (a,b)) = "vec2(" ++ showParams [a,b] ++ ")"
  show (V3 (a,b,c)) = "vec3(" ++ showParams [a,b,c] ++ ")"
  show (V4 (a,b,c,d)) = "vec4(" ++ showParams [a,b,c,d] ++ ")"
  show (Mat4 m) = "??? mat4 isn't done yet ???"
  show (Ref r) = r
  show (App n ls) = n ++ "(" ++ showParams ls ++ ")"
  show (BinOp b e e') = show e ++ " " ++ show b ++ show e'
  show (AccessN s n) = s ++ "." ++ n
  show (AccessI s i) = s ++ "[" ++ show i ++ "]"
  show _ = error "Undefined value present! Invalid program"

-- func has a name, a list of name-type pairs, it's result type, and an expr
data Func = Func String [(String,Type)] Type TExpr

instance Show Func where
  show (Func f ls t e) = show t ++ " " ++ f ++ "(" ++ concat (intersperse "," (map (\(n,t') -> show t' ++ " " ++ n) ls)) ++ ") {\n" ++ show e ++ "\n}\n\n"

instance Eq Func where
  (==) (Func a _ _ _) (Func b _ _ _) = a == b

instance Ord Func where
  compare (Func a _ _ _) (Func b _ _ _) = compare a b

-- during evaluation, a value can be wrapped into a known expr if it derives wrappable
-- so we can do w/e we want for these, functions, and wrap the results without having to do it explicitly ourselves every time, it's either supported or it's not
class Wrappable a where
  wrap :: a -> Expr

instance Wrappable Int where
  wrap i = I i

instance Wrappable Bool where
  wrap b = B b

instance Wrappable Float where
  wrap f = F f

instance Wrappable Double where
  wrap d = D d

instance (Wrappable a) => Wrappable (a,a) where
  wrap (x,y) = V2 (wrap x, wrap y)

instance (Wrappable a) => Wrappable (a,a,a) where
  wrap (x,y,z) = V3 (wrap x, wrap y, wrap z)

instance (Wrappable a) => Wrappable (a,a,a,a) where
  wrap (x,y,z,w) = V4 (wrap x, wrap y, wrap z, wrap w)

twrap :: Wrappable a => String -> Type -> a -> (Env,Expr)
twrap n TI x = ([],wrap x)

-- | Produce a varying var, passed to next part of the shader
varying :: Type -> String -> Expr -> Func
varying t s e = Func s [] t (Return e)

-- | Attributes are produced in the same way
attribute = varying

-- | Uniforms's are produced in much the sameway
uniform = varying
