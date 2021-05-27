{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Data.Matrix
import Data.List
import qualified Data.Set as S


-- | Unary Operators
data UOp =
  Neg   | -- integer negation
  Flip  | -- flip the bits
  Not     -- logical negation

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

  Compose | -- special bin op for composition, does NOT output to an actual program, but just combines those shaders internally

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

data Type =
  TI |
  TB |
  TF |
  TV2 |
  TV3 |
  TV4 |
  TMat4 |
  TArray


data Expr =
  Mut Type String Expr Expr    | -- mutable binding
  Const Type String Expr Expr    | -- const binding
  Update String Expr Expr | -- updating of an env variable (effectful, and may be the last statement)
  Out String Expr Expr | -- akin to update, but ONLY for return values in shaders
  Branch Expr Expr Expr       | -- branching
  For Expr (Maybe String) Expr Expr  | -- produces a value from evaluating expr 'int' times, e.x. let x = 1+x \n let x = 1+x \n ... let x = 1+x \n x (optional index name can be given after do
  SComment String Expr         | -- single-line comment
  BComment String Expr         | -- block comment
  Seq Expr Expr | -- sequence 2 top-level exprs (statements), used to join statements together across shaders together (composition)
  NOp | -- nop
  I Int  |    -- literal int
  B Bool |    -- bool
  F Float |   -- float
  D Double |  -- double
  V2 (Expr,Expr) |            -- vec2
  V3 (Expr,Expr,Expr) |       -- vec3
  V4 (Expr,Expr,Expr,Expr) |  -- vec4
  Mat4 (Matrix Expr) |        -- mat4, as any size for now, TODO we'll fix this later
  Array [Expr] |          -- lists, which correspond to arrays
  Ref String            | -- reference (corresponds to let bound var OR function w/ no args (global ref))
  App String [Expr] Expr | -- prefix app
  BinOp BOp Expr Expr   | -- infix app
  AccessN String String | -- structure access by name
  AccessI String Int      -- structure access by index

-- env is a list of functions
type Env = [Opaque]
type Funcs = [(Maybe ShaderType, Func)]

data ShaderType = VertShader | FragShader
  deriving (Show, Eq)

-- | Shaders have a type, a set of inputs, and a set of outputs, as well as an expr
data Shader = Shader {
  shaderType :: ShaderType,
  inEnv :: Env,
  outEnv :: Env,
  shaderBody :: Expr
  }

guard :: Bool -> Maybe ()
guard True  = Just ()
guard False = Nothing

unique :: Ord a => [a] -> [a] -> [a]
unique l r = S.toList $ S.intersection (S.fromList l) (S.fromList r)

class Composable a where
 comp :: a -> a -> Maybe a

instance Composable Env where
  comp l r = do
               guard (null $ unique l r)
               return (l ++ r)


-- This could be a case for Monoid
instance Composable Shader where
  comp s s' = do
                 guard (shaderType s == shaderType s')
                 ine  <- comp (inEnv s)  (inEnv s')
                 oute <- comp (outEnv s) (outEnv s')
                 let bod = Seq (shaderBody s) (shaderBody s')
                 return $ Shader (shaderType s) ine oute bod


-- program is a Global Env + Attr Env + Vertex Shader + Fragment Shader
data Prog = Prog Env Funcs Shader Shader

data OpaqueType = Uniform | Attribute | Varying

data Opaque = Opaque {
  opaqueType :: OpaqueType,
  opaqueVType :: Type,
  opaqueName :: String
  }

-- top-level exprs, similar to statements
data Ret = Body Expr String

-- func has a name, a list of name-type pairs, it's result type, and an expr
data Func = Func String [(String,Type)] Type Ret

apply :: (Expr -> Expr) -> Func -> Func
apply f (Func n p t (Body b r)) = Func n p t (Body (f b) r)

instance Eq Opaque where
  (==)    Opaque{opaqueName = on} Opaque{opaqueName = on'} = on == on'

instance Ord Opaque where
  compare Opaque{opaqueName = on} Opaque{opaqueName = on'} = compare on on'

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

attribute :: Type -> String -> Opaque
attribute = Opaque Attribute

varying :: Type -> String -> Opaque
varying = Opaque Varying

uniform :: Type -> String -> Opaque
uniform = Opaque Uniform
