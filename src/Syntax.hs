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

-- top-level exprs, similar to statements
data Ret = Body Expr String | Uniform | Attribute | Varying

data Expr =
  Let Type String Expr Expr    | -- let binding
  Update String Expr (Maybe Expr) | -- updating of an env variable (effectful, and may be the last statement)
  Branch Expr Expr Expr       | -- branching
  For Int (Maybe String) Expr Expr  | -- produces a value from evaluating expr 'int' times, e.x. let x = 1+x \n let x = 1+x \n ... let x = 1+x \n x (optional index name can be given after do
  SComment String Expr         | -- single-line comment
  BComment String Expr         | -- block comment
  Seq Expr Expr | -- sequence 2 top-level exprs (statements), used to join statements together across shaders together (composition)
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
  App String [Expr]     | -- prefix app
  BinOp BOp Expr Expr   | -- infix app
  AccessN String String | -- structure access by name
  AccessI String Int      -- structure access by index

-- env is a list of functions
type Env = [Func]

data ShaderType = VertShader | FragShader
  deriving (Show, Eq)

data Shader = Shader ShaderType Env Expr

class Composable a where
 comp :: a -> a -> Maybe a

instance Composable [Func] where
  comp l r = case null $ S.intersection (S.fromList l) (S.fromList r) of
                     True  -> Just (l++r)
                     False -> Nothing

-- This could be a case for Monoid
instance Composable Shader where
  comp (Shader s e t) (Shader s' e' t') = sameS s s' >> comp e e' >>= \e -> Just $ Shader s e (Seq t t')
    where
      sameS a b = if a == b then Just () else Nothing


-- program is a Global Env + Attr Env + Vertex Shader + Fragment Shader
data Prog = Prog Env Env Shader Shader

-- func has a name, a list of name-type pairs, it's result type, and an expr
data Func = Func String [(String,Type)] Type Ret

apply :: (Expr -> Expr) -> Func -> Func
apply f (Func n p t (Body b r)) = Func n p t (Body (f b) r)
apply _ f = f


--commentS :: String -> Func -> Func

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

defineExternal :: Type -> String -> Ret -> Func
defineExternal t s e = Func s [] t e

-- | Produce a varying var, passed to next part of the shader
varying :: Type -> String -> Func
varying t s = defineExternal t s Varying

-- | Attributes are produced in the same way
attribute :: Type -> String -> Func
attribute t s = defineExternal t s Attribute

-- | Uniforms's are produced in much the sameway
uniform :: Type -> String -> Func
uniform t s = defineExternal t s Uniform
