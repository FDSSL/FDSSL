{-# LANGUAGE FlexibleInstances #-}

module Syntax where

--
-- FDSSL Syntax
--
-- Uses an abstract syntax that is not parametric or higher-ordered
--

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

-- | Simple show instances for BinOps
instance Show BOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"

  show And  = "&&"
  show Or   = "||"
  show Eq   = "=="
  show Neq  = "!="
  show Gte  = ">="
  show Gt   = ">"
  show Lte  = "<="
  show Lt   = "<"

  show BitAnd = "&"
  show BitOr  = "|"
  show BitXor = "^"

data Type =
  TI |
  TB |
  TF |
  TV2 |
  TV3 |
  TV4 |
  TMat4 |
  TArray |
  TNull
  deriving (Eq, Ord)

-- | Simple show instances for types
instance Show Type where
  show TI = "int"
  show TB = "bool"
  show TF = "float"
  show TV2 = "vec2"
  show TV3 = "vec3"
  show TV4 = "vec4"
  show TMat4 = "mat4"
  show _ = error "Some type not implemented yet in show instance for 'Type' in Syntax.hs"

-- | FDSSL Expressions
-- There is a mixing here between actual expressions and what would constitute statements
-- We would like to demarc this in future versions
data Expr =
  Mut Type String Expr            | -- mutable binding
  Const Type String Expr          | -- const binding
  Update String Expr              | -- updating of an env variable (effectful, and may be the last statement)
  Out String Expr                 | -- akin to update, but ONLY for return values in shaders
  Branch Expr Block Block       | -- branching
  For Expr (Maybe String) Block  | -- produces a value from evaluating expr 'int' times, e.x. let x = 1+x \n let x = 1+x \n ... let x = 1+x \n x (optional index name can be given after do
  SComment String                 | -- single-line comment
  BComment String                 | -- block comment
  NOp                             | -- nop
  I Int                           |    -- literal int
  B Bool                          |    -- bool
  F Float                         |   -- float
  D Double                        |  -- double
  V2 (Expr,Expr)                  |            -- vec2
  V3 (Expr,Expr,Expr)             |       -- vec3
  V4 (Expr,Expr,Expr,Expr)        |  -- vec4
  Mat4 (Matrix Expr)              |        -- mat4, as any size for now, TODO we'll fix this later
  Array [Expr]                    |          -- lists, which correspond to arrays
  Ref String                      | -- reference (corresponds to let bound var OR function w/ no args (global ref))
  App String [Expr]               | -- prefix app
  BinOp BOp Expr Expr             | -- infix app
  AccessN String String           | -- structure access by name
  AccessI String Int                -- structure access by index
  deriving Show

-- | Whether an expr is an immediate value or not
isImm :: Expr -> Bool
isImm (I _) = True
isImm (B _) = True
isImm (F _) = True
isImm (D _) = True
isImm (V2 _) = True
isImm (V3 _) = True
isImm (V4 _) = True
isImm (Mat4 _) = True
isImm (Ref _) = True
isImm (AccessN _ _) = True
isImm (AccessI _ _) = True
isImm (BinOp _ _ _) = True
isImm (App _ _) = True
isImm _ = False

-- | Used to denote constraints on functions
data Req = Needs Type
         | None
         | Nott Type

-- | Denotes constraints on binary operations
-- Takes a Binary operator
-- Returns parameter type and return type
bopType :: BOp -> (Req, Req)
bopType Add     = (Nott TB, None)
bopType Sub     = (Nott TB, None)
bopType Mul     = (Nott TB, None)
bopType Div     = (Nott TB, None)
bopType Mod     = (Needs TI, Needs TI)
bopType And     = (Needs TB, Needs TB)
bopType Or      = (Needs TB, Needs TB)
bopType Compose = (Needs TNull, Needs TNull)
bopType Eq      = (None, Needs TB)
bopType Neq     = (None, Needs TB)
bopType Gte     = (Nott TB, Needs TB)
bopType Gt      = (Nott TB, Needs TB)
bopType Lte     = (Nott TB, Needs TB)
bopType Lt      = (Nott TB, Needs TB)
bopType BitAnd  = (Nott TB, None)
bopType BitOr   = (Nott TB, None)
bopType BitXor  = (Nott TB, None)

-- env is a list of functions
type Env = [Opaque]
type Block = [Expr]

-- | List of Functions that may be built-in to specific shader stage
type Funcs = [(Maybe ShaderType, Func)]

-- | Shader types, Vertex or Fragment (not including Tesselation, Geometry, or Compute shaders here)
data ShaderType = VertShader | FragShader
  deriving (Show, Eq, Ord)

-- | Shaders have a type, a set of inputs, and a set of outputs, as well as an expr
data Shader = Shader {
  shaderType :: ShaderType,
  inEnv :: Env,
  outEnv :: Env,
  shaderBody :: [Expr]
}
  deriving Show

-- | General Guard
guard :: Bool -> Maybe ()
guard True  = Just ()
guard False = Nothing

-- | Retrive duplicates (if any) from a list
duplicates :: Ord a => [a] -> [a] -> [a]
duplicates l r = S.toList $ S.intersection (S.fromList l) (S.fromList r)

-- | Typeclass for defining our form of composition
-- Composition in this fashion is not a -> b and b -> c producing a -> c,
-- but is a gluing of the inputs & outputs to combine their effects
-- so our form is as follows
-- a -> b & c -> d
-- (a,c) -> (c,d)
-- it's a literal stacking of function definitions
-- We do this to resemble the singular structure of a shader w/ several ins & outs
class Composable a where
 comp :: a -> a -> Maybe a

instance Composable Env where
  comp l r = return $ S.toList $ S.fromList $ l ++ r


-- This could be a case for Monoid
instance Composable Shader where
  comp s s' = do
                 -- can only compose shaders of the same type
                 guard (shaderType s == shaderType s')
                 -- simply join the inputs & outputs uniquely
                 let ui = S.toList $ S.fromList $ inEnv s ++ inEnv s'
                 let uo = S.toList $ S.fromList $ outEnv s ++ outEnv s'
                 -- return a new composite shader
                 return $ Shader (shaderType s) ui uo (shaderBody s ++ shaderBody s')


-- | Program contains an Env, Functions, a Vertex shader and a Fragment shader
data Prog = Prog Env Funcs Shader Shader
  deriving Show

-- | An opaque type is one that is external to a stage in a pipeline,
-- where a stage is either the Vertex or Fragment shader
data OpaqueType = Uniform | Attribute | Varying
  deriving Eq

instance Show OpaqueType where
  show Uniform   = "uniform"
  show Attribute = "attribute"
  show Varying   = "varying"

-- | An opaque value is external value with a given modifier, known type, and a name
-- Opaque values correspond to values provided outside from a shader stage (or outside from the whole program entirely).
-- Their values are not capable of being known in their context where they are received into a stage.
data Opaque = Opaque {
  opaqueType  :: OpaqueType,
  opaqueVType :: Type,
  opaqueName  :: String
  }

instance Show Opaque where
  show (Opaque ot t n) = (++";\n") . mconcat $ intersperse " " $ [show ot, show t, n]

-- top-level exprs, similar to statements

-- | Func has a name, a list of name-type pairs, it's result type, and an expr
data Func = Func {
  funcName :: String,
  funcParams :: [(String,Type)],
  funcRetType :: Type,
  funcBody :: Block
  }
  deriving Show

apply :: (Block -> Block) -> Func -> Func
apply f (Func n p t b) = Func n p t (f b)


instance Eq Opaque where
  (==)    Opaque{opaqueName = on} Opaque{opaqueName = on'} = on == on'

instance Ord Opaque where
  compare Opaque{opaqueName = on} Opaque{opaqueName = on'} = compare on on'

-- | During evaluation (for programs written in the AST directly), a value can be wrapped into a known expr if it derives wrappable
-- so we can do w/e we want for these, functions, and wrap the results without having to do it explicitly ourselves every time, it's either supported or it's not
-- this is a vestige of sorts, and was more relevant when we wrote programs in their AST form directly within Haskell
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
