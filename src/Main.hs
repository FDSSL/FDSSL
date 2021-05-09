--{-# LANGUAGE GADTs #-}
--{- LANGUAGE  AllowAmbiguousTypes -}
{-# LANGUAGE TypeOperators #-}

module Main where

import Syntax
import Pretty


------------
import Data.Matrix
import Data.Vector
import Data.Typeable
import Numeric.Natural
------------

main :: IO ()
main = do
  _ <- putStrLn $ pretty exampleProg
  putStrLn "Testing"





------------------- Cody's additions
-- data Var a where
--   Val  ::        Num a => a    -> Var a
--   ValB ::                 Bool -> Var Bool
--   Vec  ::        Num a => Dim  -> [a]          -> Var a
--   VecB ::                 Dim  -> [Bool]       -> Var Bool
--   Mat  :: Fractional a => Dim  -> Dim -> [[a]] -> Var a

data Type = Double | Float | Bool | Int | Uint
  deriving Show

data Dim = Two | Three | Four
  deriving (Eq, Ord, Show)

-- I want that type damnit!
conv :: Type -> TypeRep
conv Double = let a = 1 :: Double  in typeOf a
conv Float  = let a = 1 :: Float   in typeOf a
conv Int    = let a = 1 :: Int     in typeOf a
conv Bool   = let a = True         in typeOf a
conv Uint   = let a = 1 :: Natural in typeOf a


-- These are for external
data Var =
    Var
  | Vec Dim
  | Mat Dim Dim
  deriving Show

--type Env = [(Var,Matrix a)]

data Variable a = Variable {
  name :: String,
  vtype :: TypeRep,
  value :: a
}
  deriving Show

instance Functor Variable where
  fmap f (Variable s t v) = Variable s t (f v)


boolM :: Bool -> Maybe ()
boolM True  = Just ()
boolM False = Nothing

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf f a = (boolM . f) a >> Just a


ext :: Var -> Type -> String -> Maybe (Variable Var)
ext Var       t      s = Just (Variable s (conv t)      Var)
ext v@(Vec _) t      s = Just (Variable s (conv t)      v)
ext m         Float  s = Just (Variable s (conv Float)  m)
ext m         Double s = Just (Variable s (conv Double) m)
ext _         _      _ = Nothing

checkBounds :: Int -> Bool
checkBounds x = x >= 2 && x <= 4

checkMatrix :: Matrix a -> Bool
checkMatrix = Prelude.all checkBounds . ([nrows, ncols] <*>) . \m -> [m]


defM :: (Typeable a, Fractional a) => String -> Matrix a -> Maybe (Variable (Matrix a))
defM s m =  nothingIf checkMatrix m >> Just (Variable s (typeRep m) m)


defV :: Typeable a => String -> Vector a -> Maybe (Variable (Vector a))
defV s v = nothingIf (checkBounds . Data.Vector.length) v >> Just (Variable s (typeRep v) v)



-- data Dim1 a = Vec Dim a
-- data Dim0 a = Val a
-- data Type a = Dim0 a | Dim1 a | Matrix a



--type BType = Type Bool




-- class Vectorizable a where
--   create :: String -> a -> Maybe (Variable a)

-- instance Enum a => Vectorizable (Vector a) where
--   create s v = defV s Int v

-- instance RealFloat a => Vectorizable (Vector a) where
--   create s v = defV s Float v
