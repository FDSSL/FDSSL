--{-# LANGUAGE GADTs #-}
module Main where

import Data.Matrix
import Data.Vector

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


-- These are for external
data Var =
    Var
  | Vec Dim
  | Mat Dim Dim
  deriving Show

--type Env = [(Var,Matrix a)]

type Variable a = (String, Type, Either a Var)


external :: Var -> Type -> String -> Maybe (Variable a)
external Var       t      s = Just (s, t,      Right Var)
external v@(Vec _) t      s = Just (s, t,      Right v)
external m         Float  s = Just (s, Float,  Right m)
external m         Double s = Just (s, Double, Right m)
external _         _      _ = Nothing

defM :: Fractional a => String -> Matrix a -> Maybe (Variable (Matrix a))
defM s m =  case Prelude.all (\x -> x >=2 && x <= 4) [nrows m, ncols m] of
                  True  -> Just (s,Float,Left m)
                  False -> Nothing

defV :: String -> Vector a -> Maybe (Variable (Vector a))
defV s v = case (length v >= 2 && length v <= 4) of
             True -> Just (s, )

-- data Dim1 a = Vec Dim a
-- data Dim0 a = Val a
-- data Type a = Dim0 a | Dim1 a | Matrix a



--type BType = Type Bool


main :: IO ()
main = putStrLn "Hello, Haskell!"
