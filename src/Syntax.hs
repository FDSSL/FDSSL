{-# LANGUAGE ExistentialQuantification #-}
-- forall a. Show a ... maybe...

module Syntax where

import Data.List

-- kinda a deep embedding...

-- for now just a string, to test with
type FDSSLProgram a = Expr a

data Expr a =
  Ref String |
  Val a |
  Var String (Expr a) (Expr a) |  -- var
  App String [Expr a]  -- app

instance Show a => Show (Expr a) where
  show (Ref s) = s
  show (Val i) = show i
  show (Var s e1 e2) = s ++ " = " ++ show e1 ++ ";\n" ++ show e2
  show (App s es)  = s ++ "(" ++ intercalate "," (map show es) ++ ")"

data Func a = Func String (Expr a)

-- instance Functor Expr where
--   fmap f (Val v) = Val (f v)
--   fmap f (Var s e1 e2) = Var s (fmap f e1) (fmap f e2)
--   fmap f (App s e) = App s (map (fmap f) e)
--   fmap f (Ref s) = (Ref s)
--
-- instance Applicative Expr where
--   pure = Val
--   (Val f) <*> (Val e) = Val (f e)
--
-- instance Monad Expr where
--   return = Val
--   (Ref s) >>= f = Ref s
--   (Val i) >>= f = let x = f i in
--

exampleProg :: FDSSLProgram Int
exampleProg = (Var "x" (Val 5) (Var "y" (Val 9) (App "f" [(Ref "x"),(Ref "y")])))
