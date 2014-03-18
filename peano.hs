module Peano where

data Nat = Zero
         | Succ Nat
    deriving (Show, Read, Eq)


isZero :: Nat -> Bool
isZero Zero = True
isZero (Succ _) = False

predecessor :: Nat -> Nat
predecessor Zero = undefined
predecessor (Succ n) = n

successor :: Nat -> Nat
{- successor n = Succ n -}
successor = Succ

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ n) o = successor $ n `plus` o

minus :: Nat -> Nat -> Nat
minus Zero Zero = Zero
minus Zero (Succ _) = undefined
minus n@(Succ _) Zero = n
minus (Succ n) (Succ o) = n `minus` o
