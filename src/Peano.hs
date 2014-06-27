module Peano where

import Data.Monoid

class Monoid a => Semiring a where
    sone    :: a
    stimes  :: a -> a -> a

data Nat = Zero
         | Succ Nat
    deriving (Show, Read, Eq, Ord)

instance Monoid Nat where
    mempty = Zero
    mappend = plus

instance Semiring Nat where
    sone = Succ Zero
    stimes n m = mconcat $ replicate (fromEnum n) m

instance Enum Nat where
    succ = successor
    pred = predecessor
    toEnum i = iterate successor Zero !! i
    fromEnum Zero = 0
    fromEnum (Succ Zero) = 1
    fromEnum (Succ n) = fromEnum n + 1

-- So now you can do
-- fromEnum $ succ . succ . succ . succ $ Zero
-- -> 4

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

-- Test definitions
