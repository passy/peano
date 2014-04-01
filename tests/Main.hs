module Main where

import Peano

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Peano" [

        ]
    ]
