module Main (main) where

import Test.Hspec
import qualified Test1 as T1 (spec)
import qualified Test2 as T2 (spec)

main :: IO ()
main = hspec $ do
    T1.spec
    T2.spec