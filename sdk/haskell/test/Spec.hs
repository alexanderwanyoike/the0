module Main (main) where

import Test.Hspec
import qualified InputSpec

main :: IO ()
main = hspec InputSpec.spec
