module Main (main) where

import Test.Hspec
import qualified InputSpec
import qualified StateSpec
import qualified QuerySpec

main :: IO ()
main = hspec $ do
    InputSpec.spec
    StateSpec.spec
    QuerySpec.spec
