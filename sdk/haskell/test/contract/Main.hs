-- | Runs every bot contract case in sdk/contract/cases against this SDK.
--
-- The case format and the matching rules are in sdk/contract/README.md.
--
-- This test program is also the contract bot: the harness starts it again
-- with @contract-bot@ as its first argument, and it then plays one case.
-- Being one program means the bot needs no build step of its own, and hspec
-- never starts in the bot, so it prints nothing on the bot's stdout.
module Main (main) where

import qualified ContractBot
import qualified Harness
import System.Environment (getArgs)
import Test.Hspec (hspec)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [role, casePath, queryResultPath]
            | role == Harness.contractBotArg -> ContractBot.main casePath queryResultPath
        _ -> hspec Harness.spec
