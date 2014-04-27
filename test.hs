module Main ( main ) where

import qualified Data.ByteString.UTF8 as B
import System.Exit
import Test.HUnit
import Text.Regex.PCRE

testcases = [
  ("ℝ X", "[^ ]", "ℝ"),
  ("ℝ X", "[^ ]+", "ℝ")
  ]

testbytestring :: (String, String, String) -> Assertion
testbytestring (txt, pat, exp) = bexp @=? bact
  where btxt = B.fromString txt
        bpat = B.fromString pat
        bexp = B.fromString exp
        bact = (btxt =~ bpat :: B.ByteString)

teststring :: (String, String, String) -> Assertion
teststring (txt, pat, exp) = exp @=? act
  where act = (txt =~ pat :: String)

tests1 :: Test
tests1 = TestList $ map TestCase $ map testbytestring testcases

tests2 :: Test
tests2 = TestList $ map TestCase $ map teststring testcases

tests :: Test
tests = TestList [tests1, tests2]

main :: IO ()
main = do
  counts <- runTestTT tests
  putStrLn $ show counts
  exitWith $ getexitcode counts
  where
    getexitcode :: Counts -> ExitCode
    getexitcode c
      | (failures c) > 0 = ExitFailure 2
      | (errors c) > 0 = ExitFailure 1
      | otherwise = ExitSuccess
