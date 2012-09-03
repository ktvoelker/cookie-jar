
module Main where

import Control.Monad
import Data.Array
import Test.HUnit
import System.Exit

import qualified Network.DNS.Public as P

import Public
import Util

empty = sessionTest "Empty" $ return ()

simple = sessionTest "Simple key/value pair" $ do
  recv (time ! 0) (ep host1 path1 True True) "x=y"
  send (time ! 1) (ep host1 path1 True True) "x=y"

sessionTests rules = map ($ rules)
  [ empty
  , simple
  ]

main = do
  rulesString <- readFile "data/effective_tld_names.dat"
  let rules = P.parseRules rulesString
  c <- runTestTT $ TestList $ concatMap ($ rules) [publicSuffixTests, sessionTests]
  when (errors c /= 0 || failures c /= 0) exitFailure

