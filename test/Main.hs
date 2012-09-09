
module Main where

import Control.Monad
import Data.Array
import Test.HUnit
import System.Exit

import qualified Network.DNS.Public as P

import Public
import Util

import Simple
import Domain
import Expires
import MaxAge

sessionTests rules = map ($ rules)
  $  simpleTests
  ++ domainTests
  ++ expiresTests
  ++ maxAgeTests

main = do
  rulesString <- readFile "data/effective_tld_names.dat"
  let rules = P.parseRules rulesString
  c <- runTestTT $ TestList $ concatMap ($ rules) [publicSuffixTests, sessionTests]
  when (errors c /= 0 || failures c /= 0) exitFailure

