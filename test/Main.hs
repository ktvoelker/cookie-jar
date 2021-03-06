
module Main where

import Control.Monad
import Test.HUnit
import System.Exit

import qualified Network.DNS.Public as P

import Public

import Simple
import Domain
import Expires
import MaxAge
import Path
import Replace
import Persistent
import Secure
import HttpOnly
import Multiple

sessionTests rules = map ($ rules)
  $  simpleTests
  ++ domainTests
  ++ expiresTests
  ++ maxAgeTests
  ++ pathTests
  ++ replaceTests
  ++ persistentTests
  ++ secureTests
  ++ httpOnlyTests
  ++ multipleTests

main = do
  rulesString <- readFile "data/effective_tld_names.dat"
  let rules = P.parseRules rulesString
  c <- runTestTT $ TestList $ concatMap ($ rules) [publicSuffixTests, sessionTests]
  when (errors c /= 0 || failures c /= 0) exitFailure

