
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
import Path
import Replace
import Persistent
import Secure

sessionTests rules = map ($ rules)
  $  simpleTests
  ++ domainTests
  ++ expiresTests
  ++ maxAgeTests
  ++ pathTests
  ++ replaceTests
  ++ persistentTests
  ++ secureTests

main = do
  rulesString <- readFile "data/effective_tld_names.dat"
  let rules = P.parseRules rulesString
  c <- runTestTT $ TestList $ concatMap ($ rules) [publicSuffixTests, sessionTests]
  when (errors c /= 0 || failures c /= 0) exitFailure

