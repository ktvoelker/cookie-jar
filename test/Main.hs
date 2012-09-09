
module Main where

import Control.Monad
import Data.Array
import Test.HUnit
import System.Exit

import qualified Network.DNS.Public as P

import Public
import Util

test0 = sessionTest "Empty" $ return ()

test1 = sessionTest "Simple key/value pair" $ do
  recv (time ! 0) (ep host1 path1 True True) "x=y"
  send (time ! 1) (ep host1 path1 True True) "x=y"

test2 = sessionTest "Overwrite key" $ do
  recv (time ! 0) (ep host1 path1 True True) "x=y"
  send (time ! 1) (ep host1 path1 True True) "x=y"
  recv (time ! 2) (ep host1 path1 True True) "x=z"
  send (time ! 3) (ep host1 path1 True True) "x=z"

timeParseTest n xs = sessionTest ("Time parsing: " ++ xs) $ do
  recv (time ! (n - 1)) e $ cs ++ "; Expires=" ++ xs
  send (time ! (n - 1)) e cs
  noSend (time ! n) e
  where
    cs = "x=y"
    e = ep host1 path1 True True

sessionTests rules = map ($ rules)
  [ test0
  , test1
  , test2
  , timeParseTest 1 "01 70 0:0:1 Janbearpig" -- 1970-01-01 00:00:01
  ]

main = do
  rulesString <- readFile "data/effective_tld_names.dat"
  let rules = P.parseRules rulesString
  c <- runTestTT $ TestList $ concatMap ($ rules) [publicSuffixTests, sessionTests]
  when (errors c /= 0 || failures c /= 0) exitFailure

