
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
  recv (time 0) (ep host1 path1 True True) "x=y"
  send (time 1) (ep host1 path1 True True) "x=y"

test2 = sessionTest "Overwrite key" $ do
  recv (time 0) (ep host1 path1 True True) "x=y"
  send (time 1) (ep host1 path1 True True) "x=y"
  recv (time 2) (ep host1 path1 True True) "x=z"
  send (time 3) (ep host1 path1 True True) "x=z"

timeParseTest n xs = sessionTest ("Time parsing: " ++ xs) $ do
  recv (time $ n - 1) e $ cs ++ "; Expires=" ++ xs
  send (time $ n - 1) e cs
  noSend (time n) e
  where
    cs = "x=y"
    e = ep host1 path1 True True

sessionTests rules = map ($ rules)
  [ test0
  , test1
  , test2
  -- 1970-01-01 00:00:01
  -- strange field-orderings
  , timeParseTest 1 "01 70 0:0:1 Janbearpig"
  , timeParseTest 1 "1970 1 00:00:01 jan"
  , timeParseTest 1 "01 0:00:1 JANU 1970"
  -- valid delimiters
  , timeParseTest 1 "1970 Jan\t01!00:00:01"
  , timeParseTest 1 "1970\"Jan#01$00:00:01"
  , timeParseTest 1 "1970%Jan&01'00:00:01"
  , timeParseTest 1 "1970(Jan)01*00:00:01"
  , timeParseTest 1 "1970+Jan,01-00:00:01"
  , timeParseTest 1 "1970.Jan/01.00:00:01"
  , timeParseTest 1 " !\"#$%&'()*+,-./1970 Jan 01 00:00:01 !\"#$%&'()*+,-./"
  -- years
  , timeParseTest year "1971 Jan 01 00:00:00"
  , timeParseTest (2 * year) "1972 Jan 01 00:00:00"
  -- months
  , timeParseTest (31 * day) "1970 Feb 01 00:00:00"
  , timeParseTest ((31 + 28) * day) "1970 Mar 01 00:00:00"
  , timeParseTest (year - 31 * day) "1970 Dec 01 00:00:00"
  -- days of the month
  , timeParseTest day "1970 Jan 02 00:00:00"
  , timeParseTest (2 * day) "1970 Jan 03 00:00:00"
  -- hours
  , timeParseTest hour "1970 Jan 01 01:00:00"
  , timeParseTest (2 * hour) "1970 Jan 01 02:00:00"
  , timeParseTest (day - hour) "1970 Jan 01 23:00:00"
  -- minutes
  , timeParseTest minute "1970 Jan 01 00:01:00"
  , timeParseTest (2 * minute) "1970 Jan 01 00:02:00"
  , timeParseTest (hour - minute) "1970 Jan 01 00:59:00"
  -- seconds
  , timeParseTest second "1970 Jan 01 00:00:01"
  , timeParseTest (2 * second) "1970 Jan 01 00:00:02"
  , timeParseTest (minute - second) "1970 Jan 01 00:00:59"
  -- recent years
  , timeParseTest year2000 "Jan 01, 00 00:00:00"
  , timeParseTest (year2000 - second) "Dec 31, 99 23:59:59"
  , timeParseTest (year2000 + second) "Jan 01, 00 00:00:01"
  -- leap years
  , timeParseTest (year2000 + year + day) "Jan 01, 01 00:00:00"
  , timeParseTest (year2000 - 3 * year) "Jan 01, 97 00:00:00"
  , timeParseTest (year2000 - 4 * year - day) "Jan 01, 96 00:00:00"
  ]

main = do
  rulesString <- readFile "data/effective_tld_names.dat"
  let rules = P.parseRules rulesString
  c <- runTestTT $ TestList $ concatMap ($ rules) [publicSuffixTests, sessionTests]
  when (errors c /= 0 || failures c /= 0) exitFailure

