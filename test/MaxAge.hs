
module MaxAge (maxAgeTests) where

import Util

maxAgeTests = [test0, test1, test2, test3, test4]

e = ep host1 path1 True True

test0 = sessionTest "Positive max-age" $ do
  recv (time 0) e "x=y; max-age=42"
  send (time 41) e "x=y"
  noSend (time 42) e

test1 = sessionTest "Zero max-age" $ do
  recv (time 0) e "x=y; max-age = 0"
  noSend (time 0) e

test2 = sessionTest "Negative max-age" $ do
  recv (time 0) e "x=y; max-age =-42"
  noSend (time (-43)) e
  noSend (time (-42)) e
  noSend (time 0) e

test3 = sessionTest "Invalid max-age" $ do
  recv (time 0) e "x=y; max-age= +10"
  send (time 0) e "x=y"
  send (time 9) e "x=y"
  send (time 10) e "x=y"

test4 = sessionTest "Invalid max-age 2" $ do
  recv (time 20) e "a=b; max-age= 10g"
  send (time 20) e "a=b"
  send (time 29) e "a=b"
  send (time 30) e "a=b"

