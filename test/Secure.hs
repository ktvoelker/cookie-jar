
module Secure (secureTests) where

import Util

secureTests = [test0, test1, test2, test3]

t = time 0
e = ep host1 path1 True

test0 = sessionTest "Set secure flag" $ do
  recv t (e True) "x=y; secure"
  send t (e True) "x=y"
  noSend t (e False)

test1 = sessionTest "Overwrite secure cookie with non-secure cookie" $ do
  recv t (e True) "x=y; secure"
  send t (e True) "x=y"
  noSend t (e False)
  recv t (e False) "x=z"
  send t (e True) "x=z"
  send t (e False) "x=z"

test2 = sessionTest "Set secure flag from non-secure host" $ do
  recv t (e False) "x=y; secure"
  send t (e True) "x=y"
  noSend t (e False)

test3 = sessionTest "No secure flag" $ do
  recv t (e True) "x=y"
  send t (e True) "x=y"
  send t (e False) "x=y"

