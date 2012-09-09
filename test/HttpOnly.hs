
module HttpOnly (httpOnlyTests) where

import Util

httpOnlyTests = [test0, test1, test2, test3, test4, test5]

t = time 0
e ho = ep host1 path1 ho False

test0 = sessionTest "Set HttpOnly flag" $ do
  recv t (e True) "x=y; HttpOnly"
  send t (e True) "x=y"
  noSend t (e False)

test1 = sessionTest "Try to set HttpOnly from non-HTTP source" $ do
  recv t (e False) "x=y; HttpOnly"
  noSend t (e True)
  noSend t (e False)

test2 = sessionTest "Try to overwrite HttpOnly cookie from non-HTTP source" $ do
  recv t (e True) "x=y; HttpOnly"
  send t (e True) "x=y"
  noSend t (e False)
  recv t (e False) "x=z; HttpOnly"
  send t (e True) "x=y"
  noSend t (e False)

test3 = sessionTest "Try to remove HttpOnly flag from non-HTTP source" $ do
  recv t (e True) "x=y; HttpOnly"
  send t (e True) "x=y"
  noSend t (e False)
  recv t (e False) "x=z"
  send t (e True) "x=y"
  noSend t (e False)

test4 = sessionTest "Remove HttpOnly flag from an HTTP source" $ do
  recv t (e True) "x=y; HttpOnly"
  send t (e True) "x=y"
  noSend t (e False)
  recv t (e True) "x=z"
  send t (e True) "x=z"
  send t (e False) "x=z"

test5 = sessionTest "Overwrite HttpOnly cookie value" $ do
  recv t (e True) "x=y; HttpOnly"
  send t (e True) "x=y"
  noSend t (e False)
  recv t (e True) "x=z; HttpOnly"
  send t (e True) "x=z"
  noSend t (e False)

