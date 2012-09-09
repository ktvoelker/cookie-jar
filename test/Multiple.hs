
module Multiple (multipleTests) where

import Util

multipleTests = [test0, test1, test2]

test0 = sessionTest "Sorting of cookies with same-length path" $ do
  recv (time 0) (ep host1 path1 True True) "x=y"
  send (time 1) (ep host1 path1 True True) "x=y"
  recv (time 2) (ep host1 path1 True True) "a=b"
  send (time 3) (ep host1 path1 True True) "x=y; a=b"

test1 = sessionTest "Sorting of cookies with same creation time" $ do
  recv (time 0) (ep host1 path1 True True) "x=y"
  recv (time 0) (ep host1 path1a True True) "a=b"
  send (time 1) (ep host1 path1a True True) "a=b; x=y"

test2 = sessionTest "Sorting of cookies" $ do
  recv (time 0) (ep host1 path1 True True) "x=y"
  recv (time 0) (ep host1 path1a True True) "a=b"
  recv (time 1) (ep host1 path1 True True) "c=d"
  recv (time 1) (ep host1 path1a True True) "q=r"
  send (time 2) (ep host1 path1a True True) "a=b; q=r; x=y; c=d"

