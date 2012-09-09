
module Simple (simpleTests) where

import Util

simpleTests = [test0, test1, test2]

test0 = sessionTest "Empty" $ return ()

test1 = sessionTest "Simple key/value pair" $ do
  recv (time 0) (ep host1 path1 True True) "x=y"
  send (time 1) (ep host1 path1 True True) "x=y"

test2 = sessionTest "Overwrite key" $ do
  recv (time 0) (ep host1 path1 True True) "x=y"
  send (time 1) (ep host1 path1 True True) "x=y"
  recv (time 2) (ep host1 path1 True True) "x=z"
  send (time 3) (ep host1 path1 True True) "x=z"

