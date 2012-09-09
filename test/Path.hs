
module Path (pathTests) where

import Util

pathTests = [test0, test1, test2, test3, test4, test5, test6, test7, test8]

e path = ep host1 path True True

test0 = sessionTest "No path given" $ do
  recv (time 0) (e path1) "x=y"
  send (time 0) (e pathRoot) "x=y"
  send (time 0) (e path1) "x=y"
  send (time 0) (e path1a) "x=y"
  send (time 0) (e path1b) "x=y"
  send (time 0) (e path2) "x=y"
  send (time 0) (e path2a) "x=y"
  send (time 0) (e path2b) "x=y"

test1 = sessionTest "No path given (set from subdir)" $ do
  recv (time 0) (e path1a) "x=y"
  noSend (time 0) (e pathRoot)
  send (time 0) (e path1) "x=y"
  send (time 0) (e path1a) "x=y"
  send (time 0) (e path1b) "x=y"
  noSend (time 0) (e path2)
  noSend (time 0) (e path2a)
  noSend (time 0) (e path2b)

test2 = sessionTest "One-level path given; no trailing slash" $ do
  recv (time 0) (e path1) "x=y; path=/path1"
  noSend (time 0) (e pathRoot)
  send (time 0) (e path1) "x=y"
  send (time 0) (e path1a) "x=y"
  send (time 0) (e path1b) "x=y"
  noSend (time 0) (e path2)
  noSend (time 0) (e path2a)
  noSend (time 0) (e path2b)

test3 = sessionTest "One-level path given; trailing slash" $ do
  recv (time 0) (e path1) "x=y; path=/path1/"
  noSend (time 0) (e pathRoot)
  noSend (time 0) (e path1)
  send (time 0) (e path1a) "x=y"
  send (time 0) (e path1b) "x=y"
  noSend (time 0) (e path2)
  noSend (time 0) (e path2a)
  noSend (time 0) (e path2b)

test4 = sessionTest "Deeper path given; no trailing slash" $ do
  recv (time 0) (e path1) "x=y; path=/path1/a"
  noSend (time 0) (e pathRoot)
  noSend (time 0) (e path1)
  send (time 0) (e path1a) "x=y"
  noSend (time 0) (e path1b)
  noSend (time 0) (e path2)
  noSend (time 0) (e path2a)
  noSend (time 0) (e path2b)

test5 = sessionTest "Deeper path given; trailing slash" $ do
  recv (time 0) (e path1) "x=y; path=/path1/a/"
  noSend (time 0) (e pathRoot)
  noSend (time 0) (e path1)
  noSend (time 0) (e path1a)
  noSend (time 0) (e path1b)
  noSend (time 0) (e path2)
  noSend (time 0) (e path2a)
  noSend (time 0) (e path2b)

test6 = sessionTest "Non-absolute path given" $ do
  recv (time 0) (e path1) "x=y; path=path1/a/"
  send (time 0) (e pathRoot) "x=y"
  send (time 0) (e path1) "x=y"
  send (time 0) (e path1a) "x=y"
  send (time 0) (e path1b) "x=y"
  send (time 0) (e path2) "x=y"
  send (time 0) (e path2a) "x=y"
  send (time 0) (e path2b) "x=y"

test7 = sessionTest "Empty path given" $ do
  recv (time 0) (e path1) "x=y; path"
  send (time 0) (e pathRoot) "x=y"
  send (time 0) (e path1) "x=y"
  send (time 0) (e path1a) "x=y"
  send (time 0) (e path1b) "x=y"
  send (time 0) (e path2) "x=y"
  send (time 0) (e path2a) "x=y"
  send (time 0) (e path2b) "x=y"

test8 = sessionTest "Empty path given from subdir" $ do
  recv (time 0) (e path1a) "x=y; path"
  noSend (time 0) (e pathRoot)
  send (time 0) (e path1) "x=y"
  send (time 0) (e path1a) "x=y"
  send (time 0) (e path1b) "x=y"
  noSend (time 0) (e path2)
  noSend (time 0) (e path2a)
  noSend (time 0) (e path2b)

