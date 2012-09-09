
module Domain (domainTests) where

import Util

domainTests = [test0, test1, test2, test3, test4]

test0 = sessionTest "Host-only cookie" $ do
  recv (time 0) (ep host1 path1 True True) "w=x"
  send (time 0) (ep host1 path1 True True) "w=x"
  noSend (time 0) (ep host1a path1 True True)
  noSend (time 0) (ep host1b path1 True True)

test1 = sessionTest "Explicit domain on self" $ do
  recv (time 0) (ep host1 path1 True True) "y=z; domain=host1.example"
  send (time 0) (ep host1 path1 True True) "y=z"
  send (time 0) (ep host1a path1 True True) "y=z"
  send (time 0) (ep host1b path1 True True) "y=z"

test2 = sessionTest "Explicit domain on parent" $ do
  recv (time 0) (ep host1a path1 True True) "a=b; domain=host1.example"
  send (time 0) (ep host1 path1 True True) "a=b"
  send (time 0) (ep host1a path1 True True) "a=b"
  send (time 0) (ep host1b path1 True True) "a=b"

test3 = sessionTest "Host-only on subdomain" $ do
  recv (time 0) (ep host1a path1 True True) "x=y"
  noSend (time 0) (ep host1 path1 True True)
  send (time 0) (ep host1a path1 True True) "x=y"
  noSend (time 0) (ep host1b path1 True True)

test4 = sessionTest "Explicit domain on parent (leading dot)" $ do
  recv (time 0) (ep host1a path1 True True) "a=b; domain=.host1.example"
  send (time 0) (ep host1 path1 True True) "a=b"
  send (time 0) (ep host1a path1 True True) "a=b"
  send (time 0) (ep host1b path1 True True) "a=b"

