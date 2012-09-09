
module Replace (replaceTests) where

import Control.Monad.State
import Test.HUnit

import Web.CookieJar.Types

import Util

replaceTests = [test0, test1, test2, test3, test4]

check c a = do
  jar <- get
  liftIO $ do
    assertEqual "Creation time" (time c) . cCreation . head . jarCookies $ jar
    assertEqual "Last access time" (time a) . cAccess . head . jarCookies $ jar

test0 = sessionTest "Replace cookie" $ do
  recv (time 0) (ep host1 path1 True True) "x=y"
  check 0 0
  send (time 1) (ep host1 path1 True True) "x=y"
  check 0 1
  recv (time 2) (ep host1 path1 True True) "x=z"
  check 0 2
  send (time 3) (ep host1 path1 True True) "x=z"
  check 0 3

test1 = sessionTest "Replace host-only cookie with explicit domain" $ do
  recv (time 0) (ep host1a path1 True True) "x=y"
  check 0 0
  send (time 1) (ep host1a path1 True True) "x=y"
  check 0 1
  recv (time 2) (ep host1a path1 True True) "x=z; domain=a.host1.example"
  check 0 2
  send (time 3) (ep host1a path1 True True) "x=z"
  check 0 3

test2 = sessionTest "Replace default-path cookie with explicit path" $ do
  recv (time 0) (ep host1 path1a True True) "x=y"
  check 0 0
  send (time 1) (ep host1 path1 True True) "x=y"
  check 0 1
  send (time 1) (ep host1 path1a True True) "x=y"
  check 0 1
  recv (time 2) (ep host1 path1 True True) "x=z; path=/path1"
  check 0 2
  send (time 3) (ep host1 path1 True True) "x=z"
  check 0 3
  send (time 3) (ep host1 path1a True True) "x=z"
  check 0 3

test3 = sessionTest "Last access time" $ do
  recv (time 0) (ep host1 path1a True True) "x=y"
  check 0 0
  noSend (time 1) (ep host2 path1a True True)
  check 0 0
  send (time 3) (ep host1 path1 True True) "x=y"
  check 0 3
  noSend (time 7) (ep host1 path2 True True)
  check 0 3
  send (time 6) (ep host1 path1b True True) "x=y"
  check 0 6

test4 = sessionTest "Delete cookie" $ do
  recv (time 0) (ep host1 path1 True True) "x=y"
  check 0 0
  send (time 1) (ep host1 path1 True True) "x=y"
  check 0 1
  recv (time 2) (ep host1 path1 True True) "x=; max-age=0"
  noSend (time 3) (ep host1 path1 True True)
  recv (time 4) (ep host1 path1 True True) "x=z"
  check 4 4
  send (time 5) (ep host1 path1 True True) "x=z"
  check 4 5

