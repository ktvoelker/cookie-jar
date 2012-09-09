
module Persistent (persistentTests) where

import Control.Monad.State

import Web.CookieJar (endSession)

import Util

persistentTests = [test0, test1, test2]

test0 = sessionTest "Non-persistent cookie" $ do
  recv (time 0) (ep host1 path1 True True) "x=y"
  send (time 1) (ep host1 path1 True True) "x=y"
  modify endSession
  noSend (time 2) (ep host1 path1 True True)

test1 = sessionTest "Persistent cookie set with max-age" $ do
  recv (time 0) (ep host1 path1 True True) "x=y; max-age=30"
  send (time 1) (ep host1 path1 True True) "x=y"
  modify endSession
  send (time 29) (ep host1 path1 True True) "x=y"
  noSend (time 30) (ep host1 path1 True True)

test2 = sessionTest "Persistent cookie set with expires" $ do
  recv (time 0) (ep host1 path1 True True) "x=y; expires=Jan 1, 1970 00:00:30"
  send (time 1) (ep host1 path1 True True) "x=y"
  modify endSession
  send (time 29) (ep host1 path1 True True) "x=y"
  noSend (time 30) (ep host1 path1 True True)

