
module Web.CookieJar
  ( Jar()
  , getCookies
  , receive
  , send
  , receiveHeaders
  , sendHeaders
  ) where

import qualified Data.ByteString as BS

import Control.Monad
import Data.Maybe

import Web.CookieJar.Types
import Web.CookieJar.Parser

receive :: Time -> Endpoint -> SetCookie -> Jar -> Jar
receive = undefined

send :: Time -> Jar -> Endpoint -> [Cookie]
send = undefined

receiveHeaders :: Time -> Endpoint -> ResponseHeaders -> Jar -> Jar
receiveHeaders time host =
  flip (foldr $ receive time host)
  . catMaybes
  . map (parseSetCookie . snd)
  . filter ((== "Set-Cookie") . fst)

sendHeaders :: Time -> Jar -> Endpoint -> RequestHeaders
sendHeaders = undefined

