
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
import Web.CookieJar.Parser.Util

domainMatches :: Bytes -> Bytes -> Bool
domainMatches bs ds
  | bs == ds
    = True
  | otherwise
    = ds `BS.isSuffixOf` bs
      && BS.pack [period] `BS.isSuffixOf` BS.take (BS.length bs - BS.length ds) bs
      && isHostName bs

-- TODO should be False when the string is an IP address
-- TODO is there any way to do a positive test instead?
isHostName :: Bytes -> Bool
isHostName _ = True

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

