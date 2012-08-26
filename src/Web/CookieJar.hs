
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

defaultPath :: Endpoint -> Bytes
defaultPath Endpoint{..} = case BS.uncons epPath of
  Just (0x2F, bs) ->
    let pos = last $ BS.findIndices (== slash) epPath
    in if pos == 0 then root else BS.take (BS.length epPath - pos) epPath
  _ -> root
  where
    root = BS.pack [slash]

pathMatches :: Bytes -> Bytes -> Bool
pathMatches rp cp
  | rp == cp = True
  | pre && root `BS.isSuffixOf` cp = True
  | pre && root `BS.isPrefixOf` BS.drop (BS.length cp) rp = True
  | otherwise = False
  where
    pre = cp `BS.isPrefixOf` rp
    root = BS.pack [slash]

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

