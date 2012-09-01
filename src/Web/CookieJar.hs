
module Web.CookieJar
  ( Jar()
  , endpoint
  , receive
  , send
  , receiveHeaders
  , sendHeaders
  ) where

import qualified Data.ByteString as BS

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time

import Web.CookieJar.Types
import Web.CookieJar.Parser
import Web.CookieJar.Parser.Util

endpoint :: Bytes -> Bytes -> Bool -> Bool -> Endpoint
endpoint = Endpoint . bytesToLower

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
receive now ep@Endpoint{..} SetCookie{..} jar = if abort then jar else Jar $
  Cookie
  { cName     = scName
  , cValue    = scValue
  , cCreation = maybe now id $ fmap cCreation same
  , cAccess   = now
  , cExpires  = exp
  , cPersist  = maybe False (const True) exp
  , cDomain   = domain
  , cHostOnly = dMat == Nothing
  , cPath     = path
  , cSecure   = scSecure
  , cHttpOnly = scHttpOnly
  } : cs
  where
    (sames, cs) = 
      partition (\Cookie{..} -> (cName, cDomain, cPath) == (scName, domain, path))
      $ getCookies jar
    same = listToMaybe $ take 1 sames
    exp = fmap (flip addUTCTime now . fromIntegral) scMaxAge `mplus` scExpires
    dMat = fmap (epDomain `domainMatches`) scDomain
    -- TODO check for public suffixes and abort as needed
    abort = 
      dMat == Just False
      || scHttpOnly && not epHttp
      || fmap cHttpOnly same == Just True && not epHttp
    domain = scDomain `mplus` Just epDomain
    path = Just $ case scPath of
      Nothing -> defaultPath ep
      Just DefaultPath -> defaultPath ep
      Just (Path p) -> p

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

