
module Web.CookieJar
  ( Jar()
  , emptyJar
  , Cookie(..)
  , SetCookie(..)
  , SetCookiePath(..)
  , emptySetCookie
  , Endpoint(..)
  , receive
  , send
  , receiveHeaders
  , sendHeaders
  , endSession
  ) where

import qualified Data.ByteString as BS

import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time

import qualified Network.DNS.Public as P

import Web.CookieJar.Types
import Web.CookieJar.Parser
import Web.CookieJar.Parser.Util

domainMatches :: CI Bytes -> CI Bytes -> Bool
domainMatches bs ds
  | bs == ds
    = True
  | otherwise
    = ds' `BS.isSuffixOf` bs'
      && BS.pack [period] `BS.isSuffixOf` BS.take (BS.length bs' - BS.length ds') bs'
      && isHostName bs'
  where
    bs' = foldedCase bs
    ds' = foldedCase ds

-- TODO should be False when the string is an IP address
-- TODO is there any way to do a positive test instead?
isHostName :: Bytes -> Bool
isHostName _ = True

defaultPath :: Endpoint -> Bytes
defaultPath Endpoint{..} = case BS.uncons epPath of
  Just (0x2F, bs) ->
    let pos = last $ BS.findIndices (== slash) epPath
    in if pos == 0 then root else BS.take pos epPath
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

endSession :: Jar -> Jar
endSession = modifyCookies $ filter cPersist

expire :: Time -> Jar -> Jar
expire now = modifyCookies . filter $ not . (== Just True) . fmap (<= now) . cExpires

receive :: Time -> Endpoint -> SetCookie -> Jar -> Jar
receive now ep@Endpoint{..} SetCookie{..} jar =
  expire now
  $ if abort then jar else Jar (jarRules jar)
  $ Cookie
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
      $ jarCookies jar
    same = listToMaybe $ take 1 sames
    {--
     - Non-positive max-age values are supposed to result in an expiration
     - date set to the "earliest representable time" (section 5.2.2).
     - However, UTCTime does not have an earliest representable time, as it
     - allows negative days, which are stored using infinite-precision
     - integers.
     -
     - This isn't a huge problem, because the effect of setting a cookie to
     - expire at the earliest representable time is that it expires
     - immediately, which is an effect we can achieve by setting the
     - expiration time to any time equal to or earlier than now.
     --}
    exp =
      if fmap (< 0) scMaxAge == Just True
        then Just now
        else fmap (flip addUTCTime now . fromIntegral) scMaxAge `mplus` scExpires
    public =
      let d = fmap original scDomain >>= P.makeDomain
      in isJust d && fmap (P.publicSuffix $ jarRules jar) d == d
    exactDomain = scDomain == Just epDomain
    scDomain' = if public && exactDomain then Nothing else scDomain
    dMat = fmap (epDomain `domainMatches`) scDomain'
    abort = 
      dMat == Just False
      || scHttpOnly && not epHttp
      || fmap cHttpOnly same == Just True && not epHttp
      || public && not exactDomain
    domain = maybe epDomain id scDomain'
    path = case scPath of
      Nothing -> defaultPath ep
      Just DefaultPath -> defaultPath ep
      Just (Path p) -> p

sendNoExpire :: Time -> Jar -> Endpoint -> ([Cookie], Jar)
sendNoExpire now jar ep =
  (sortBy headerOrder send', Jar (jarRules jar) $ send' ++ noSend)
  where
    (send, noSend) = partition (shouldSend ep) $ jarCookies jar
    send' = map (\c -> c { cAccess = now }) send

headerOrder :: Cookie -> Cookie -> Ordering
headerOrder a b = let f = BS.length . cPath in case compare (f b) (f a) of
  EQ -> compare (cCreation a) (cCreation b)
  o -> o

shouldSend :: Endpoint -> Cookie -> Bool
shouldSend Endpoint{..} Cookie{..} =
  hostOk
  && epPath `pathMatches` cPath
  && (not cSecure || epSecure)
  && (not cHttpOnly || epHttp)
  where
    hostOk =
      cHostOnly && epDomain == cDomain
      || not cHostOnly && epDomain `domainMatches` cDomain

send :: Time -> Jar -> Endpoint -> ([Cookie], Jar)
send now = sendNoExpire now . expire now

receiveHeaders :: Time -> Endpoint -> ResponseHeaders -> Jar -> Jar
receiveHeaders time host =
  flip (foldr $ receive time host)
  . catMaybes
  . map (parseSetCookie . snd)
  . filter ((== "Set-Cookie") . fst)

makeHeaderValue :: Cookie -> Bytes
makeHeaderValue Cookie{..} = cName `BS.append` BS.cons equals cValue

sendHeaders :: Time -> Jar -> Endpoint -> (RequestHeaders, Jar)
sendHeaders now jar ep = (map ("Cookie",) bs, jar')
  where
    bs = case map makeHeaderValue cs of
      [] -> []
      (b : bs) -> [BS.concat $ b : concatMap ((sep :) . (: [])) bs]
    sep = BS.pack [semicolon, space]
    (cs, jar') = send now jar ep

