
module Web.CookieJar.Parser where

import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI

import Control.Monad

import Web.CookieJar.Types
import Web.CookieJar.Parser.Date
import Web.CookieJar.Parser.Util

parseSetCookie :: Bytes -> Maybe SetCookie
parseSetCookie bs = do
  guard $ name /= BS.empty
  (0x3D, rawValue) <- BS.uncons valueWithEquals
  attrs <- parseAttributes unparsedAttributes
  return $ foldr ($) (emptySetCookie name (trim rawValue)) attrs
  where
    (nameValuePair, unparsedAttributes) = BS.span (/= semicolon) bs
    (rawName, valueWithEquals) = BS.span (/= equals) nameValuePair
    name = trim rawName

type Attribute = SetCookie -> SetCookie

parseAttributes :: Bytes -> Maybe [Attribute]
parseAttributes = mapM parseAttribute . BS.split semicolon . BS.drop 1

parseAttribute :: Bytes -> Maybe Attribute
parseAttribute bs = maybe (const $ Just id) id (lookup (CI.mk name) avParsers) value
  where
    (rawName, rawValue) = BS.span (/= equals) bs
    name = trim rawName
    value = trim rawValue

avParsers :: [(CI.CI Bytes, Bytes -> Maybe Attribute)]
avParsers =
  [ ("Expires",  parseExpires)
  , ("Max-Age",  parseMaxAge)
  , ("Domain",   parseDomain)
  , ("Path",     parsePath)
  , ("Secure",   parseSecure)
  , ("HttpOnly", parseHttpOnly)
  ]

parseSecure :: Bytes -> Maybe Attribute
parseSecure = const $ Just $ \sc -> sc { scSecure = True }

parseHttpOnly :: Bytes -> Maybe Attribute
parseHttpOnly = const $ Just $ \sc -> sc { scHttpOnly = True }

parsePath :: Bytes -> Maybe Attribute
parsePath bs
  | bs == BS.empty = f DefaultPath
  | BS.head bs /= slash = f DefaultPath
  | otherwise = f $ Path bs
  where
    f p = Just $ \sc -> sc { scPath = Just p }

parseDomain :: Bytes -> Maybe Attribute
parseDomain bs
  | bs == BS.empty = Just id
  | Just (0x2E, ds) <- BS.uncons bs = f ds
  | otherwise = f bs
  where
    f bs = Just $ \sc -> sc { scDomain = Just $ mk bs }

parseExpires :: Bytes -> Maybe Attribute
parseExpires =
  maybe (Just id) (\d -> Just $ \sc -> sc { scExpires = Just d })
  . parseDate

parseMaxAge :: Bytes -> Maybe Attribute
parseMaxAge bs
  | bs == BS.empty = Just id
  | Just (0x2D, ds) <- BS.uncons bs = f ds
  | otherwise = f bs
  where
    f ds = Just $ \sc -> sc { scMaxAge = digitsValue $ BS.unpack ds }

