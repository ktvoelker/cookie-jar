
module Web.CookieJar.Parser.Util where

import qualified Data.ByteString as BS

import Web.CookieJar.Types

colon :: Word8
colon = 0x3A

semicolon :: Word8
semicolon = 0x3B

equals :: Word8
equals = 0x3D

space :: Word8
space = 0x20

hTab :: Word8
hTab = 0x09

slash :: Word8
slash = 0x2F

period :: Word8
period = 0x2E

negative :: Word8
negative = 0x2D

isWhitespace :: Word8 -> Bool
isWhitespace w = w == space || w == hTab

trim :: Bytes -> Bytes
trim = let f = BS.reverse . BS.dropWhile isWhitespace in f . f

-- TODO swap the details into isDigit?
isNonDigit :: Word8 -> Bool
isNonDigit w =
     w >= 0x00 && w <= 0x2F
  || w >= 0x3A && w <= 0xFF

isDigit :: Word8 -> Bool
isDigit = not . isNonDigit

digitValue :: (Integral a) => Word8 -> Maybe a
digitValue w
  | w >= 0x30 && w <= 0x39
    = Just $ fromIntegral $ w - 0x30
  | otherwise
    = Nothing

digitsValue :: (Integral a) => [Word8] -> Maybe a
digitsValue = f . reverse . map digitValue
  where
    f []            = Just 0
    f (Nothing : _) = Nothing
    f (Just d : ds) = fmap ((+ d) . (* 10)) $ f ds

