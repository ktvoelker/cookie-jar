
module Network.DNS.Public.Types
  ( Domain(..)
  , Pattern(..)
  , makeDomain
  , makePattern
  , makeTextDomain
  , makeTextPattern
  , makeStringDomain
  , makeStringPattern
  , showDomain
  , isSuffixOf
  , matches
  , dropSubdomains
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IDN.IDNA as IDNA

import Control.Monad
import Data.Word (Word8)

capitalA :: Word8
capitalA = 65

capitalZ :: Word8
capitalZ = 90

lowercaseA :: Word8
lowercaseA = 97

lowerDiff :: Word8
lowerDiff = lowercaseA - capitalA

byteToLower :: Word8 -> Word8
byteToLower n = if n >= capitalA && n <= capitalZ then n + lowerDiff else n

-- |A domain name, which is a sequence of domain labels
newtype Domain = Domain { getLabels :: [BS.ByteString] } deriving (Eq, Show)

-- |A domain pattern, which is like a domain name, except that the sequence
-- may also contain wildcards
newtype Pattern = Pattern { getPattern :: [Maybe BS.ByteString] } deriving (Eq, Show)

period :: Word8
period = 46

star :: BS.ByteString
star = BS.pack [42]

split :: BS.ByteString -> [BS.ByteString]
split = reverse . BS.split period . BS.map byteToLower . BS.dropWhile (== period)

-- |Parse a domain name
makeDomain :: BS.ByteString -> Maybe (Domain)
makeDomain bs = case split bs of
  [] -> Nothing
  xs -> Just $ Domain xs

-- |Parse a domain pattern, where asterisks are wildcards
makePattern :: BS.ByteString -> Maybe (Pattern)
makePattern bs = case split bs of
  [] -> Nothing
  xs -> Just $ Pattern $ map f xs
  where
    f x
      | x == star = Nothing
      | otherwise = Just x

fromText :: T.Text -> Maybe BS.ByteString
fromText t = case IDNA.toASCII IDNA.defaultFlags $ T.dropWhile (== '.') t of
  Left _ -> Nothing
  Right bs -> Just bs

-- |Parse a Unicode domain name
makeTextDomain :: T.Text -> Maybe (Domain)
makeTextDomain = makeDomain <=< fromText

-- |Parse a Unicode domain pattern
makeTextPattern :: T.Text -> Maybe (Pattern)
makeTextPattern = makePattern <=< fromText

fromString :: String -> Maybe BS.ByteString
fromString = fromText . T.pack

-- |Parse a Unicode domain name
makeStringDomain :: String -> Maybe (Domain)
makeStringDomain = makeDomain <=< fromString

-- |Parse a Unicode domain pattern
makeStringPattern :: String -> Maybe (Pattern)
makeStringPattern = makePattern <=< fromString

matchesSuffixOf :: (a -> b -> Bool) -> [a] -> [b] -> Bool
matchesSuffixOf p = f
  where
    f [] _ = True
    f _ [] = False
    f (a : as) (b : bs)
      | p a b = f as bs
      | otherwise = False

isSuffixOf :: Domain -> Domain -> Bool
isSuffixOf as bs =
  let f = matchesSuffixOf (==)
  in getLabels as `f` getLabels bs

matches :: Pattern -> Domain -> Bool
matches as bs =
  let f = matchesSuffixOf (flip $ maybe True . (==))
  in getPattern as `f` getLabels bs

-- |Show the ASCII form of a domain name
showDomain :: Domain -> BS.ByteString
showDomain (Domain xs) = BS.drop 1 . BS.concat . map (BS.cons period) . reverse $ xs

-- |Drop a number of the earliest labels or wildcards in a domain
dropSubdomains :: Int -> Domain -> Domain
dropSubdomains n (Domain as) = Domain $ take (length as - n) as

