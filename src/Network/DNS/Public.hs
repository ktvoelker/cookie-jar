
module Network.DNS.Public
  ( Rules()
  , parseRules
  , splitDomain
  , isPublicDomain
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IDN.IDNA as IDNA

import Data.Char
import Data.List
import Data.List.Split
import Data.Word (Word8)

data Label = AnyLabel | Label BS.ByteString | Error String deriving (Show)

data Rule = Rule Bool [Label] deriving (Show)

newtype Rules = Rules { getRules :: [Rule] } deriving (Show)

period :: Word8
period = 46

star :: Word8
star = 42

toLabels :: String -> [Label]
toLabels = textToLabels . T.pack

textToLabels :: T.Text -> [Label]
textToLabels =
  either ((: []) . Error . show) bytesToLabels
  . IDNA.toASCII IDNA.defaultFlags

bytesToLabels :: BS.ByteString -> [Label]
bytesToLabels =
  reverse
  . map f
  . BS.split period
  . BS.dropWhile (== period)
  where
    f (BS.uncons -> Just (x, _)) | x == star = AnyLabel
    f xs = Label . BS.map byteToLower $ xs

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

orderRules :: Rule -> Rule -> Ordering
orderRules (Rule x1 es1) (Rule x2 es2) = case compare x1 x2 of
  EQ -> compare (length es2) (length es1)
  o -> o

defaultRule :: Rule
defaultRule = Rule True [AnyLabel]

parseRules :: String -> Rules
parseRules =
  Rules
  . sortBy orderRules
  . (defaultRule :)
  . map f
  . filter (not . ("//" `isPrefixOf`))
  . filter (not . null)
  . map (takeWhile $ not . isSpace)
  . lines
  where
    f ('!' : es) = Rule False $ toLabels es
    f es = Rule True $ toLabels es

isPublicDomain :: Rules -> BS.ByteString -> Bool
isPublicDomain rs ds = BS.null $ fst $ splitDomain rs ds

input :: BS.ByteString -> [BS.ByteString]
input = reverse . BS.split period . BS.map byteToLower

output :: [BS.ByteString] -> BS.ByteString
output (x : xs) = BS.concat $ x : concatMap ((BS.pack [period] :) . (: [])) xs

splitDomain :: Rules -> BS.ByteString -> (BS.ByteString, BS.ByteString)
splitDomain (getRules -> rs) (input -> ds) =
  g $ case head $ filter (\(Rule _ es) -> match ds es) rs of
    Rule True es -> f (length es) ds
    Rule False es -> f (length es - 1) ds
  where
    f = splitAt . (length ds -)
    g (a, b) = (output a, output b)

match :: [BS.ByteString] -> [Label] -> Bool
match [] _ = True
match (d : ds) (Label r : rs)
  | d == r = match ds rs
  | d /= r = False
match (_ : ds) (AnyLabel : rs) = match ds rs

