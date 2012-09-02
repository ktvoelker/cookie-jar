
module Network.DNS.Public.Types
  ( Domain()
  , makeDomain
  , makePattern
  , makeTextDomain
  , makeTextPattern
  , makeStringDomain
  , makeStringPattern
  , showDomain
  , isSuffixOf
  , countLabels
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

type family Label (a :: Bool) :: *

type instance Label True = Maybe BS.ByteString

type instance Label False = BS.ByteString

class Unify a b where
  unify :: a -> b -> Bool

instance Unify (Maybe BS.ByteString) BS.ByteString where
  unify Nothing _ = True
  unify (Just a) b = a == b

instance Unify BS.ByteString (Maybe BS.ByteString) where
  unify a b = unify b a

instance Unify BS.ByteString BS.ByteString where
  unify a b = a == b

newtype Domain (a :: Bool) = Domain { getLabels :: [Label a] }

instance Eq (Domain False) where
  (Domain as) == (Domain bs) = as == bs

deriving instance Show (Domain True)

deriving instance Show (Domain False)

period :: Word8
period = 46

star :: BS.ByteString
star = BS.pack [42]

split :: BS.ByteString -> [BS.ByteString]
split = reverse . BS.split period . BS.map byteToLower . BS.dropWhile (== period)

makeDomain :: BS.ByteString -> Maybe (Domain False)
makeDomain bs = case split bs of
  [] -> Nothing
  xs -> Just $ Domain xs

makePattern :: BS.ByteString -> Maybe (Domain True)
makePattern bs = case split bs of
  [] -> Nothing
  xs -> Just $ Domain $ map f xs
  where
    f x
      | x == star = Nothing
      | otherwise = Just x

fromText :: T.Text -> Maybe BS.ByteString
fromText t = case IDNA.toASCII IDNA.defaultFlags $ T.dropWhile (== '.') t of
  Left _ -> Nothing
  Right bs -> Just bs

makeTextDomain :: T.Text -> Maybe (Domain False)
makeTextDomain = makeDomain <=< fromText

makeTextPattern :: T.Text -> Maybe (Domain True)
makeTextPattern = makePattern <=< fromText

fromString :: String -> Maybe BS.ByteString
fromString = fromText . T.pack

makeStringDomain :: String -> Maybe (Domain False)
makeStringDomain = makeDomain <=< fromString

makeStringPattern :: String -> Maybe (Domain True)
makeStringPattern = makePattern <=< fromString

isSuffixOf :: (Unify (Label a) BS.ByteString) => Domain a -> Domain False -> Bool
isSuffixOf (Domain as) (Domain bs) = f as bs
  where
    f [] _ = True
    f _ [] = False
    f (a : as) (b : bs)
      | unify a b = f as bs
      | otherwise = False

countLabels :: Domain a -> Int
countLabels = length . getLabels

showDomain :: Domain False -> BS.ByteString
showDomain (Domain xs) = BS.drop 1 . BS.concat . map (BS.cons period) . reverse $ xs

dropSubdomains :: Int -> Domain a -> Domain a
dropSubdomains n (Domain as) = Domain $ take (length as - n) as

