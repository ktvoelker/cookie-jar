
module Network.DNS.Public.Types where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IDN.IDNA as IDNA

import Control.Monad
import Data.Word (Word8)

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
  --unify _ Nothing = True
  --unify a (Just b) = a == b

instance Unify BS.ByteString BS.ByteString where
  unify a b = a == b

newtype Domain (a :: Bool) = Domain { getLabels :: [Label a] }

period :: Word8
period = 46

star :: BS.ByteString
star = BS.pack [42]

split :: BS.ByteString -> [BS.ByteString]
split = dropWhile BS.null . BS.split period

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
fromText t = case IDNA.toASCII IDNA.defaultFlags t of
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

isSuffixOf :: (Unify (Label a) (Label b)) => Domain a -> Domain b -> Bool
isSuffixOf (Domain as) (Domain bs) = f as bs
  where
    f [] _ = True
    f _ [] = False
    f (a : as) (b : bs)
      | unify a b = f as bs
      | otherwise = False

