
module Web.CookieJar
  ( Jar()
  , getCookies
  ) where

import qualified Data.ByteString as BS

import Control.Monad
import Data.Maybe
import Data.Time
import Data.Word (Word8)
import Network.HTTP.Types

type Time = UTCTime

type Bytes = BS.ByteString

-- TODO
type Cookie = ()

newtype Jar = Jar { getCookies :: [Cookie] }

data Endpoint =
  Endpoint
  { epDomain :: Bytes
  , epPath   :: Bytes
  } deriving (Show)

data SetCookie =
  SetCookie
  { scName     :: Bytes
  , scValue    :: Bytes
  , scDomain   :: Maybe Bytes
  , scPath     :: Maybe Bytes
  , scSecure   :: Maybe Bool
  , scHttpOnly :: Maybe Bool
  , scExpires  :: Maybe Time
  , scMaxAge   :: Maybe Integer
  }

parseSetCookie :: Bytes -> Maybe SetCookie
parseSetCookie bs = undefined

isDateDelim :: Word8 -> Bool
isDateDelim w =
     w == 0x09
  || w >= 0x20 && w <= 0x2F
  || w >= 0x3B && w <= 0x40
  || w >= 0x5B && w <= 0x60
  || w >= 0x7B && w <= 0x7E

colon :: Word8
colon = 0x3A

-- TODO could this be defined as not . isDateDelim?
isDateNonDelim :: Word8 -> Bool
isDateNonDelim w =
     w >= 0x00 && w <= 0x08
  || w >= 0x0A && w <= 0x1F
  || w >= 0x30 && w <= 0x39 -- DIGIT
  || w == colon
  || w >= 0x41 && w <= 0x5A -- uppercase ALPHA
  || w >= 0x61 && w <= 0x7A -- lowercase ALPHA
  || w >= 0x7F && w <= 0xFF

-- TODO swap the details into isDigit?
isNonDigit :: Word8 -> Bool
isNonDigit w =
     w >= 0x00 && w <= 0x2F
  || w >= 0x3A && w <= 0xFF

isDigit :: Word8 -> Bool
isDigit = not . isNonDigit

tokenizeDate :: Bytes -> [Bytes]
tokenizeDate (BS.dropWhile isDateDelim -> bs)
  | bs == BS.empty
    = []
  | otherwise
    = let (tok, bs') = BS.span isDateNonDelim bs in tok : tokenizeDate bs'

data DateFields =
  DateFields
  { dfTime       :: Maybe (Int, Int, Int)
  , dfDayOfMonth :: Maybe Int
  , dfMonth      :: Maybe Int
  , dfYear       :: Maybe Int
  } deriving (Show)

digitValue :: Word8 -> Maybe Int
digitValue w
  | w >= 0x30 && w <= 0x39
    = Just $ fromIntegral $ w - 0x30
  | otherwise
    = Nothing

digitsValue :: [Word8] -> Maybe Int
digitsValue = f . reverse . map digitValue
  where
    f (Nothing : _) = Nothing
    f (Just d : ds) = fmap ((+ d) . (* 10)) $ f ds

parseTimeToken :: Bytes -> Maybe (Int, Int, Int)
parseTimeToken bs = do
  let [hour, min, secPlus] = map BS.unpack $ take 3 $ BS.split colon bs
  let sec = takeWhile isDigit secPlus
  let fs = [hour, min, sec]
  guard $ all ((>= 1) . length) fs
  guard $ all ((<= 2) . length) fs
  let [Just iHour, Just iMin, Just iSec] = map digitsValue fs
  return (iHour, iMin, iSec)

parseDateToken :: Bytes -> DateFields -> DateFields
parseDateToken bs df = undefined

parseDate :: Bytes -> DateFields
parseDate =
  foldr parseDateToken (DateFields Nothing Nothing Nothing Nothing)
  . tokenizeDate

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

