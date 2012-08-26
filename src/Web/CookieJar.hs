
module Web.CookieJar
  ( Jar()
  , getCookies
  ) where

import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

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

data SetCookiePath = DefaultPath | Path Bytes deriving (Show)

data SetCookie =
  SetCookie
  { scName     :: Bytes
  , scValue    :: Bytes
  , scDomain   :: Maybe Bytes
  , scPath     :: Maybe SetCookiePath
  , scSecure   :: Bool
  , scHttpOnly :: Bool
  , scExpires  :: Maybe Time
  , scMaxAge   :: Maybe Integer
  } deriving (Show)

emptySetCookie :: Bytes -> Bytes -> SetCookie
emptySetCookie n v = SetCookie n v Nothing Nothing False False Nothing Nothing

semicolon :: Word8
semicolon = 0x3B

equals :: Word8
equals = 0x3D

space :: Word8
space = 0x20

hTab :: Word8
hTab = 0x09

isWhitespace :: Word8 -> Bool
isWhitespace w = w == space || w == hTab

trim :: Bytes -> Bytes
trim = let f = BS.reverse . BS.dropWhile isWhitespace in f . f

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

slash :: Word8
slash = 0x2F

parsePath :: Bytes -> Maybe Attribute
parsePath bs
  | bs == BS.empty = f DefaultPath
  | BS.head bs /= slash = f DefaultPath
  | otherwise = f $ Path bs
  where
    f p = Just $ \sc -> sc { scPath = Just p }

period :: Word8
period = 0x2E

parseDomain :: Bytes -> Maybe Attribute
parseDomain bs
  | bs == BS.empty = Just id
  | Just (0x2E, ds) <- BS.uncons bs = f ds
  | otherwise = f bs
  where
    f bs = Just $ \sc -> sc { scDomain = Just $ bytesToLower bs }

parseExpires :: Bytes -> Maybe Attribute
parseExpires =
  maybe (Just id) (\d -> Just $ \sc -> sc { scExpires = Just d })
  . parseDate

negative :: Word8
negative = 0x2D

parseMaxAge :: Bytes -> Maybe Attribute
parseMaxAge bs
  | bs == BS.empty = Just id
  | Just (0x2D, ds) <- BS.uncons bs = f ds
  | otherwise = f bs
  where
    f ds = Just $ \sc -> sc { scMaxAge = digitsValue $ BS.unpack ds }

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

bytesToLower :: Bytes -> Bytes
bytesToLower = BS.map $ \w -> if w >= 0x41 && w <= 0x5A then w + 0x20 else w

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

digitValue :: (Integral a) => Word8 -> Maybe a
digitValue w
  | w >= 0x30 && w <= 0x39
    = Just $ fromIntegral $ w - 0x30
  | otherwise
    = Nothing

digitsValue :: (Integral a) => [Word8] -> Maybe a
digitsValue = f . reverse . map digitValue
  where
    f (Nothing : _) = Nothing
    f (Just d : ds) = fmap ((+ d) . (* 10)) $ f ds

parseTimeToken :: Bytes -> Maybe (Int, Int, Int)
parseTimeToken bs = do
  let [hour, min, secPlus] = map BS.unpack $ take 3 $ BS.split colon bs
  let sec = takeWhile isDigit secPlus
  let fs = [hour, min, sec]
  guard $ all (lengthRange 1 2) fs
  let [Just iHour, Just iMin, Just iSec] = map digitsValue fs
  return (iHour, iMin, iSec)

lengthRange :: Int -> Int -> [a] -> Bool
lengthRange lo hi xs = let n = length xs in lo <= n && n <= hi

parseDayOfMonthToken :: Bytes -> Maybe Int
parseDayOfMonthToken = parseIntToken 1 2

parseYearToken :: Bytes -> Maybe Int
parseYearToken = parseIntToken 2 4

parseIntToken :: Int -> Int -> Bytes -> Maybe Int
parseIntToken lo hi bs = do
  let ds = takeWhile isDigit $ BS.unpack bs
  guard $ lengthRange lo hi ds
  digitsValue ds

months :: [(Bytes, Int)]
months =
  flip zip [1 ..]
  $ map E.encodeUtf8
  $ [ "jan"
    , "feb"
    , "mar"
    , "apr"
    , "may"
    , "jun"
    , "jul"
    , "aug"
    , "sep"
    , "oct"
    , "nov"
    , "dec"
    ]

-- TODO case-insensitive?
parseMonthToken :: Bytes -> Maybe Int
parseMonthToken = flip lookup months . BS.take 3

parseDateToken :: Bytes -> DateFields -> DateFields
parseDateToken bs df
  | dfTime df == Nothing && isJust timeToken
    = df { dfTime = timeToken }
  | dfDayOfMonth df == Nothing && isJust domToken
    = df { dfDayOfMonth = domToken }
  | dfMonth df == Nothing && isJust monthToken
    = df { dfMonth = monthToken }
  | dfYear df == Nothing && isJust yearToken
    = df { dfYear = yearToken }
  | otherwise
    = df
  where
    timeToken  = parseTimeToken bs
    domToken   = parseDayOfMonthToken bs
    monthToken = parseMonthToken bs
    yearToken  = parseYearToken bs

parseDateFields :: Bytes -> DateFields
parseDateFields =
  foldr parseDateToken (DateFields Nothing Nothing Nothing Nothing)
  . tokenizeDate

parseDate :: Bytes -> Maybe Time
parseDate bs = case parseDateFields bs of
  DateFields
    { dfTime = Just (hour, min, sec)
    , dfDayOfMonth = Just dom
    , dfMonth = Just month
    , dfYear = Just year
    } -> do
      let
      { year'
        = if year >= 0 && year <= 69
          then year + 2000
          else
            if year >= 70 && year <= 99
            then year + 1900
            else year
      }
      guard
        $  dom >= 1
        && dom <= 31
        && year' < 1601
        && hour <= 23
        && min <= 59
        && sec <= 59
      day <- fromGregorianValid (fromIntegral year') month dom
      let tod = secondsToDiffTime . fromIntegral $ sec + min * 60 + hour * 3600
      return $ UTCTime day tod
  _ -> Nothing

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

