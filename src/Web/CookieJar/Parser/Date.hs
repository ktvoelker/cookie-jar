
module Web.CookieJar.Parser.Date where

import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI

import Control.Monad
import Data.Maybe
import Data.Time

import Web.CookieJar.Types
import Web.CookieJar.Parser.Util

isDateDelim :: Word8 -> Bool
isDateDelim w =
     w == 0x09
  || w >= 0x20 && w <= 0x2F
  || w >= 0x3B && w <= 0x40
  || w >= 0x5B && w <= 0x60
  || w >= 0x7B && w <= 0x7E

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

parseTimeToken :: Bytes -> Maybe (Int, Int, Int)
parseTimeToken bs = do
  [hour, min, secPlus] <- return $ map BS.unpack $ take 3 $ BS.split colon bs
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

months :: [(CI.CI Bytes, Int)]
months =
  flip zip [1 ..]
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

parseMonthToken :: Bytes -> Maybe Int
parseMonthToken = flip lookup months . CI.mk . BS.take 3

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
  -- A left fold is needed because the same field can look like both
  -- a day-of-the-month and a year, and RFC 6265 requires that the leftmost
  -- token which can be parsed as a day-of-the-month is used as the day of
  -- the month.
  foldl (flip parseDateToken) (DateFields Nothing Nothing Nothing Nothing)
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
        && year' >= 1601
        && hour <= 23
        && min <= 59
        && sec <= 59
      day <- fromGregorianValid (fromIntegral year') month dom
      let tod = secondsToDiffTime . fromIntegral $ sec + min * 60 + hour * 3600
      return $ UTCTime day tod
  _ -> Nothing

