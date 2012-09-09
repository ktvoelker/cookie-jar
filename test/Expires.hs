
module Expires (expiresTests) where

import Util

-- 5.1.1.
timeParseTest valid n xs = sessionTest ("Time parsing: " ++ xs) $ do
  recv (time $ n - 1) e $ cs ++ "; Expires=" ++ xs
  send (time $ n - 1) e cs
  if valid
    then noSend (time n) e
    -- An invalid date doesn't invalidate the whole-cookie, so it becomes
    -- a non-persistent cookie with no particular expiration.
    else send (time n) e cs >> send (time $ n + year) e cs
  where
    cs = "x=y"
    e = ep host1 path1 True True

expiresTests =
  -- strange field-orderings
  [ timeParseTest True 1 "01 70 0:0:1 Janbearpig"
  , timeParseTest True 1 "1970 1 00:00:01 jan"
  , timeParseTest True 1 "01 0:00:1 JANU 1970"
  -- valid delimiters
  , timeParseTest True 1 "1970 Jan\t01!00:00:01"
  , timeParseTest True 1 "1970\"Jan#01$00:00:01"
  , timeParseTest True 1 "1970%Jan&01'00:00:01"
  , timeParseTest True 1 "1970(Jan)01*00:00:01"
  , timeParseTest True 1 "1970+Jan,01-00:00:01"
  , timeParseTest True 1 "1970.Jan/01.00:00:01"
  , timeParseTest True 1 " !\"#$%&'()*+,-./1970 Jan 01 00:00:01 !\"#$%&'()*+,-./"
  -- years
  , timeParseTest True year "1971 Jan 01 00:00:00"
  , timeParseTest True (2 * year) "1972 Jan 01 00:00:00"
  -- months
  , timeParseTest True (31 * day) "1970 Feb 01 00:00:00"
  , timeParseTest True ((31 + 28) * day) "1970 Mar 01 00:00:00"
  , timeParseTest True (year - 31 * day) "1970 Dec 01 00:00:00"
  -- days of the month
  , timeParseTest True day "1970 Jan 02 00:00:00"
  , timeParseTest True (2 * day) "1970 Jan 03 00:00:00"
  -- hours
  , timeParseTest True hour "1970 Jan 01 01:00:00"
  , timeParseTest True (2 * hour) "1970 Jan 01 02:00:00"
  , timeParseTest True (day - hour) "1970 Jan 01 23:00:00"
  -- minutes
  , timeParseTest True minute "1970 Jan 01 00:01:00"
  , timeParseTest True (2 * minute) "1970 Jan 01 00:02:00"
  , timeParseTest True (hour - minute) "1970 Jan 01 00:59:00"
  -- seconds
  , timeParseTest True second "1970 Jan 01 00:00:01"
  , timeParseTest True (2 * second) "1970 Jan 01 00:00:02"
  , timeParseTest True (minute - second) "1970 Jan 01 00:00:59"
  -- recent years
  , timeParseTest True year2000 "Jan 01, 00 00:00:00"
  , timeParseTest True (year2000 - second) "Dec 31, 99 23:59:59"
  , timeParseTest True (year2000 + second) "Jan 01, 00 00:00:01"
  -- leap years
  , timeParseTest True (year2000 + year + day) "Jan 01, 01 00:00:00"
  , timeParseTest True (year2000 - 3 * year) "Jan 01, 97 00:00:00"
  , timeParseTest True (year2000 - 4 * year - day) "Jan 01, 96 00:00:00"
  -- invalid times
  , timeParseTest False 0 "Ja 01, 1970 00:00:00"
  , timeParseTest False 0 "Ajan 01, 1970 00:00:00"
  , timeParseTest False 0 "Jan 001, 1970 00:00:00"
  , timeParseTest False 0 "70 Jan 01, 00:00:00"
  , timeParseTest False 0 "01970 Jan 01, 00:00:00"
  , timeParseTest False (-200 * year) "1600 Jan 01, 00:00:00"
  , timeParseTest False day "1970 Jan 01, 24:00:00"
  , timeParseTest False 0 "1970 Jan 00, 00:00:00"
  , timeParseTest False (32 * day) "1970 Jan 32, 00:00:00"
  , timeParseTest False hour "1970 Jan 01, 00:60:00"
  , timeParseTest False minute "1970 Jan 01, 00:00:60"
  , timeParseTest False ((31 + 29) * day) "1970 Feb 29, 00:00:00"
  , timeParseTest False ((31 + 30) * day) "1970 Feb 30, 00:00:00"
  ]

