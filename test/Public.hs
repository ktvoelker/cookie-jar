
module Public (publicSuffixTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Control.Monad
import Data.Functor
import Data.String
import System.Environment
import Test.HUnit

import Network.DNS.Public
import Network.DNS.Public.Types (dropSubdomains)

checkPublicSuffix :: Rules -> String -> String -> Test
checkPublicSuffix rules domain suffix =
  TestLabel label $ TestCase $ assertBool (show $ showDomain <$> suffix'') pass
  where
    label = "(" ++ domain ++ "," ++ suffix ++ ")"
    domain' = makeStringDomain domain
    suffix' =
      if null suffix
      then domain'
      else dropSubdomains 1 <$> makeStringDomain suffix
    suffix'' = publicSuffix rules <$> domain'
    pass = suffix' == suffix''

publicSuffixTests :: Rules -> [Test]
publicSuffixTests r =
  [ checkPublicSuffix r "COM" ""
  , checkPublicSuffix r "example.COM" "example.com"
  , checkPublicSuffix r "WwW.example.COM" "example.com"
  , checkPublicSuffix r ".com" ""
  , checkPublicSuffix r ".example" ""
  , checkPublicSuffix r ".example.com" "example.com"
  , checkPublicSuffix r ".example.example" "example.example"
  , checkPublicSuffix r "example" ""
  , checkPublicSuffix r "example.example" "example.example"
  , checkPublicSuffix r "b.example.example" "example.example"
  , checkPublicSuffix r "a.b.example.example" "example.example"
  , checkPublicSuffix r "biz" ""
  , checkPublicSuffix r "domain.biz" "domain.biz"
  , checkPublicSuffix r "b.domain.biz" "domain.biz"
  , checkPublicSuffix r "a.b.domain.biz" "domain.biz"
  , checkPublicSuffix r "com" ""
  , checkPublicSuffix r "example.com" "example.com"
  , checkPublicSuffix r "b.example.com" "example.com"
  , checkPublicSuffix r "a.b.example.com" "example.com"
  , checkPublicSuffix r "uk.com" ""
  , checkPublicSuffix r "example.uk.com" "example.uk.com"
  , checkPublicSuffix r "b.example.uk.com" "example.uk.com"
  , checkPublicSuffix r "a.b.example.uk.com" "example.uk.com"
  , checkPublicSuffix r "test.ac" "test.ac"
  , checkPublicSuffix r "cy" ""
  , checkPublicSuffix r "c.cy" ""
  , checkPublicSuffix r "b.c.cy" "b.c.cy"
  , checkPublicSuffix r "a.b.c.cy" "b.c.cy"
  , checkPublicSuffix r "jp" ""
  , checkPublicSuffix r "test.jp" "test.jp"
  , checkPublicSuffix r "www.test.jp" "test.jp"
  , checkPublicSuffix r "ac.jp" ""
  , checkPublicSuffix r "test.ac.jp" "test.ac.jp"
  , checkPublicSuffix r "www.test.ac.jp" "test.ac.jp"
  , checkPublicSuffix r "kyoto.jp" ""
  , checkPublicSuffix r "c.kyoto.jp" "c.kyoto.jp"
  , checkPublicSuffix r "b.c.kyoto.jp" "c.kyoto.jp"
  , checkPublicSuffix r "a.b.c.kyoto.jp" "c.kyoto.jp"
  , checkPublicSuffix r "pref.kyoto.jp" "pref.kyoto.jp"
  , checkPublicSuffix r "www.pref.kyoto.jp" "pref.kyoto.jp"
  , checkPublicSuffix r "city.kyoto.jp" "city.kyoto.jp"
  , checkPublicSuffix r "www.city.kyoto.jp" "city.kyoto.jp"
  , checkPublicSuffix r "om" ""
  , checkPublicSuffix r "test.om" ""
  , checkPublicSuffix r "b.test.om" "b.test.om"
  , checkPublicSuffix r "a.b.test.om" "b.test.om"
  , checkPublicSuffix r "songfest.om" "songfest.om"
  , checkPublicSuffix r "www.songfest.om" "songfest.om"
  , checkPublicSuffix r "us" ""
  , checkPublicSuffix r "test.us" "test.us"
  , checkPublicSuffix r "www.test.us" "test.us"
  , checkPublicSuffix r "ak.us" ""
  , checkPublicSuffix r "test.ak.us" "test.ak.us"
  , checkPublicSuffix r "www.test.ak.us" "test.ak.us"
  , checkPublicSuffix r "k12.ak.us" ""
  , checkPublicSuffix r "test.k12.ak.us" "test.k12.ak.us"
  , checkPublicSuffix r "www.test.k12.ak.us" "test.k12.ak.us"
  ]

