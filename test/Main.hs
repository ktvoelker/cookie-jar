
module Main where

import Control.Monad
import Control.Monad.State
import System.Exit
import Test.HUnit

import qualified Network.DNS.Public as P
import Web.CookieJar

import Public

type Session a = StateT Jar IO a

runSession :: P.Rules -> Session a -> IO a
runSession = flip evalStateT . emptyJar . Just

sessionTest :: String -> Session () -> P.Rules -> Test
sessionTest n s r = TestLabel n $ TestCase $ runSession r s

emptyTest = sessionTest "Empty test" $ return ()

sessionTests rules = map ($ rules)
  [ emptyTest
  ]

main = do
  rulesString <- readFile "data/effective_tld_names.dat"
  let rules = P.parseRules rulesString
  c <- runTestTT $ TestList $ concatMap ($ rules) [publicSuffixTests, sessionTests]
  when (errors c /= 0 || failures c /= 0) exitFailure

