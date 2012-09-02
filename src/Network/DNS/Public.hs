
module Network.DNS.Public
  ( Rules()
  , parseRules
  , isPublicDomain
  , publicSuffix
  , module Network.DNS.Public.Types
  ) where

import Data.Char
import Data.Functor
import Data.List (isPrefixOf, sortBy)
import Data.Maybe

import Network.DNS.Public.Types

data Rule = Rule { positive :: Bool, pattern :: Domain True } deriving (Show)

newtype Rules = Rules { getRules :: [Rule] } deriving (Show)

orderRules :: Rule -> Rule -> Ordering
orderRules (Rule x1 d1) (Rule x2 d2) = case compare x1 x2 of
  EQ -> compare (countLabels d2) (countLabels d1)
  o -> o

defaultRule :: Rule
defaultRule = Rule True . fromJust $ makeStringPattern "*"

parseRules :: String -> Rules
parseRules =
  Rules
  . sortBy orderRules
  . (defaultRule :)
  . catMaybes
  . map f
  . filter (not . ("//" `isPrefixOf`))
  . filter (not . null)
  . map (takeWhile $ not . isSpace)
  . lines
  where
    f ('!' : ds) = Rule False <$> makeStringPattern ds
    f ds = Rule True <$> makeStringPattern ds

isPublicDomain :: Rules -> Domain False -> Bool
isPublicDomain rs d = publicSuffix rs d == d

publicSuffix :: Rules -> Domain False -> Domain False
publicSuffix (Rules rs) d = dropSubdomains (countLabels d - publicLabels) d
  where
    countPublicLabels (Rule False d) = countLabels d - 1
    countPublicLabels (Rule True d) = countLabels d
    publicLabels =
      countPublicLabels
      . head
      . filter ((`isSuffixOf` d) . pattern)
      $ rs

