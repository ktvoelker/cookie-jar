
-- |This module provides the ability to determine the \"public suffix\" of
-- a domain name, which is the longest suffix under which other domains can
-- be registered by the public.
--
-- See <http://publicsuffix.org/> for more information.
--
-- This module includes support for Unicode domain names as specified by
-- RFC 5890, \"Internationalized Domain Names for Applications\"
-- (<http://www.rfc-editor.org/rfc/rfc5890.txt>).
module Network.DNS.Public (
  -- * Types
    Rules()
  , Domain()
  , makeDomain
  , makeTextDomain
  , makeStringDomain
  -- * Algorithms
  , parseRules
  , isPublicDomain
  , publicSuffix
  , showDomain
  ) where

import Data.Char
import Data.Functor
import Data.List (isPrefixOf, sortBy)
import Data.Maybe

import Network.DNS.Public.Types

data Rule = Rule { positive :: Bool, pattern :: Pattern } deriving (Eq, Show)

-- |Rules for determining the public suffix of a domain
--
-- A concrete set of rules can be obtained from
-- <http://publicsuffix.org/list/> in a format understood by "parseRules".
newtype Rules = Rules { getRules :: [Rule] } deriving (Eq, Show)

orderRules :: Rule -> Rule -> Ordering
orderRules (Rule x1 d1) (Rule x2 d2) = case compare x1 x2 of
  EQ -> compare (len d2) (len d1)
  o -> o
  where
    len = length . getPattern

defaultRule :: Rule
defaultRule = Rule True . fromJust $ makeStringPattern "*"

-- |Parse a rule-set in the format described by
-- <http://publicsuffix.org/list/>
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

-- |True iff subdomains can be registered by the public under the given
-- domain
isPublicDomain :: Rules -> Domain -> Bool
isPublicDomain rs d = publicSuffix rs d == d

-- |Determine the public suffix of a domain
publicSuffix :: Rules -> Domain -> Domain
publicSuffix (Rules rs) d = dropSubdomains ((length . getLabels) d - publicLabels) d
  where
    countPublicLabels (Rule False d) = (length . getPattern) d - 1
    countPublicLabels (Rule True d) = (length . getPattern) d
    publicLabels =
      countPublicLabels
      . head
      . filter ((`matches` d) . pattern)
      $ rs

