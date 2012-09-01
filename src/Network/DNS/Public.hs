
module Network.DNS.Public where

import Data.Char
import Data.List
import Data.List.Split
import Data.String

data Label a = AnyLabel | Label a deriving (Show)

data Rule a =
  Rule
  { public :: [Label a]
  , except :: [[Label a]]
  } deriving (Show)

toLabels :: (IsString a) => String -> [Label a]
toLabels = reverse . map f . split (dropInitBlank $ dropDelims $ oneOf ".")
  where
    f "*" = AnyLabel
    f xs = Label $ fromString xs

parseRules :: (IsString a) => String -> [Rule a]
parseRules =
  map f
  . split (keepDelimsL $ whenElt $ not . ("!" `isPrefixOf`))
  . filter (not . ("//" `isPrefixOf`))
  . filter (not . null)
  . map (takeWhile $ not . isSpace)
  . lines
  where
    f [] = undefined
    f (p : es) = Rule { public = toLabels p, except = map (toLabels . drop 1) es }

isPublic :: (Eq a) => [Rule a] -> [a] -> Bool
isPublic rs ds = any (match $ reverse ds) rs

match :: (Eq a) => [a] -> Rule a -> Bool
match ds Rule{..} = matchLabel ds public && not (any (matchLabel ds) except)

matchLabel :: (Eq a) => [a] -> [Label a] -> Bool
matchLabel [] _ = True
matchLabel (d : ds) (Label r : rs)
  | d == r = matchLabel ds rs
  | d /= r = False
matchLabel (_ : ds) (AnyLabel : rs) = matchLabel ds rs

