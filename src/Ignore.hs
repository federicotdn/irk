module Ignore
  ( Ignore (..),
    Pattern (..),
    Segment (..),
    parse,
    ignores,
    ignores',
  )
where

import Data.List (tails)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Encoding (utf16le, utf8)
import System.OsPath (OsPath, encodeWith, splitDirectories)
import qualified System.OsString as OS

-- TODO: Add a Glob constructor to match more complex segments
-- such as "abc?d", "foo*bar", "a[a-zA-Z]b", etc.
data Segment
  = DAsterisk
  | Asterisk
  | Const OsPath
  | Prefix OsPath
  | Suffix OsPath
  deriving (Show, Eq)

data Pattern = Pattern
  { pSegments :: [Segment],
    pDir :: Bool,
    pNegated :: Bool,
    pAnchored :: Bool
  }
  deriving (Show, Eq)

newtype Ignore = Ignore [Pattern] deriving (Show, Eq)

instance Semigroup Ignore where
  (Ignore p1) <> (Ignore p2) = Ignore (p1 <> p2)

parseSegment :: Text -> Maybe Segment
parseSegment source
  | source == "**" = Just DAsterisk
  | source == "*" = Just Asterisk
  | scount == 0 && acount == 1 && aprefix = Suffix . OS.tail <$> encoded
  | scount == 0 && acount == 1 && asuffix = Prefix . OS.init <$> encoded
  | otherwise = Const <$> encoded
  where
    aprefix = "*" `T.isPrefixOf` source
    asuffix = "*" `T.isSuffixOf` source
    acount = T.count "*" source
    scount = T.length (T.filter (`elem` ['[', ']', '?', '\\']) source)
    encoded = either (const Nothing) Just $ encodeWith utf8 utf16le (T.unpack source)

parsePattern :: Text -> Pattern
parsePattern source =
  let (source', negated) =
        if not (T.null source) && T.head source == '!'
          then (T.tail source, True)
          else (source, False)
      segments = filter (not . T.null) $ T.splitOn "/" source'
      segments' = mapMaybe parseSegment segments
      dir = "/" `T.isSuffixOf` source'
      anchored = "/" `T.isPrefixOf` source' || length segments' > 1
   in Pattern
        { pSegments = segments',
          pDir = dir,
          pNegated = negated,
          pAnchored = anchored
        }

parse :: Text -> Ignore
parse source =
  let sourceLines = filter (not . T.null) $ map T.strip $ T.lines source
      patterns = filter (\l -> T.head l /= '#') sourceLines
   in Ignore (map parsePattern patterns)

segmentMatches :: Segment -> OsPath -> Bool
segmentMatches target path = case target of
  Asterisk -> True
  Const val -> path == val
  Suffix val -> val `OS.isSuffixOf` path
  Prefix val -> val `OS.isPrefixOf` path
  -- NOTE: DAsterisk is already handled in 'patternIgnoresInner'.
  DAsterisk -> error "Unhandled ** pattern"

patternIgnoresInner :: Pattern -> [OsPath] -> Maybe Bool
patternIgnoresInner pat@Pattern {pNegated = True} splitPath =
  not <$> patternIgnoresInner pat {pNegated = False} splitPath
patternIgnoresInner pat@Pattern {pSegments = segs} splitPath =
  if null splitPath
    then
      -- Exhausted path without returning False, meaning we might
      -- have an ignore-match. This depends on whether there is more
      -- pattern to match.
      if null segs then Just True else Nothing
    else case segs of
      [] -> Nothing
      (DAsterisk : rest) ->
        if null rest
          then Just True
          else
            let matches = mapMaybe (patternIgnoresInner (pat {pSegments = rest})) (init $ tails splitPath)
             in if null matches then Nothing else Just (or matches)
      (seg : rest) ->
        let tailPath = drop 1 splitPath
            match = segmentMatches seg (head splitPath)
            continued = patternIgnoresInner (pat {pSegments = rest}) tailPath
            retry = if null tailPath then Nothing else patternIgnoresInner pat tailPath
         in if pAnchored pat
              then if match then continued else Nothing
              else (if match && isJust continued then continued else retry)

patternIgnores :: Pattern -> [OsPath] -> Bool -> Maybe Bool
patternIgnores pat splitPath dir =
  if (pDir pat && not dir) || null splitPath
    then Nothing
    else patternIgnoresInner pat splitPath

ignoresInner :: Ignore -> [OsPath] -> Bool -> Bool
ignoresInner (Ignore patterns) splitPath dir =
  last $ False : mapMaybe (\pat -> patternIgnores pat splitPath dir) patterns

ignores :: Ignore -> OsPath -> Bool -> Bool
ignores ign path = ignoresInner ign (splitDirectories path)

ignores' :: Ignore -> [OsPath] -> Bool -> Bool
ignores' = ignoresInner
