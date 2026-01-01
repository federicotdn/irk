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
import System.OsPath (OsPath, splitDirectories, unsafeEncodeUtf)
import qualified System.OsString as OS

data Segment = DAsterisk | Asterisk | Const OsPath | Prefix OsPath | Suffix OsPath | Glob OsPath deriving (Show, Eq)

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

parseSegment :: Text -> Segment
parseSegment source
  | source == "**" = DAsterisk
  | source == "*" = Asterisk
  | acount == 1 && "*" `T.isPrefixOf` source = Suffix (OS.tail encoded)
  | acount == 1 && "*" `T.isSuffixOf` source = Prefix (OS.init encoded)
  | otherwise = Const encoded
  where
    acount = T.count "*" source
    encoded = unsafeEncodeUtf (T.unpack source)

parsePattern :: Text -> Pattern
parsePattern source =
  let (source', negated) =
        if not (T.null source) && T.head source == '!'
          then (T.tail source, True)
          else (source, False)
      segments = filter (not . T.null) $ T.splitOn "/" source'
      segments' = map parseSegment segments
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

-- TODO: Not complete, though it's good enough for most use cases.
segmentMatches :: Segment -> OsPath -> Bool
segmentMatches target path = case target of
  DAsterisk -> error "Unhandled double asterisk pattern"
  Asterisk -> True
  Const val -> path == val
  Prefix val -> val `OS.isPrefixOf` path
  Suffix val -> val `OS.isSuffixOf` path
  Glob _ -> undefined

patternIgnores' :: Pattern -> [OsPath] -> Maybe Bool
patternIgnores' pat@Pattern {pNegated = True} splitPath =
  not <$> patternIgnores' pat {pNegated = False} splitPath
patternIgnores' pat@Pattern {pSegments = segs} splitPath =
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
            let matches = mapMaybe (patternIgnores' (pat {pSegments = rest})) (init $ tails splitPath)
             in if null matches then Nothing else Just (or matches)
      (seg : rest) ->
        let tailPath = tail splitPath
            match = segmentMatches seg (head splitPath)
            continued = patternIgnores' (pat {pSegments = rest}) tailPath
            retry = if null tailPath then Nothing else patternIgnores' pat tailPath
         in if pAnchored pat
              then if match then continued else Nothing
              else (if match && isJust continued then continued else retry)

patternIgnores :: Pattern -> [OsPath] -> Bool -> Maybe Bool
patternIgnores pat path dir =
  if (pDir pat && not dir) || null path
    then Nothing
    else patternIgnores' pat path

-- TODO: Weird naming
ignores' :: Ignore -> [OsPath] -> Bool -> Bool
ignores' (Ignore patterns) splitPath dir =
  last $ False : mapMaybe (\pat -> patternIgnores pat splitPath dir) patterns

ignores :: Ignore -> OsPath -> Bool -> Bool
ignores pat path = ignores' pat (splitDirectories path)
