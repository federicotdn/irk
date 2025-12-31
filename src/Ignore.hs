module Ignore where

import Data.List (intersperse, tails)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, splitDirectories, unsafeEncodeUtf)
import Utils (os)

data Part = Sep | DAsterisk | Path OsPath deriving (Show, Eq)

data Pattern = Negated Pattern | Pattern [Part] Bool deriving (Show, Eq)

newtype Ignore = Ignore [Pattern] deriving (Show, Eq)

parsePart :: Text -> Part
parsePart source = case source of
  "/" -> Sep
  "**" -> DAsterisk
  -- TODO: Make it safe
  _ -> Path $ unsafeEncodeUtf (T.unpack source)

parsePattern :: Text -> Pattern
parsePattern source =
  if not (T.null source) && T.head source == '!'
    then
      Negated $ parsePattern (T.tail source)
    else
      let parts = filter (not . T.null) $ T.splitOn "/" source
          prefix = [Sep | "/" `T.isPrefixOf` source]
          dir = "/" `T.isSuffixOf` source
       in Pattern (prefix ++ intersperse Sep (map parsePart parts)) dir

parse :: Text -> Ignore
parse source =
  let sourceLines = filter (not . T.null) $ map T.strip $ T.lines source
      patterns = filter (\l -> T.head l /= '#') sourceLines
   in Ignore (map parsePattern patterns)

pathMatches :: OsPath -> OsPath -> Bool
pathMatches path target = path == target || target == os "*" -- TODO: Implement '*'

patternIgnores' :: Pattern -> Bool -> [OsPath] -> Maybe Bool
patternIgnores' (Negated pat) anchored path =
  not <$> patternIgnores' pat anchored path
patternIgnores' pat@(Pattern parts dir) anchored path =
  if null path
    then
      -- Exhausted path without returning False, meaning we might
      -- have an ignore-match. This depends on whether there is more
      -- nonempty pattern to match.
      Just (patternEmpty pat)
    else case parts of
      [] -> Nothing
      (Sep : rest) -> patternIgnores' (Pattern rest dir) anchored path
      (DAsterisk : rest) ->
        if null rest
          then if null path then Nothing else Just True
          else
            let matches = mapMaybe (patternIgnores' (Pattern rest dir) anchored) (tails path)
             in if null matches then Nothing else Just $ or matches
      (Path target : rest) ->
        let tpath = tail path
            match = pathMatches (head path) target
            continued = patternIgnores' (Pattern rest dir) anchored tpath
            retry = if null tpath then Nothing else patternIgnores' pat anchored tpath
         in if anchored
              then if match then continued else Nothing
              else (if match && isJust continued then continued else retry)

patternDir :: Pattern -> Bool
patternDir (Pattern _ dir) = dir
patternDir (Negated pat) = patternDir pat

patternEmpty :: Pattern -> Bool
patternEmpty (Pattern parts _) = all (== Sep) parts
patternEmpty (Negated pat) = patternEmpty pat

patternAnchored :: Pattern -> Bool
patternAnchored (Pattern parts _) = Sep `elem` parts
patternAnchored (Negated pat) = patternAnchored pat

patternIgnores :: Pattern -> [OsPath] -> Bool -> Maybe Bool
patternIgnores pat path dir =
  if (patternDir pat && not dir) || null path
    then Nothing
    else patternIgnores' pat (patternAnchored pat) path

ignores :: Ignore -> OsPath -> Bool -> Bool
ignores (Ignore patterns) path dir -- TODO: splitDirectories is probably very slow
  =
  last $ False : mapMaybe (\pat -> patternIgnores pat (splitDirectories path) dir) patterns
