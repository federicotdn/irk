module Ignore where

import Data.List (intersperse)
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
  if T.head source == '!'
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

patternIgnores' :: Pattern -> [OsPath] -> Bool -> Maybe Bool
patternIgnores' (Negated pat) path anchored =
  not <$> patternIgnores' pat path anchored
patternIgnores' pat@(Pattern parts dir) path anchored =
  if null path
    then
      -- Exhausted path without returning False, meaning we have an ignore-match.
      Just True
    else case parts of
      [] -> Nothing
      [Sep] -> Nothing
      (Sep : rest) -> patternIgnores' (Pattern rest dir) path anchored
      (DAsterisk : _) -> undefined
      (Path target : rest) ->
        let tpath = tail path
            match = pathMatches (head path) target
            continued = patternIgnores' (Pattern rest dir) tpath anchored
            retry = if null tpath then Nothing else patternIgnores' pat tpath anchored
         in if anchored
              then if match then continued else Nothing
              else (if match && isJust continued then continued else retry)

patternDir :: Pattern -> Bool
patternDir (Pattern _ dir) = dir
patternDir (Negated pat) = patternDir pat

patternAnchored :: Pattern -> Bool
patternAnchored (Pattern parts _) = Sep `elem` parts
patternAnchored (Negated pat) = patternAnchored pat

patternIgnores :: Pattern -> [OsPath] -> Bool -> Maybe Bool
patternIgnores pat path dir =
  if (patternDir pat && not dir) || null path
    then Nothing
    else patternIgnores' pat path (patternAnchored pat)

ignores :: Ignore -> OsPath -> Bool -> Bool
ignores (Ignore patterns) path dir -- TODO: splitDirectories is probably very slow
  =
  last $ False : mapMaybe (\pat -> patternIgnores pat (splitDirectories path) dir) patterns
