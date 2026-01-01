module Ignore
  ( Ignore (..),
    Part (..),
    Pattern (..),
    parse,
    ignores,
    ignores',
  )
where

import Data.List (intersperse, tails)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, splitDirectories, unsafeEncodeUtf)
import qualified System.OsString as OS
import Utils (os)

data Part = Sep | DAsterisk | Path OsPath deriving (Show, Eq)

data Pattern = Negated Pattern | Pattern [Part] Bool deriving (Show, Eq)

newtype Ignore = Ignore [Pattern] deriving (Show, Eq)

instance Semigroup Ignore where
  (Ignore p1) <> (Ignore p2) = Ignore (p1 <> p2)

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

simplify :: Pattern -> Pattern
simplify (Negated pat) = Negated (simplify pat)
simplify pat@(Pattern parts dir) = case parts of
  [Sep] -> Pattern [] dir
  [DAsterisk] -> Pattern [Path $ os "*"] dir
  (Sep : DAsterisk : rest) -> Pattern (DAsterisk : rest) dir
  _ -> pat

parse :: Text -> Ignore
parse source =
  let sourceLines = filter (not . T.null) $ map T.strip $ T.lines source
      patterns = filter (\l -> T.head l /= '#') sourceLines
   in Ignore (map (simplify . parsePattern) patterns)

-- TODO: Not complete, though it's good enough for most use cases.
pathMatches :: OsPath -> OsPath -> Bool
pathMatches path target
  | os "*" `OS.isPrefixOf` target = OS.tail target `OS.isSuffixOf` path
  | os "*" `OS.isSuffixOf` target = OS.init target `OS.isPrefixOf` path
  | path == target = True
  | otherwise = False

patternIgnores' :: Pattern -> Bool -> [OsPath] -> Maybe Bool
patternIgnores' (Negated pat) anchored splitPath =
  not <$> patternIgnores' pat anchored splitPath
patternIgnores' pat@(Pattern parts dir) anchored splitPath =
  if null splitPath
    then
      -- Exhausted path without returning False, meaning we might
      -- have an ignore-match. This depends on whether there is more
      -- pattern to match.
      if null parts then Just True else Nothing
    else case parts of
      [] -> Nothing
      (Sep : rest) -> patternIgnores' (Pattern rest dir) anchored splitPath
      (DAsterisk : rest) ->
        if null rest
          then Just True
          else
            let matches = mapMaybe (patternIgnores' (Pattern rest dir) anchored) (tails splitPath)
             in if null matches then Nothing else Just (or matches)
      (Path target : rest) ->
        let tailPath = tail splitPath
            match = pathMatches (head splitPath) target
            continued = patternIgnores' (Pattern rest dir) anchored tailPath
            retry = if null tailPath then Nothing else patternIgnores' pat anchored tailPath
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
    else patternIgnores' pat (patternAnchored pat) path

-- TODO: Weird naming
ignores' :: Ignore -> [OsPath] -> Bool -> Bool
ignores' (Ignore patterns) splitPath dir =
  last $ False : mapMaybe (\pat -> patternIgnores pat splitPath dir) patterns

ignores :: Ignore -> OsPath -> Bool -> Bool
ignores pat path = ignores' pat (splitDirectories path)
