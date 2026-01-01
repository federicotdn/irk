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

data Pattern = Pattern
  { pParts :: [Part],
    pDir :: Bool,
    pNegated :: Bool,
    pAnchored :: Bool
  }
  deriving (Show, Eq)

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
  let (source', negated) =
        if not (T.null source) && T.head source == '!'
          then (T.tail source, True)
          else (source, False)
      prefix = [Sep | "/" `T.isPrefixOf` source']
      parts = filter (not . T.null) $ T.splitOn "/" source'
      parts' = prefix ++ intersperse Sep (map parsePart parts)
      dir = "/" `T.isSuffixOf` source'
      anchored = Sep `elem` parts'
   in Pattern
        { pParts = parts',
          pDir = dir,
          pNegated = negated,
          pAnchored = anchored
        }

simplify :: Pattern -> Pattern
simplify pat@Pattern {pParts = parts} = case parts of
  [DAsterisk] -> pat {pParts = [Path $ os "*"]}
  (Sep : rest) -> pat {pParts = rest}
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

patternIgnores' :: Pattern -> [OsPath] -> Maybe Bool
patternIgnores' pat@Pattern {pNegated = True} splitPath =
  not <$> patternIgnores' pat {pNegated = False} splitPath
patternIgnores' pat@Pattern {pParts = parts} splitPath =
  if null splitPath
    then
      -- Exhausted path without returning False, meaning we might
      -- have an ignore-match. This depends on whether there is more
      -- pattern to match.
      if null parts then Just True else Nothing
    else case parts of
      [] -> Nothing
      (Sep : rest) -> patternIgnores' (pat {pParts = rest}) splitPath
      (DAsterisk : rest) ->
        if null rest
          then Just True
          else
            let matches = mapMaybe (patternIgnores' (pat {pParts = rest})) (tails splitPath)
             in if null matches then Nothing else Just (or matches)
      (Path target : rest) ->
        let tailPath = tail splitPath
            match = pathMatches (head splitPath) target
            continued = patternIgnores' (pat {pParts = rest}) tailPath
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
