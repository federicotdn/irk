module IgnoreSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Ignore
import Test.Hspec
import Utils (os)

path :: String -> Part
path p = Segment $ Const (os p)

parseln :: [Text] -> Ignore
parseln patterns = parse (T.intercalate "\n" patterns)

pat :: [Part] -> Bool -> Bool -> Bool -> Pattern
pat parts dir negated anchored =
  Pattern
    { pParts = parts,
      pDir = dir,
      pNegated = negated,
      pAnchored = anchored
    }

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses an ignore correctly" $ do
      parse "" `shouldBe` Ignore []
      parse "\n  \n\t\t\n \n" `shouldBe` Ignore []
      parse "# test" `shouldBe` Ignore []
      parse "  # test " `shouldBe` Ignore []
      parse "  # test\n#test" `shouldBe` Ignore []
      parse "path" `shouldBe` Ignore [pat [path "path"] False False False]
      parse "!path" `shouldBe` Ignore [pat [path "path"] False True False]
      parse "path/" `shouldBe` Ignore [pat [path "path"] True False False]
      parse "path///" `shouldBe` Ignore [pat [path "path"] True False False]
      parse "/path/" `shouldBe` Ignore [pat [path "path"] True False True]
      parse "/path/foo/" `shouldBe` Ignore [pat [path "path", path "foo"] True False True]
      parse "/path///foo/" `shouldBe` Ignore [pat [path "path", path "foo"] True False True]
      parse "**/path/" `shouldBe` Ignore [pat [DAsterisk, path "path"] True False True]
      parse "/path/**" `shouldBe` Ignore [pat [path "path", DAsterisk] False False True]
      parse "/path/**/" `shouldBe` Ignore [pat [path "path", DAsterisk] True False True]

  describe "ignores" $ do
    it "ignores paths correctly (single pattern)" $ do
      ignores (parse "") (os "") False `shouldBe` False
      ignores (parse "") (os "foo") False `shouldBe` False
      ignores (parse "!") (os "foo") False `shouldBe` False
      ignores (parse "!*") (os "foo") False `shouldBe` False
      ignores (parse "bar") (os "foo") False `shouldBe` False
      ignores (parse "!bar") (os "foo") False `shouldBe` False
      ignores (parse "foo") (os "foo") False `shouldBe` True
      ignores (parse "/") (os "foo") False `shouldBe` False
      ignores (parse "/") (os "foo") True `shouldBe` False
      ignores (parse "/") (os "") False `shouldBe` False
      ignores (parse "!foo") (os "foo") False `shouldBe` False
      ignores (parse "!!foo") (os "foo") False `shouldBe` False
      ignores (parse "foo") (os "foo") True `shouldBe` True
      ignores (parse "foo/") (os "foo") False `shouldBe` False
      ignores (parse "foo/") (os "foo") True `shouldBe` True
      ignores (parse "/foo") (os "foo") False `shouldBe` True
      ignores (parse "foo/bar") (os "foo/bar") False `shouldBe` True
      ignores (parse "foo/bar") (os "foo/baz") False `shouldBe` False
      ignores (parse "/foo/bar") (os "foo/bar") False `shouldBe` True
      ignores (parse "/foo/bar") (os "baz/foo/bar") False `shouldBe` False
      ignores (parse "bar") (os "foo/bar") False `shouldBe` True
      ignores (parse "bar/") (os "foo/bar") True `shouldBe` True
      ignores (parse "/bar") (os "foo/bar") False `shouldBe` False
      ignores (parse "foo/bar") (os "baz/foo/bar") False `shouldBe` False
      ignores (parse "*") (os "foo") False `shouldBe` True
      ignores (parse "*") (os "foo") True `shouldBe` True
      ignores (parse "*") (os "foo/bar/baz") False `shouldBe` True
      ignores (parse "/*") (os "foo") False `shouldBe` True
      ignores (parse "/*") (os "foo/bar") False `shouldBe` False
      ignores (parse "*/*") (os "foo/bar") False `shouldBe` True
      ignores (parse "*/*") (os "foo/bar/baz") False `shouldBe` False
      ignores (parse "*/*") (os "foo") False `shouldBe` False
      ignores (parse "foo/bar") (os "foo/x/bar") False `shouldBe` False
      ignores (parse "foo/*/bar") (os "foo/x/bar") False `shouldBe` True
      ignores (parse "foo/*/bar") (os "foo/x/baz") False `shouldBe` False
      ignores (parse "foo/*/bar") (os "foo/x") False `shouldBe` False
      ignores (parse "**") (os "foo") False `shouldBe` True
      ignores (parse "**") (os "foo/bar/baz") False `shouldBe` True
      ignores (parse "**/bar") (os "x/foo/bar") False `shouldBe` True
      ignores (parse "**/foo/bar") (os "x/foo/bar") False `shouldBe` True
      ignores (parse "**/foo/baz") (os "x/foo/bar") False `shouldBe` False
      ignores (parse "bar/**") (os "bar/a") False `shouldBe` True
      ignores (parse "*/**") (os "bar/a") False `shouldBe` True
      ignores (parse "bar/**") (os "bar") False `shouldBe` False
      ignores (parse "bar/**/x") (os "bar/x") False `shouldBe` True
      ignores (parse "bar/**/x") (os "bar/1/2/3/x") False `shouldBe` True
      ignores (parse "bar/**/x/") (os "bar/1/2/3/x") False `shouldBe` False
      ignores (parse "bar/**/x/") (os "bar/1/2/3/x") True `shouldBe` True
      ignores (parse "bar/**/") (os "bar/test") True `shouldBe` True
      ignores (parse "bar/**/") (os "bar/test") False `shouldBe` False
      ignores (parse "**/x") (os "foo") False `shouldBe` False
      ignores (parse "**/**") (os "foo") False `shouldBe` True
      ignores (parse "**/**") (os "foo/bar") False `shouldBe` True
      ignores (parse "!**/foo") (os "bar/foo") False `shouldBe` False
      ignores (parse "a/**/b/**/c") (os "a/x/b/y/c") False `shouldBe` True
      ignores (parse "a/**/m/**/c") (os "a/x/b/y/c") False `shouldBe` False
      ignores (parse "/**") (os "foo") False `shouldBe` True
      ignores (parse "/**") (os "foo/yyy") False `shouldBe` True
      ignores (parse "/**/foo") (os "foo") False `shouldBe` True
      ignores (parse "/**/foo") (os "x/foo") False `shouldBe` True
      ignores (parse "/**/foo") (os "x/y/foo") False `shouldBe` True
      ignores (parse "!foo/") (os "foo") True `shouldBe` False
      ignores (parse "!foo") (os "foo") True `shouldBe` False
      ignores (parse ".*") (os "foo") False `shouldBe` False
      ignores (parse ".*") (os ".foo") False `shouldBe` True
      ignores (parse ".*") (os "bar/.foo") False `shouldBe` True
      ignores (parse ".*") (os "bar/.") False `shouldBe` True
      ignores (parse "*.py") (os "test.py") False `shouldBe` True
      ignores (parse "*.py") (os "test.c") False `shouldBe` False
      ignores (parse "*.py") (os "foo/test.py") False `shouldBe` True
      ignores (parse "foo/*.py") (os "test.py") False `shouldBe` False
      ignores (parse "foo/*.py") (os "foo/test.py") False `shouldBe` True
      ignores (parse "**/*.py") (os "a/b/foo/test.py") False `shouldBe` True
      ignores (parse "**/*.py") (os "test.py") False `shouldBe` True
      ignores (parse "**/") (os "test") True `shouldBe` True
      ignores (parse "**/") (os "test/dir/foo") True `shouldBe` True
      ignores (parse "**/") (os "test/file") False `shouldBe` False
      ignores (parse "/*/") (os "test") True `shouldBe` True
      ignores (parse "/*/") (os "test/foo") True `shouldBe` False
      ignores (parse "**/*.py") (os "zzz") False `shouldBe` False
      ignores (parse "!**/*.py") (os "zzz") False `shouldBe` False
      -- From https://git-scm.com/docs/gitignore:
      ignores (parse "doc/frotz/") (os "doc/frotz") True `shouldBe` True
      ignores (parse "doc/frotz/") (os "a/doc/frotz") True `shouldBe` False
      ignores (parse "frotz/") (os "frotz") True `shouldBe` True
      ignores (parse "frotz/") (os "a/frotz") True `shouldBe` True
      ignores (parse "**/foo") (os "a/foo") False `shouldBe` True
      ignores (parse "**/foo/bar") (os "a/b/foo/bar") False `shouldBe` True
      ignores (parse "abc/**") (os "abc/foo/bar") False `shouldBe` True
      ignores (parse "a/**/b") (os "a/b") False `shouldBe` True
      ignores (parse "a/**/b") (os "a/x/b") False `shouldBe` True
      ignores (parse "a/**/b") (os "a/x/y/b") False `shouldBe` True
      ignores (parse "foo/*") (os "foo/test.json") False `shouldBe` True
      ignores (parse "foo/*") (os "foo/bar") True `shouldBe` True
      ignores (parse "foo/*") (os "foo/bar/hello.c") False `shouldBe` False

    it "ignores paths correctly (multiple patterns)" $ do
      ignores (parseln ["foo", "!foo"]) (os "foo") False `shouldBe` False
      ignores (parseln ["!foo", "!!foo"]) (os "foo") False `shouldBe` False
      ignores (parseln ["*", "!foo"]) (os "foo") False `shouldBe` False
      ignores (parseln ["*", "!*"]) (os "foo") False `shouldBe` False
      ignores (parseln ["*", "!bar"]) (os "foo") False `shouldBe` True
      ignores (parseln ["*", "!foo", "foo"]) (os "foo") False `shouldBe` True
      ignores (parseln ["test/**", "!test/foo"]) (os "test/foo") False `shouldBe` False
      ignores (parseln ["*.py", "!test.py"]) (os "test.py") False `shouldBe` False
      ignores (parseln ["*", "!*/"]) (os "foo/bar") True `shouldBe` False
      ignores (parseln ["*", "!*/"]) (os "foo/bar") False `shouldBe` True
      ignores (parseln ["*", "!*/", "!**/*.py"]) (os "bar.py") False `shouldBe` False
      ignores (parseln ["*", "!*/", "!**/*.py"]) (os "foo/bar.py") False `shouldBe` False
      ignores (parseln ["*", "!*/", "!*.py"]) (os "foo/bar.py") False `shouldBe` False
