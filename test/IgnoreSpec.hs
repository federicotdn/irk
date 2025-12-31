module IgnoreSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Ignore
import Test.Hspec
import Utils (os)

path :: String -> Part
path p = Path $ os p

parseln :: [Text] -> Ignore
parseln patterns = parse (T.intercalate "\n" patterns)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses an ignore correctly" $ do
      parse "" `shouldBe` Ignore []
      parse "\n  \n\t\t\n \n" `shouldBe` Ignore []
      parse "# test" `shouldBe` Ignore []
      parse "  # test " `shouldBe` Ignore []
      parse "  # test\n#test" `shouldBe` Ignore []
      parse "path" `shouldBe` Ignore [Pattern [path "path"] False]
      parse "!path" `shouldBe` Ignore [Negated $ Pattern [path "path"] False]
      parse "path/" `shouldBe` Ignore [Pattern [path "path"] True]
      parse "path///" `shouldBe` Ignore [Pattern [path "path"] True]
      parse "/path/" `shouldBe` Ignore [Pattern [Sep, path "path"] True]
      parse "/path/foo/" `shouldBe` Ignore [Pattern [Sep, path "path", Sep, path "foo"] True]
      parse "/path///foo/" `shouldBe` Ignore [Pattern [Sep, path "path", Sep, path "foo"] True]
      parse "**/path/" `shouldBe` Ignore [Pattern [DAsterisk, Sep, path "path"] True]
      parse "/path/**" `shouldBe` Ignore [Pattern [Sep, path "path", Sep, DAsterisk] False]
      parse "/path/**/" `shouldBe` Ignore [Pattern [Sep, path "path", Sep, DAsterisk] True]

  describe "ignores" $ do
    it "ignores paths correctly (single pattern)" $ do
      ignores (parse "") (os "") False `shouldBe` False
      ignores (parse "") (os "foo") False `shouldBe` False
      ignores (parse "!") (os "foo") False `shouldBe` False
      ignores (parse "!*") (os "foo") False `shouldBe` False
      ignores (parse "bar") (os "foo") False `shouldBe` False
      ignores (parse "foo") (os "foo") False `shouldBe` True
      ignores (parse "/") (os "foo") False `shouldBe` False
      ignores (parse "!foo") (os "foo") False `shouldBe` False
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
      ignores (parse "!**/foo") (os "bar/foo") False `shouldBe` False
      ignores (parse "a/**/b/**/c") (os "a/x/b/y/c") False `shouldBe` True
      ignores (parse "a/**/m/**/c") (os "a/x/b/y/c") False `shouldBe` False
      ignores (parse "/**") (os "foo") False `shouldBe` True
      ignores (parse "/**") (os "foo/bar") False `shouldBe` True
      ignores (parse "/**/foo") (os "foo") False `shouldBe` True
      ignores (parse "/**/foo") (os "x/foo") False `shouldBe` True
      ignores (parse "/**/foo") (os "x/y/foo") False `shouldBe` True
      ignores (parse "!foo/") (os "foo") True `shouldBe` False
      ignores (parse "!foo") (os "foo") True `shouldBe` False
      ignores (parse "*.py") (os "test.py") False `shouldBe` True
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

    it "ignores paths correctly (multiple patterns)" $ do
      ignores (parseln ["foo", "!foo"]) (os "foo") False `shouldBe` False
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
