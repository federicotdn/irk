module IgnoreSpec (spec) where

import Ignore
import Test.Hspec
import Utils (os)

path :: String -> Part
path p = Path $ os p

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
      ignores (parse "bar") (os "foo") False `shouldBe` False
      ignores (parse "foo") (os "foo") False `shouldBe` True
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
      ignores (parse "*") (os "foo/bar/baz") False `shouldBe` True
      ignores (parse "foo/*/bar") (os "foo/x/bar") False `shouldBe` True
      ignores (parse "foo/*/bar") (os "foo/x/baz") False `shouldBe` False
      ignores (parse "foo/*/bar") (os "foo/x") False `shouldBe` False
