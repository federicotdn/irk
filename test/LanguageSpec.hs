module LanguageSpec (spec) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Language
import Test.Hspec
import Testing (readTestFile)
import Types (IrkFile (..), IrkFilePos (..), file)
import Utils (os)

spec :: Spec
spec = do
  describe "languageByPath" $ do
    it "retrieves the correct language" $ do
      lName (fromJust $ languageByPath (os "test.hs")) `shouldBe` "Haskell"

  describe "python" $ do
    let py = fromJust $ Map.lookup "python" languages

    it "processes results correctly" $ do
      let pos path = IrkFilePos (file {iPath = os path}) 0 0
      lProcessResults py [] `shouldBe` []
      lProcessResults py [pos "a/b.py"] `shouldBe` [pos "a/b.py"]
      lProcessResults py [pos "a/lib.py", pos "a/lib64.py"] `shouldBe` [pos "a/lib.py", pos "a/lib64.py"]
      lProcessResults py [pos ".venv/lib/a/b.py"] `shouldBe` [pos ".venv/lib/a/b.py"]
      lProcessResults py [pos ".venv/lib64/a/b.py"] `shouldBe` [pos ".venv/lib64/a/b.py"]
      lProcessResults py [pos ".venv/lib64/a/b.py", pos ".venv/lib/a/b.py"] `shouldBe` [pos ".venv/lib/a/b.py"]

    it "finds the symbol definition(s)" $ do
      pythonExample <- readTestFile "python/test.py"

      lFindSymbolDefinition py "missing" pythonExample `shouldBe` []
      lFindSymbolDefinition py "ident1" pythonExample `shouldBe` [IrkFilePos file 2 4]
      lFindSymbolDefinition py "ident2" pythonExample `shouldBe` [IrkFilePos file 7 4, IrkFilePos file 9 4]
      lFindSymbolDefinition py "ident4" pythonExample `shouldBe` [IrkFilePos file 13 8]
      lFindSymbolDefinition py "Ident5" pythonExample `shouldBe` [IrkFilePos file 18 6]
      lFindSymbolDefinition py "Ident6" pythonExample `shouldBe` [IrkFilePos file 21 10]
      lFindSymbolDefinition py "IDENT7" pythonExample `shouldBe` [IrkFilePos file 24 0]
      lFindSymbolDefinition py "ident8" pythonExample `shouldBe` []

    it "gets the symbol at a position correctly" $ do
      lSymbolAtPosition py "" (IrkFilePos file 0 0) `shouldBe` Nothing

      lSymbolAtPosition py "foobar" (IrkFilePos file 0 0) `shouldBe` Just "foobar"
      lSymbolAtPosition py "foobar" (IrkFilePos file 0 0) `shouldBe` Just "foobar"
      lSymbolAtPosition py "foobar" (IrkFilePos file 0 4) `shouldBe` Just "foobar"
      lSymbolAtPosition py "foobar" (IrkFilePos file 0 6) `shouldBe` Just "foobar"

      lSymbolAtPosition py "foobar_" (IrkFilePos file 0 0) `shouldBe` Just "foobar_"
      lSymbolAtPosition py "foobar_a" (IrkFilePos file 0 0) `shouldBe` Just "foobar_a"

      lSymbolAtPosition py "_foobar" (IrkFilePos file 0 2) `shouldBe` Just "_foobar"
      lSymbolAtPosition py "a_foobar" (IrkFilePos file 0 2) `shouldBe` Just "a_foobar"

      lSymbolAtPosition py "x foobar y" (IrkFilePos file 0 4) `shouldBe` Just "foobar"
      lSymbolAtPosition py "x foobar\n y" (IrkFilePos file 0 4) `shouldBe` Just "foobar"
      lSymbolAtPosition py "x \nfoobar\n y" (IrkFilePos file 1 4) `shouldBe` Just "foobar"

  describe "c" $ do
    let c = fromJust $ Map.lookup "c" languages

    it "finds the symbol definition(s)" $ do
      cExample <- readTestFile "c/test.c"

      lFindSymbolDefinition c "missing" cExample `shouldBe` []
      lFindSymbolDefinition c "IDENT1" cExample `shouldBe` [IrkFilePos file 2 8]
      lFindSymbolDefinition c "IDENT2" cExample `shouldBe` [IrkFilePos file 3 10]
      lFindSymbolDefinition c "ident3" cExample `shouldBe` [IrkFilePos file 7 5]
      lFindSymbolDefinition c "ident4" cExample `shouldBe` [IrkFilePos file 9 5]
      lFindSymbolDefinition c "ident5" cExample `shouldBe` [IrkFilePos file 13 5]
      lFindSymbolDefinition c "ident6" cExample `shouldBe` [IrkFilePos file 18 5]
      lFindSymbolDefinition c "ident7" cExample `shouldBe` []
      lFindSymbolDefinition c "ident8" cExample `shouldBe` [IrkFilePos file 25 7]
      lFindSymbolDefinition c "IDENT9" cExample `shouldBe` [IrkFilePos file 27 8]
      lFindSymbolDefinition c "ident10" cExample `shouldBe` [IrkFilePos file 31 12]

  describe "go" $ do
    let go = fromJust $ Map.lookup "go" languages

    it "finds the symbol definition(s)" $ do
      goExample <- readTestFile "go/main.go"

      lFindSymbolDefinition go "missing" goExample `shouldBe` []
      lFindSymbolDefinition go "MyIdent1" goExample `shouldBe` [IrkFilePos file 8 5]
      lFindSymbolDefinition go "MyIdent2" goExample `shouldBe` [IrkFilePos file 13 5]
      lFindSymbolDefinition go "MyIdent3" goExample `shouldBe` [IrkFilePos file 17 5]
      lFindSymbolDefinition go "ident5" goExample `shouldBe` [IrkFilePos file 23 5]
      lFindSymbolDefinition go "ident6" goExample `shouldBe` [IrkFilePos file 27 20]
      lFindSymbolDefinition go "myIdent7" goExample `shouldBe` [IrkFilePos file 31 5]
      lFindSymbolDefinition go "myIdent8" goExample `shouldBe` [IrkFilePos file 32 5]
      lFindSymbolDefinition go "MyIdent9" goExample `shouldBe` [IrkFilePos file 34 5]

  describe "haskell" $ do
    let hs = fromJust $ Map.lookup "haskell" languages

    it "finds the symbol definition(s)" $ do
      hsExample <- readTestFile "haskell/test.hs"

      lFindSymbolDefinition hs "missing" hsExample `shouldBe` []
      lFindSymbolDefinition hs "ident1" hsExample `shouldBe` [IrkFilePos file 2 0]
      lFindSymbolDefinition hs "Ident2" hsExample `shouldBe` [IrkFilePos file 5 5]
      lFindSymbolDefinition hs "Ident3" hsExample `shouldBe` [IrkFilePos file 7 8]
      lFindSymbolDefinition hs "Ident4" hsExample `shouldBe` [IrkFilePos file 9 5]
      lFindSymbolDefinition hs "Ident5" hsExample `shouldBe` [IrkFilePos file 11 6]
      lFindSymbolDefinition hs "Ident6" hsExample `shouldBe` [IrkFilePos file 0 7]
