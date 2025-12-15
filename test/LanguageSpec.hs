module LanguageSpec (spec) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Language
import Test.Hspec
import Testing (readTestFile)
import Utils (FilePos (..), os)

spec :: Spec
spec = do
  describe "languageByPath" $ do
    it "retrieves the correct language" $ do
      lName (fromJust $ languageByPath (os "test.hs")) `shouldBe` "Haskell"

  describe "python" $ do
    let py = fromJust $ Map.lookup "python" languages

    it "finds the symbol definition(s)" $ do
      pythonExample <- readTestFile "python/test.py"

      lFindSymbolDefinition py "missing" pythonExample `shouldBe` []
      lFindSymbolDefinition py "ident1" pythonExample `shouldBe` [FilePos Nothing 2 4]
      lFindSymbolDefinition py "ident2" pythonExample `shouldBe` [FilePos Nothing 7 4, FilePos Nothing 9 4]
      lFindSymbolDefinition py "ident4" pythonExample `shouldBe` [FilePos Nothing 13 8]
      lFindSymbolDefinition py "Ident5" pythonExample `shouldBe` [FilePos Nothing 18 6]
      lFindSymbolDefinition py "Ident6" pythonExample `shouldBe` [FilePos Nothing 21 10]

    it "gets the symbol at a position correctly" $ do
      lSymbolAtPosition py "" (FilePos Nothing 0 0) `shouldBe` Nothing

      lSymbolAtPosition py "foobar" (FilePos Nothing 0 0) `shouldBe` Just "foobar"
      lSymbolAtPosition py "foobar" (FilePos Nothing 0 0) `shouldBe` Just "foobar"
      lSymbolAtPosition py "foobar" (FilePos Nothing 0 4) `shouldBe` Just "foobar"
      lSymbolAtPosition py "foobar" (FilePos Nothing 0 6) `shouldBe` Just "foobar"

      lSymbolAtPosition py "foobar_" (FilePos Nothing 0 0) `shouldBe` Just "foobar_"
      lSymbolAtPosition py "foobar_a" (FilePos Nothing 0 0) `shouldBe` Just "foobar_a"

      lSymbolAtPosition py "_foobar" (FilePos Nothing 0 2) `shouldBe` Just "_foobar"
      lSymbolAtPosition py "a_foobar" (FilePos Nothing 0 2) `shouldBe` Just "a_foobar"

      lSymbolAtPosition py "x foobar y" (FilePos Nothing 0 4) `shouldBe` Just "foobar"
      lSymbolAtPosition py "x foobar\n y" (FilePos Nothing 0 4) `shouldBe` Just "foobar"
      lSymbolAtPosition py "x \nfoobar\n y" (FilePos Nothing 1 4) `shouldBe` Just "foobar"

  describe "c" $ do
    let c = fromJust $ Map.lookup "c" languages

    it "finds the symbol definition(s)" $ do
      cExample <- readTestFile "c/test.c"

      lFindSymbolDefinition c "missing" cExample `shouldBe` []
      lFindSymbolDefinition c "IDENT1" cExample `shouldBe` [FilePos Nothing 2 8]
      lFindSymbolDefinition c "IDENT2" cExample `shouldBe` [FilePos Nothing 3 10]
      lFindSymbolDefinition c "ident3" cExample `shouldBe` [FilePos Nothing 7 5]
      lFindSymbolDefinition c "ident4" cExample `shouldBe` [FilePos Nothing 9 5]
      lFindSymbolDefinition c "ident5" cExample `shouldBe` [FilePos Nothing 13 5]
      lFindSymbolDefinition c "ident6" cExample `shouldBe` [FilePos Nothing 18 5]
      lFindSymbolDefinition c "ident7" cExample `shouldBe` []
      lFindSymbolDefinition c "ident8" cExample `shouldBe` [FilePos Nothing 25 7]
      lFindSymbolDefinition c "IDENT9" cExample `shouldBe` [FilePos Nothing 27 8]
      lFindSymbolDefinition c "ident10" cExample `shouldBe` [FilePos Nothing 31 12]

  describe "go" $ do
    let go = fromJust $ Map.lookup "go" languages

    it "finds the symbol definition(s)" $ do
      goExample <- readTestFile "go/main.go"

      lFindSymbolDefinition go "missing" goExample `shouldBe` []
      lFindSymbolDefinition go "MyIdent1" goExample `shouldBe` [FilePos Nothing 8 5]
      lFindSymbolDefinition go "MyIdent2" goExample `shouldBe` [FilePos Nothing 13 5]
      lFindSymbolDefinition go "MyIdent3" goExample `shouldBe` [FilePos Nothing 17 5]
      lFindSymbolDefinition go "ident5" goExample `shouldBe` [FilePos Nothing 23 5]
      lFindSymbolDefinition go "ident6" goExample `shouldBe` [FilePos Nothing 27 20]
      lFindSymbolDefinition go "myIdent7" goExample `shouldBe` [FilePos Nothing 31 5]
      lFindSymbolDefinition go "myIdent8" goExample `shouldBe` [FilePos Nothing 32 5]
      lFindSymbolDefinition go "MyIdent9" goExample `shouldBe` [FilePos Nothing 34 5]

  describe "haskell" $ do
    let hs = fromJust $ Map.lookup "haskell" languages

    it "finds the symbol definition(s)" $ do
      hsExample <- readTestFile "haskell/test.hs"

      lFindSymbolDefinition hs "missing" hsExample `shouldBe` []
      lFindSymbolDefinition hs "ident1" hsExample `shouldBe` [FilePos Nothing 2 0]
      lFindSymbolDefinition hs "Ident2" hsExample `shouldBe` [FilePos Nothing 5 5]
      lFindSymbolDefinition hs "Ident3" hsExample `shouldBe` [FilePos Nothing 7 8]
      lFindSymbolDefinition hs "Ident4" hsExample `shouldBe` [FilePos Nothing 9 5]
      lFindSymbolDefinition hs "Ident5" hsExample `shouldBe` [FilePos Nothing 11 6]
      lFindSymbolDefinition hs "Ident6" hsExample `shouldBe` [FilePos Nothing 0 7]
