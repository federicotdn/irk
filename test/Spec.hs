module Main (main) where

import qualified InfixSpec
import qualified IrkSpec
import qualified LSPServerSpec
import qualified LSPSpec
import qualified LanguageSpec
import Test.Hspec
import qualified UtilsSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Irk" IrkSpec.spec
  describe "LSPServer" LSPServerSpec.spec
  describe "LSP" LSPSpec.spec
  describe "Language" LanguageSpec.spec
  describe "Utils" UtilsSpec.spec
  describe "Infix" InfixSpec.spec
