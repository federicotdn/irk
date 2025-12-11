module Main (main) where

import qualified InfixSpec
import qualified IrkSpec
import qualified LSPSpec
import qualified LanguageSpec
import qualified ServerSpec
import Test.Hspec
import qualified UtilsSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Irk" IrkSpec.spec
  describe "Server" ServerSpec.spec
  describe "LSP" LSPSpec.spec
  describe "Language" LanguageSpec.spec
  describe "Utils" UtilsSpec.spec
  describe "Infix" InfixSpec.spec
