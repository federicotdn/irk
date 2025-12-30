module InfixSpec (spec) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Infix
import Test.Hspec

text :: T.Text
text =
  "#This is an example text I have placed here in order to test out the isInfixC function.\n\
  \The text is long enough so that we can effectively perform a SIMD search using AVX2 instructions.\n\
  \It's all quite interesting, I think. Here are some more random words."

spec :: Spec
spec = do
  describe "cpuHasAVX2" $ do
    it "reports True when the CPU supports AVX2 instructions" $ do
      -- It is very unlikely that the CPU we are running on will not
      -- support AVX2.
      cpuHasAVX2 `shouldBe` True

  describe "isInfixOfC" $ do
    let encoded = encodeUtf8 text

    it "detects infix correctly (basic)" $ do
      (encodeUtf8 "" `isInfixOfC` encoded) `shouldBe` True
      (encodeUtf8 "foobar" `isInfixOfC` encoded) `shouldBe` False
      (encodeUtf8 "random" `isInfixOfC` encoded) `shouldBe` True
      (encodeUtf8 "ds." `isInfixOfC` encoded) `shouldBe` True
      (encodeUtf8 "#Th" `isInfixOfC` encoded) `shouldBe` True
      (encoded `isInfixOfC` encoded) `shouldBe` True
