module Test.Vision.Image.Storage.PGM (testPGM) where

import Vision.Image.Storage.PGM
import Vision.Image.Storage.PGM.Internal as I
import Vision.Image.Grey (Grey)

import Data.Convertible (convert)
import Data.Either (isRight)

import Test.Hspec
import Test.QuickCheck
import Test.Vision.Image.Storage.PGM.Internal (pgmFile, pgm)

testPGM = describe "Vision.Image.Storage.PGM" $
  it "should hold that (convert :: Grey -> I.PGM) . (convert :: I.PGM -> Grey) == id" $ property $ do
    let toPGM  = convert :: Grey -> PGM
    let toGrey = convert :: PGM  -> Grey
    forAll pgm (\pgm -> (toPGM . toGrey) pgm `shouldBe` pgm)
