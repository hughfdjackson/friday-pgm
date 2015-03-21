module Main (main) where

import Test.Hspec
import Test.Vision.Image.Storage.PGM.Internal (testInternal)
import Test.Vision.Image.Storage.PGM (testPGM)

-- Tests
main = hspec $ do
  testInternal
  testPGM