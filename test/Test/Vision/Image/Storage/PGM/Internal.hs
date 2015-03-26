module Test.Vision.Image.Storage.PGM.Internal (
  testInternal,
  pgmFile, pgm
) where

import Vision.Image.Storage.PGM.Internal as I

import Test.Hspec
import Test.QuickCheck

import Data.Functor ((<$>))
import Data.Monoid (mconcat)
import Data.Either (isRight)
import Data.Binary (Word8)
import Data.List (intersperse)
import Data.Convertible (convert)
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

-- Generators
sepBy gen xs = sequence $ intersperse gen $ return <$> xs
space = elements [" ", "\t", "\n", "\r", "\f"]
spaces = mconcat <$> listOf1 space

pixels :: Int -> Int -> Int -> Gen [Word8]
pixels w h m = vectorOf (w * h) $ elements [0..(convert m)]

number :: Gen Int
number = getNonNegative <$> arbitrary

config :: Gen (Int, Int, Int, [Word8])
config = do
  width  <- number
  height <- number
  maxVal <- return 255
  px <- pixels width height maxVal
  return (width, height, maxVal, px)

pgm :: Gen PGM
pgm = pgm' <$> config

pgm' :: (Int, Int, Int, [Word8]) -> PGM
pgm' (width, height, maxVal, px) = PGM width height maxVal $ V.fromList px

pgmFile = pgmFile' <$> config

pgmFile' (width, height, maxVal, px) = do
  let headerItems = "P5" : (show <$> [width, height, maxVal])
  header <- BC.pack . mconcat <$> sepBy spaces headerItems
  space' <- BC.pack <$> space
  let body = B.pack px

  return $ mconcat [header, space', body]

pgmAndPgmFile :: Gen (PGM, B.ByteString)
pgmAndPgmFile = do
  config' <- config
  let pgm'' = pgm' config'
  file' <- pgmFile' config'
  return (pgm'', file')

-- Tests
testInternal = describe "Vision.Image.Storage.PGM.Internal" $ do
  it "should have the property parse . format == Right id" $ property $
    forAll pgm (\pgm' -> (I.parse . I.format) pgm' `shouldBe` Right pgm')

  it "parsing file should produce the same as the pre-genned parsed equivalent" $ property $
    forAll pgmAndPgmFile (\(pgm', pgmFile') -> I.parse pgmFile' `shouldBe` Right pgm')