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
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

-- Generators
magic :: BC.ByteString
magic = BC.pack "P5"

int :: Gen Int
int =  unpackPositive <$> (arbitrary :: Gen (Positive Int))
  where unpackPositive (Positive i) = i

space :: Gen BC.ByteString
space = BC.singleton <$> elements " \t\n\r\f"

spaces :: Gen BC.ByteString
spaces = mconcat <$> listOf1 space

pixel :: Word8 -> Gen B.ByteString
pixel max = B.singleton <$> elements [0..max]

pixels :: Int -> Int -> Word8 -> Gen B.ByteString
pixels w h max = mconcat <$> vectorOf (w * h) (pixel max)

packNum :: (Num a, Show a) => a -> BC.ByteString
packNum = BC.pack . show

pgmFile = do
  s       <- spaces
  width   <- int
  s'      <- spaces
  height  <- int
  s''     <- spaces
  let maxVal = 255 -- hardcoded for the meantime; will deal with > 255 format later
  s       <- space
  px      <- pixels width height maxVal
  return $ mconcat [magic, s, packNum width, s', packNum height, s'', packNum maxVal, s, px]

pgm :: Gen I.PGM
pgm = do
  w  <- int
  h  <- int
  let max = 255
  px <- vectorOf (w * h) $ elements ([0..255] :: [Word8])
  return $ I.PGM w h max $ V.fromList px

-- Tests
testInternal = describe "Vision.Image.Storage.PGM.Internal" $ do
  it "should have the property parse . format == Right id" $ property $
    forAll pgm (\pgm' -> (I.parse . I.format) pgm' `shouldBe` Right pgm')

  it "parse should succeed for all valid pgmFiles" $ property $
    forAll pgmFile (\pgmFile' -> I.parse pgmFile' `shouldSatisfy` isRight)
