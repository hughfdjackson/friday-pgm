{-# LANGUAGE OverloadedStrings #-}

module Vision.Image.Storage.PGM.Internal (
  PGM(..),
  parse,
  format,
) where

import Vision.Image.Type (Manifest)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Data.Binary
import Data.Functor ((<$>))
import Data.Monoid (mconcat)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as P

-- | AST representation of the literal shape of the PGM
-- including the 'max' value.  When translating into a
-- friday GreyImage, some normalisation may be necessary
data PGM = PGM {
  pgmWidth   :: Int,
  pgmHeight  :: Int,
  pgmMaxVal  :: Int,
  pgmPixels  :: V.Vector Word8
} deriving (Eq, Show)

-- PARSE
parse :: B.ByteString -> Either String PGM
parse = P.parseOnly parsePGM

parsePGM :: P.Parser PGM
parsePGM = do
  P.string "P5"
  skipSpaces
  width <- takeInt
  skipSpaces
  height <- takeInt
  skipSpaces
  maxVal <- takeInt
  P.skip isSpace
  px <- V.fromList . B.unpack <$> P.takeByteString
  return $ PGM width height maxVal px

skipSpaces = P.skipWhile isSpace

isSpace = flip elem spaceCodes
  where spaceCodes = B.unpack $ BC.pack " \t\n\r\f"

takeInt :: P.Parser Int
takeInt = readInt . BC.unpack <$> P.takeTill isSpace
  where readInt = read :: String -> Int


-- FORMAT
format :: PGM -> B.ByteString
format (PGM w h m px) = mconcat [headers, pixels]
  where headers = BC.pack $ "P5 " ++ show w ++ " " ++ " " ++ show h ++ " " ++ show m ++ " "
        pixels  = B.pack $ V.toList px
