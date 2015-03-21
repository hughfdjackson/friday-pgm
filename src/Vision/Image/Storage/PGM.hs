{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Vision.Image.Storage.PGM (
  save, load
) where

import Vision.Image.Storage.PGM.Internal (PGM(..), parse, format)

import Data.Convertible (Convertible(..), convert)
import Data.Functor ((<$>))
import Data.Word (Word8)
import System.FilePath (FilePath)
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V

import Vision.Image (Manifest (..), Grey)
import Vision.Image.Grey.Type (GreyPixel(..))
import Vision.Primitive.Shape (ix2, Z(..), (:.)(..))

instance Convertible PGM Grey where
    safeConvert = Right . pgmToGrey

pgmToGrey :: PGM -> Grey
pgmToGrey (PGM w h max px) = Manifest (ix2 h w) (GreyPixel `V.map` px)

instance Convertible Grey PGM where
  safeConvert = Right . greyToPGM

greyToPGM :: Grey -> PGM
greyToPGM (Manifest (Z :. w :. h) px) = PGM h w 255 (unwrap `V.map` px)
  where unwrap (GreyPixel n) = n

-- |IO
save :: FilePath -> Grey -> IO ()
save path = B.writeFile path . format . greyToPGM

load :: FilePath -> IO Grey
load path = do
  contents <- B.readFile path
  parsed <- eitherToM $ parse contents
  return $ pgmToGrey parsed

eitherToM :: (Monad m, Show a) => Either a b -> m b
eitherToM (Left a)  = fail $ show a
eitherToM (Right b) = return b