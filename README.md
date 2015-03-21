# friday-pgm

This package offers native haskell parsing for a subset of the .pgm spec.

It will work with files where in the Raw PGM format, that have a maxVal of 255.

# Why

Setting up `friday-devil` can be a pain - preferable to have a native haskell parser for ease of portability. 

# Example

```haskell
import Vision.Image.Storage.PGM

main = do
  img <- load "example-images/my-img.pgm"
  -- use as you would any other Grey image in the friday library
```