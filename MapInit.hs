module MapInit
( randomsToVectors
, vectors
, pixels
) where

import NoiseTypes

-- generate normalized vectors from random numbers

{-
    Turns list of random integers into list of vectors.
-}
randomsToVectors :: [Integer] -> [Vector]
randomsToVectors [] = []
randomsToVectors (x:y:z:xs) = newVector (-1) (-1) (fx, fy) : randomsToVectors xs
      where theta = (x*100) + (y*10) + z
            fx = cos (fromIntegral theta * (pi/180.0))
            fy = sin (fromIntegral theta * (pi/180.0))

{-
    Initialize CornerMap of vectors
-}
vectors :: [Vector] -> Integer -> CornerMap
vectors vs n = indexVectors mat 0
   where mat = vectorsToMap vs n

vectorsToMap :: [Vector] -> Integer -> CornerMap
vectorsToMap [] _ = []
vectorsToMap vs n = row : vectorsToMap nvs n
        where row = fst spl
              nvs = snd spl
              spl = splitAt (fromIntegral n) vs

{-
    Add indices to each vector
-}
indexVectors :: CornerMap -> Int -> CornerMap
indexVectors []     _ = []
indexVectors (r:rs) i = indexVectorsRow r i 0 : indexVectors rs (i+1)

indexVectorsRow :: [Vector] -> Int -> Int -> [Vector]
indexVectorsRow []     _ _ = []
indexVectorsRow (v:vs) i j = newVector i j (vec v) : indexVectorsRow vs i (j+1)

{-
    Initialize Map of empty pixels
-}
pixels :: Integer -> Integer -> Map
pixels ch sz = indexPixels mat 0
      where mat = genEmptyMap ch sz

genEmptyMap :: Integer -> Integer -> Map
genEmptyMap cs sz = replicate (fromIntegral ps) row
        where row = replicate (fromIntegral ps) (newPixel (-1) (-1) 0.0)
              ps  = cs * sz

{-
    Add indices to each pixel
-}
indexPixels :: Map -> Int -> Map
indexPixels [] _ = []
indexPixels (r:rs) i = indexPixelsRow r i 0 : indexPixels rs (i+1)

indexPixelsRow :: [Pixel] -> Int -> Int -> [Pixel]
indexPixelsRow []     _ _ = []
indexPixelsRow (p:ps) i j = newPixel i j (pix p) : indexPixelsRow ps i (j+1)
