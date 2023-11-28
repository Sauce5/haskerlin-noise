module MapInit
( randomsToVectors
, assignVectors
, genEmptyMap
) where

import NoiseTypes

randomsToVectors :: [Integer] -> [Vector]
randomsToVectors [] = []
randomsToVectors [x] = []
randomsToVectors (x:y:xs) = normalize (fx,fy) : randomsToVectors xs
    where fx = fromIntegral x
          fy = fromIntegral y

hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt (a*a + b*b)

normalize :: Vector -> Vector
normalize (x,y) = (x/h,y/h)
        where h = hypotenuse x y

assignVectors :: [Vector] -> Integer -> CornerMap
assignVectors [] _ = []
assignVectors vs i = row : assignVectors nvs i
         where row = fst spl
               nvs = snd spl
               spl = splitAt (fromIntegral i) vs

genEmptyMap :: Integer -> Integer -> Map
genEmptyMap cs sz = replicate (fromIntegral ps) row
        where row = replicate (fromIntegral ps) 0.0
              ps  = cs * sz