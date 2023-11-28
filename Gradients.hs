module Gradients
( assignGradients
) where

import NoiseTypes

assignGradients :: CornerMap -> (Integer,Integer) -> Map -> Map
assignGradients cm (0,0) om = om 
assignGradients cm (0,1) om = om
assignGradients cm (1,0) om = om
assignGradients cm (1,1) om = om

dotProduct :: Vector -> Vector -> Double
dotProduct (i1,i2) (o1,o2) = (i1 * o1) + (i2 * o2)

chunkOfPixel :: (Integer,Integer) -> Integer -> (Integer,Integer)
chunkOfPixel (p1,p2) sz = (c1,c2)
    where c1 = p1 `div` sz
          c2 = p2 `div` sz
