module Gradients
( gradients
) where

import NoiseTypes

-- cornermap, map, chunk size, infl vector to use
gradients :: CornerMap -> Map -> Integer -> (Integer,Integer) -> Map
gradients cm m sz c = bindMap m (grad cm sz c)

-- grad function for each pixel
grad :: CornerMap -> Integer -> (Integer,Integer) -> Pixel -> Pixel
grad cm sz c p = newPixel (pixX p) (pixY p) (dotProduct iv ov)
      where iv = inflVectorOf p cm c sz
            ov = offsVectorOf p c sz

inflVectorOf :: Pixel -> CornerMap -> (Integer,Integer) -> Integer -> Vector
inflVectorOf p cm (io1,io2) sz = (cm !! fromIntegral i1) !! fromIntegral i2
       where (i1,i2) = (io1 + (p1 `div` sz), io2 + (p2 `div` sz))
             (p1,p2) = (fromIntegral (pixX p), fromIntegral (pixY p))

-- Integer arg is chunk size
offsVectorOf :: Pixel -> (Integer,Integer) -> Integer -> Vector
offsVectorOf p (ip1,ip2) sz = newVector (-1) (-1) (fromIntegral $ o1, fromIntegral $ o2)
              where (o1,o2) = (m1 - (ip1*sz), m2 - (ip2*sz))
                    (m1,m2) = (p1 `mod` sz, p2 `mod` sz)
                    (p1,p2) = (fromIntegral (pixX p), fromIntegral (pixY p))

{-
    Calculates dot product of two vectors.
-}
dotProduct :: Vector -> Vector -> Double
dotProduct i o = (i1 * o1) + (i2 * o2)
 where (i1,i2) = vec i
       (o1,o2) = vec o
