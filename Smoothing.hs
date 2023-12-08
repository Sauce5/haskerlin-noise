module Smoothing
( blend
) where

import NoiseTypes

{-
    Blend 2 maps together.
-}
blend :: Map -> Map -> Integer -> Map
blend [] [] _ = []
blend (r1:m1) (r2:m2) sz = blendRows r1 r2 sz : blend m1 m2 sz

blendRows :: [Pixel] -> [Pixel] -> Integer -> [Pixel]
blendRows [] [] _ = []
blendRows r1 r2 sz = transition c1 c2 sz 0 ++ blendRows c1s c2s sz
    where (c1,c1s) = splitAt (fromIntegral sz) r1
          (c2,c2s) = splitAt (fromIntegral sz) r2

{-
    Combined lerp and smooth step function
    (calculated across a row of pixels)
-}
transition :: [Pixel] -> [Pixel] -> Integer -> Integer -> [Pixel]
transition [] [] _ _ = []
transition (p1:p1s) (p2:p2s) sz i = newPixel (pixX p1) (pixY p1) tp : transition p1s p2s sz (i+1)
    where tp = lerp (smoothStepScaled (fromIntegral i) (fromIntegral sz)) x1 x2
          x1 = pix p1
          x2 = pix p2

{-
    Linear intERPolation function.
-}
lerp :: Double -> Double -> Double -> Double
lerp t a1 a2 = a1 + t*(a2-a1)

{-
    Smooth step function from (0,0) to (1,1).
    Optimized form of
        6t^5 - 15t^4 + 10t^3
-}
smoothStep :: Double -> Double
smoothStep t = ((6*t - 15)*t + 10)*t*t*t

{-
    Scaled wrapper for smooth step function
-}
smoothStepScaled :: Double -> Double -> Double
smoothStepScaled t s = smoothStep (t / s)
