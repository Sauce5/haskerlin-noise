module Octaves
( octave
) where

import Data.List (transpose)

import NoiseTypes
import MapInit
import Gradients
import Smoothing

{-
    Generates an octave of Perlin noise.
    Takes a corner map of influence vectors and an empty map.
-}
octave :: CornerMap -> Integer -> Integer -> Map
octave cm cl sz = transpose $ blend (transpose ulurMap) (transpose lllrMap) sz
    where ulurMap = blend ulMap urMap sz
          lllrMap = blend llMap lrMap sz
          ulMap = gradients cm (pixels cl sz) sz (0,0)
          urMap = gradients cm (pixels cl sz) sz (0,1)
          llMap = gradients cm (pixels cl sz) sz (1,0)
          lrMap = gradients cm (pixels cl sz) sz (1,1)
