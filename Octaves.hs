module Octaves
( fractal
) where

import Data.List (transpose)
import System.Random (mkStdGen, Random(randomR), StdGen)

import NoiseTypes
import MapInit
import Gradients
import Smoothing

{-
    Generate fractal noise
     - for now, stick with lacunarity = 2 and even chunk sizes
     - !!! don't use a chunk size that isn't perfectly divisible by the lacunarity !!!
    Seed -> Number of Octaves -> Fractal Perlin Noise Map
-}
fractal :: Int -> Integer -> Map
fractal seed n = addOctaves $ applyPersistance 0.5 octaves
    where octaves = genSeries seed n 6 12 2
        -- seed, octaves, chunk length, chunk size, lacunarity

{-
    Generate series of octaves with a specific lacunarity
    (lacunarity = basically how subdivided the chunks in a map are)
    Seed -> Number of Octaves -> Chunk Length -> Chunk Size -> Lacunarity -> [Octave]
-}
genSeries :: Int -> Integer -> Integer -> Integer -> Integer -> [Octave]
genSeries seed 0 ln sz l = []
genSeries seed n ln  0 l = []
genSeries seed n ln  1 l = []
genSeries seed n ln sz l = oct : genSeries seed (n-1) (ln * l) (sz `div` l) l
    where oct = genOctave seed ln sz

{-
    Generate an octave from a seed
    Seed -> Chunk Length -> Chunk Size -> Octave
-}
genOctave :: Int -> Integer -> Integer -> Octave
genOctave seed ln sz = do
    -- default parameters for now
    let chunks_length = ln       -- map is chunks_length x chunks_length
    let chunk_size = sz          -- each chunk is chunk_size x chunk_size
    -- number of vectors to make
    let num_vectors = nextSquare chunks_length
    -- generate randomly rotated vectors
    let gen = mkStdGen seed
    let rands = nextN (3*num_vectors) gen           -- each pair of nums makes a Vector
    let infl_vect_list = randomsToVectors rands
    -- init map and corner map
    let corner_map = vectors infl_vect_list (chunks_length + 1)
    octave corner_map chunks_length chunk_size

{-
    nextSquare(n) = (n+1)^2
-}
nextSquare :: Integer -> Integer
nextSquare n = (n + 1) * (n + 1)

{-
    Generates a list of n random digits
-}
nextN :: Integer -> StdGen -> [Integer]
nextN n gen = case abs n of
    0 -> []
    _ -> do
        let rn = randomR (0::Integer,9::Integer) gen
        let nx = fst rn
        nx : nextN (abs n - 1) (snd rn)

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

{-
    Modifies octaves using persistance variable
-}
applyPersistance :: Double -> [Octave] -> [Octave]
applyPersistance p [] = []
applyPersistance p (o:os) = po : applyPersistance (p * p) os
    where po = bindMap o (\px -> newPixel (pixX px) (pixY px) (p * pix px))

{-
    Adds octave values together into one map
-}
addOctaves :: [Octave] -> Map
addOctaves [] = []
addOctaves [o] = o
addOctaves (o:os) = addTwoOctaves o (addOctaves os)

addTwoOctaves :: Octave -> Octave -> Octave
addTwoOctaves [] [] = []
addTwoOctaves (r1:r1s) (r2:r2s) = addTwoOctavesRows r1 r2 : addTwoOctaves r1s r2s

addTwoOctavesRows :: [Pixel] -> [Pixel] -> [Pixel]
addTwoOctavesRows [] [] = []
addTwoOctavesRows (p1:p1s) (p2:p2s) = np : addTwoOctavesRows p1s p2s
    where np = newPixel (pixX p1) (pixY p1) (pix p1 + pix p2)
