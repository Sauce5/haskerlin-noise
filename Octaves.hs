module Octaves
( genOctave
) where

import Data.List (transpose)
import System.Random (mkStdGen, Random(randomR), StdGen)

import NoiseTypes
import MapInit
import Gradients
import Smoothing

{-
    Generate an octave from a seed
    Seed -> Chunk Length -> Chunk Size -> Map
-}
genOctave :: Int -> Integer -> Integer -> Map
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

nextSquare :: Integer -> Integer
nextSquare n = (n + 1) * (n + 1)

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
