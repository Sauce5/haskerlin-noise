import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)
import Data.List (transpose)

import NoiseTypes
import MapInit
import Gradients
import Display
import Smoothing
import Octaves

nextN :: Integer -> StdGen -> [Integer]
nextN n gen = case abs n of
    0 -> []
    _ -> do
        let rn = randomR (0::Integer,9::Integer) gen
        let nx = fst rn
        nx : nextN (abs n - 1) (snd rn)

nextSquare :: Integer -> Integer
nextSquare n = (n + 1) * (n + 1)

-- main function
main :: IO ()
main = do
    putStr "Seed: "
    seed <- getLine
    -- default parameters for now
    let chunks_length = 5       -- map is chunks_length x chunks_length
    let chunk_size = 16          -- each chunk is chunk_size x chunk_size
    -- number of vectors to make
    let num_vectors = nextSquare chunks_length
    -- generate randomly rotated vectors
    let gen = mkStdGen (read seed)
    let rands = nextN (3*num_vectors) gen           -- each pair of nums makes a Vector
    let infl_vect_list = randomsToVectors rands
    -- init map and corner map
    let corner_map = vectors infl_vect_list (chunks_length + 1)

    -- generate one octave of Perlin noise
    let octave1 = octave corner_map chunks_length chunk_size
    -- print octave
    putStrLn $ stringMap octave1
    
    return ()
