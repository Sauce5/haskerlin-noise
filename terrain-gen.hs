import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)

import NoiseTypes
import MapInit
import Gradients

nextN :: Integer -> StdGen -> [Integer]
nextN n gen = case abs n of
    0 -> []
    _ -> do
        let rn = randomR (1::Integer,99::Integer) gen
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
    let chunks_length = 3       -- map is chunks_length x chunks_length (16 chunks total)
    let chunk_size = 2         -- this means each chunk is of size 16x16 pixels
    -- number of vectors to make
    let num_vectors = nextSquare chunks_length
    -- generate randomly rotated vectors
    let gen = mkStdGen (read seed)
    let rands = nextN (2*num_vectors) gen           -- each pair of nums makes a Vector
    let infl_vect_list = randomsToVectors rands
    -- init map and corner map
    let corner_map = assignVectors infl_vect_list chunk_size
    {-
        Code below might need to be looped for each octave
        For now, the code will generate one octave
    -}
    -- fill 4 maps with dot products from each chunk's 4 influence vectors
    let ulMap = assignGradients corner_map (0,0) (genEmptyMap chunks_length chunk_size)
    let llMap = assignGradients corner_map (0,1) (genEmptyMap chunks_length chunk_size)
    let urMap = assignGradients corner_map (1,0) (genEmptyMap chunks_length chunk_size)
    let lrMap = assignGradients corner_map (1,1) (genEmptyMap chunks_length chunk_size)
    return ()
