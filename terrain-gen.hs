import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)
import Data.List (transpose)

import NoiseTypes
import MapInit
import Gradients
import Display
import Smoothing

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
    let chunks_length = 5       -- map is chunks_length x chunks_length
    let chunk_size = 16          -- each chunk is chunk_size x chunk_size
    -- number of vectors to make
    let num_vectors = nextSquare chunks_length
    -- generate randomly rotated vectors
    let gen = mkStdGen (read seed)
    let rands = nextN (2*num_vectors) gen           -- each pair of nums makes a Vector
    let infl_vect_list = randomsToVectors rands
    -- init map and corner map
    let corner_map = vectors infl_vect_list (chunks_length + 1)
    {-
        Code below might need to be looped for each octave
        For now, the code will generate one octave
    -}
    -- fill 4 maps with dot products from each chunk's 4 influence vectors
    let ulMap = gradients corner_map (pixels chunks_length chunk_size) chunk_size (0,0)
    let urMap = gradients corner_map (pixels chunks_length chunk_size) chunk_size (0,1)
    let llMap = gradients corner_map (pixels chunks_length chunk_size) chunk_size (1,0)
    let lrMap = gradients corner_map (pixels chunks_length chunk_size) chunk_size (1,1)
    -- print all 4 maps
    -- putStrLn $ stringMap ulMap
    -- putStrLn $ stringMap urMap
    -- putStrLn "\nSMOOTHED:\n"
    -- print smooth map between ulMap and urMap
    let ulurMap = blend ulMap urMap chunk_size
    let lllrMap = blend llMap lrMap chunk_size
    -- putStrLn $ stringMap ulurMap
    -- putStrLn $ stringMap lllrMap
    let blendedMap = transpose $ blend (transpose ulurMap) (transpose lllrMap) chunk_size
    putStrLn $ stringMap blendedMap
    return ()
