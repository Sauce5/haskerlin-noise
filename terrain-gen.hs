import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)
import Data.List (transpose)

import Display
import Octaves

-- main function
main :: IO ()
main = do
    putStr "Seed: "
    inpSeed <- getLine
    {-
        Error checking on the seed will go here.
    -}
    let seed = read inpSeed
    -- generate one octave of Perlin noise
    let octave1 = genOctave seed 5 16
    -- print octave
    putStrLn $ stringMap octave1
    
    return ()
