import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)
import Data.List (transpose)

import Display
import Octaves
import NoiseTypes

-- main function
main :: IO ()
main = do
    putStr "Seed: "
    inpSeed <- getLine
    {-
        Error checking on the seed will go here.
    -}
    let seed = read inpSeed
    let noise = fractal seed 8
    putStrLn $ stringMap noise disBasic

    return ()
