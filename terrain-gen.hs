import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)
import Data.List (transpose)

import Display
import Octaves
import NoiseTypes

stringOctaves :: [Octave] -> String
stringOctaves [] = ""
stringOctaves (o:os) = stringMap o ++ "---------\n" ++ stringOctaves os

app :: Double -> [Double] -> [Double]
app p [] = []
app p (d:ds) = (p * d) : app (p * p) ds

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
    let octave1 = genOctave seed 2 4
    -- print octave
    --putStrLn $ stringMap octave1
    
    -- generate 3 octaves
    -- for now, stick with lacunarity = 2 and even chunk sizes
    -- !!! don't use a chunk size that isn't perfectly divisible by the lacunarity !!!
    let octaves = genSeries seed 8 8 8 2
    --putStrLn $ stringOctaves octaves
    -- apply persistence to octaves
    let pers_oct = applyPersistance 0.5 octaves
    let added = addOctaves pers_oct
    putStrLn $ stringMap added

    return ()
