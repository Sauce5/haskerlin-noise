import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)
import Data.List (transpose)
import Text.Read (readMaybe)

import Display
import Octaves

-- main function
main :: IO ()
main = do
    putStr "Seed: "
    inpSeed <- getLine
    case readMaybe inpSeed :: Maybe Int of
        Nothing -> main
        Just s -> do
            let seed = read inpSeed
            let noise = fractal seed 4
            putStrLn $ stringMap noise disIslands
            return ()
