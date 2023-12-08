import Text.Read (readMaybe)

import Display
import Octaves (fractal)

-- main function
{-
    Main function - takes valid seed and generates noise map.
-}
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
