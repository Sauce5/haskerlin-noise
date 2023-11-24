import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)

type Vector = (Double,Double) -- that's (x, y) coordinates, not (dir, mag)
type CornerMap = [[Vector]]

type Pixel = Double
type Chunk = [[Pixel]]
type Map = [[Chunk]]

nextN :: Integer -> StdGen -> [Integer]
nextN n gen = case abs n of
    0 -> []
    _ -> do
        let rn = randomR (0::Integer,9::Integer) gen
        let nx = fst rn
        nx : nextN (abs n - 1) (snd rn)

-- main function
main :: IO ()
main = do
    putStr "Seed: "
    seed <- getLine
    let gen = mkStdGen (read seed)
    print (nextN (-10) gen)
