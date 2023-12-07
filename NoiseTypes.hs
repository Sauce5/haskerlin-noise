module NoiseTypes where

type CornerMap = [[Vector]]
data Vector = Vector {
    vecX :: Int,            -- row
    vecY :: Int,            -- col
    vec  :: (Double,Double) -- vector data
} deriving Show

type Map = [[Pixel]]
data Pixel = Pixel {
    pixX :: Int,            -- row
    pixY :: Int,            -- col
    pix  :: Double          -- pixel data
} deriving Show

type Octave = Map

{-
    Wrapper for creating a new vector.
-}
newVector :: Int -> Int -> (Double,Double) -> Vector
newVector x y v = Vector { vecX = x, vecY = y, vec = v }

{-
    Wrapper for creating a new pixel.
-}
newPixel :: Int -> Int -> Double -> Pixel
newPixel x y p = Pixel { pixX = x, pixY = y, pix = p }

{-
    Wrapper for applying (a -> a) to [[a]].
-}
bindMap :: [[a]] -> (a -> a) -> [[a]]
bindMap ll f = ll >>= (\l -> [map f l])
