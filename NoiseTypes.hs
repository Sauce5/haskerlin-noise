module NoiseTypes where

type CornerMap = [[Vector]]
data Vector = Vector {
    vecX :: Int,
    vecY :: Int,
    vec  :: (Double,Double)
} deriving Show

type Map = [[Pixel]]
data Pixel = Pixel {
    pixX :: Int,
    pixY :: Int,
    pix  :: Double
} deriving Show

newVector :: Int -> Int -> (Double,Double) -> Vector
newVector x y v = Vector { vecX = x, vecY = y, vec = v }

newPixel :: Int -> Int -> Double -> Pixel
newPixel x y p = Pixel { pixX = x, pixY = y, pix = p }

bindMap :: [[a]] -> (a -> a) -> [[a]]
bindMap ll f = ll >>= (\l -> [map f l])
