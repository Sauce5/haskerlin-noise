module NoiseTypes where

type Vector = (Double,Double) -- that's (x, y) coordinates, not (dir, mag)
type CornerMap = [[Vector]]
type Pixel = Double
type Map = [[Pixel]]
