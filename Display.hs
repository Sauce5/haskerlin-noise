module Display
( stringMap
, disBasic
) where

import Text.Printf
import NoiseTypes

stringMap :: Map -> (Pixel -> Double -> Double -> Char) -> String
stringMap m f = mapToString m min range f
    where min = minMap m
          range = maxMap m - minMap m

mapToString :: Map -> Double -> Double -> (Pixel -> Double -> Double -> Char) -> String
mapToString [] mn rg f = ""
mapToString (r:rs) mn rg f = (rowToString r mn rg f ++ "\n") ++ mapToString rs mn rg f

rowToString :: [Pixel] -> Double -> Double -> (Pixel -> Double -> Double -> Char) -> String
rowToString [] mn rg f = ""
rowToString (p:ps) mn rg f = (pixelToString p mn rg f ++ " ") ++ rowToString ps mn rg f

pixelToString :: Pixel -> Double -> Double -> (Pixel -> Double -> Double -> Char) -> String
-- pixelToString p _ _ = printf "%.5v" (show (pix p))
pixelToString p mn rg f= [f p mn rg]

minMap :: Map -> Double
minMap [] = error "Cannot find minimum: empty map"
minMap [r] = minRow r
minMap (r:s:rs) = if minRow r < minRow s then minMap (r:rs) else minMap (s:rs)

minRow :: [Pixel] -> Double
minRow [] = error "Cannot find minimum: empty row"
minRow [p] = pix p
minRow (p:q:ps) = if pix p < pix q then minRow (p:ps) else minRow (q:ps)

maxMap :: Map -> Double
maxMap [] = error "Cannot find maximum: empty map"
maxMap [r] = maxRow r
maxMap (r:s:rs) = if maxRow r > maxRow s then maxMap (r:rs) else maxMap (s:rs)

maxRow :: [Pixel] -> Double
maxRow [] = error "Cannot find maximum: empty row"
maxRow [p] = pix p
maxRow (p:q:ps) = if pix p > pix q then maxRow (p:ps) else maxRow (q:ps)

{-
    Display Methods
-}
disBasic :: Pixel -> Double -> Double -> Char
disBasic p mn rg
    | pix p <= (mn + (0.1*rg)) = '.'
    | pix p <= (mn + (0.2*rg)) = ':'
    | pix p <= (mn + (0.3*rg)) = '+'
    | pix p <= (mn + (0.4*rg)) = '='
    | pix p <= (mn + (0.5*rg)) = '?'
    | pix p <= (mn + (0.6*rg)) = '#'
    | pix p <= (mn + (0.7*rg)) = '&'
    | pix p <= (mn + (0.8*rg)) = '$'
    | pix p <= (mn + (0.9*rg)) = '%'
    | pix p <= (mn + (1.0*rg)) = '@'
    | otherwise = '@'    -- for floating point errors (pix p > 1)
