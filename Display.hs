module Display
( stringMap
) where

import Text.Printf
import NoiseTypes

stringMap :: Map -> String
stringMap m = mapToString m min range
    where min = minMap m
          range = maxMap m - minMap m

mapToString :: Map -> Double -> Double -> String
mapToString [] mn rg = ""
mapToString (r:rs) mn rg = (rowToString r mn rg ++ "\n") ++ mapToString rs mn rg

rowToString :: [Pixel] -> Double -> Double -> String
rowToString [] mn rg = ""
rowToString (p:ps) mn rg = (pixelToString p mn rg ++ " ") ++ rowToString ps mn rg

pixelToString :: Pixel -> Double -> Double -> String
-- pixelToString p _ _ = printf "%.5v" (show (pix p))
pixelToString p mn rg = [pixelToASCII p mn rg]

pixelToASCII :: Pixel -> Double -> Double -> Char
pixelToASCII p mn rg
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
    | otherwise = error "pixelToASCII: pixel math error"

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
