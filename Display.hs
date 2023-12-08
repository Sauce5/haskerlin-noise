module Display
( stringMap
, disBasic
, disIslands
, disNumbers
, disBasicAlt
) where

import Text.Printf
import NoiseTypes

stringMap :: Map -> Displayer -> String
stringMap m f = mapToString m min range f
    where min = minMap m
          range = maxMap m - minMap m

mapToString :: Map -> Double -> Double -> Displayer -> String
mapToString [] mn rg f = ""
mapToString (r:rs) mn rg f = (rowToString r mn rg f ++ "\n") ++ mapToString rs mn rg f

rowToString :: [Pixel] -> Double -> Double -> Displayer -> String
rowToString [] mn rg f = ""
rowToString (p:ps) mn rg f = (pixelToString p mn rg f ++ " ") ++ rowToString ps mn rg f

pixelToString :: Pixel -> Double -> Double -> Displayer -> String
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

type Displayer = Pixel -> Double -> Double -> Char

{-
    Displays basic topographical map
-}
disBasic :: Displayer
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

{-
    Displays basic topographical map (alternative)
-}
disBasicAlt :: Displayer
disBasicAlt p mn rg
    | pix p <= (mn + (0.125*rg)) = '.'
    | pix p <= (mn + (0.25*rg)) = ','
    | pix p <= (mn + (0.375*rg)) = ':'
    | pix p <= (mn + (0.5*rg)) = ':'
    | pix p <= (mn + (0.625*rg)) = 'i'
    | pix p <= (mn + (0.75*rg)) = 'l'
    | pix p <= (mn + (0.875*rg)) = 'w'
    | pix p <= (mn + (1.0*rg)) = 'W'
    | otherwise = 'W'    -- for floating point errors (pix p > 1)

{-
    Displays an islands map
-}
disIslands :: Displayer
disIslands p mn rg
    | pix p <= (mn + (0.65*rg)) = ' '
    | otherwise                 = '@'

{-
    Displays heights with arbitrary numbers
-}
disNumbers :: Displayer
disNumbers p mn rg
    | pix p <= (mn + (0.1*rg)) = '0'
    | pix p <= (mn + (0.2*rg)) = '1'
    | pix p <= (mn + (0.3*rg)) = '2'
    | pix p <= (mn + (0.4*rg)) = '3'
    | pix p <= (mn + (0.5*rg)) = '4'
    | pix p <= (mn + (0.6*rg)) = '5'
    | pix p <= (mn + (0.7*rg)) = '6'
    | pix p <= (mn + (0.8*rg)) = '7'
    | pix p <= (mn + (0.9*rg)) = '8'
    | pix p <= (mn + (1.0*rg)) = '9'
    | otherwise = '9'    -- for floating point errors (pix p > 1)
