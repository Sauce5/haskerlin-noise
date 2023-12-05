module Display
( mapToString
) where

import Text.Printf
import NoiseTypes

mapToString :: Map -> String
mapToString [] = ""
mapToString (r:rs) = (rowToString r ++ "\n") ++ mapToString rs

rowToString :: [Pixel] -> String
rowToString [] = ""
rowToString (p:ps) = (pixelToString p ++ "\t") ++ rowToString ps

pixelToString :: Pixel -> String
pixelToString p = printf "%.5v" (show (pix p))
