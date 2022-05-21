{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-} -- compiler was whining at `argv !! 0`... -_-
module Main where

{- ARCHITECTURE
1) Parse the command line arguments
2) Decode input image and check that we can process it
3) Compute the pairs of adjacencies
    + Don't forget about the diagonals option
4) Sort the pairs as defined in API
5) Construct the output string
6) Write output string to file

Most of the runtime will be spent in:
    3) Compute the pairs of adjacencies

Most of the development time & code lines are expected to be spent in the rest.
-}

import System.Environment
import Codec.Picture
import Data.Either
import Data.List

main :: IO ()
main = do
    -- # 1) PARSE COMMAND LINE ARGUMENTS #
    putStrLn "Start of CoulAdj-Hs"
    argv <- getArgs
    let argc = length argv
    putStr "argc = "
    print argc
    let settings = parseArgs argc argv
    putStrLn "Arguments parsed without errors."
    print (dontRelateDiagonals settings)
    print (imageFileArg settings)
    print (resultFileArg settings)
    -- # 2) DECODE INPUT IMAGE #
    imgEither <- readImage (imageFileArg settings)
    let imgDyn = either error id imgEither
    let img = convertRGBA8 imgDyn -- TODO: Check & crash if img isn't RGB8 or RGBA8
    putStr "imageWidth = "
    print (imageWidth img)
    putStr "imageHeight = "
    print (imageHeight img)
    let testPixel = PixelRGBA8 10 220 230 249
    let blu = PixelRGBA8  10  20 230 252
    let gre = PixelRGBA8  10 220  30 250
    let cya = PixelRGBA8  10 220 230 248
    let red = PixelRGBA8 210  20  30 246
    let mag = PixelRGBA8 210  20 230 244
    let yel = PixelRGBA8 210 220  30 242
    print testPixel
    let allColours = [
            yel,
            red,
            mag,
            gre,
            cya,
            blu
            ]
    let allColoursSorted = sort allColours
    putStrLn "allColours :"
    putStrLn (unlines (map show allColours))
    putStrLn "allColoursSorted :"
    putStrLn (unlines (map show allColoursSorted))
    let allColourPairs = [(a,b) | a <- allColours, b <- allColours ]
    let allColourPairsSorted = sort allColourPairs
    putStrLn "allColourPairs :"
    putStrLn (unlines (map show allColourPairs))
    putStrLn "allColourPairsSorted :"
    putStrLn (unlines (map show allColourPairsSorted))
    putStrLn "------------------------"
    putStrLn (stringify allColourPairsSorted)
    putStrLn "------------------------"
    let allRowColColours = [
            (0, 0, blu),
            (0, 0, gre),
            (0, 0, cya),
            (0, 0, red),
            (0, 0, mag),
            (0, 0, yel)
            ]
    let allPixels = extractPixels img
    let stringifiedPixels = stringifyPixels allPixels
    writeFile (resultFileArg settings) stringifiedPixels
    putStrLn "End of CoulAdj"


data ProgSettings = ProgSettings
    {   dontRelateDiagonals :: Bool,
        imageFileArg :: String,
        resultFileArg :: String    
    }

parseArgs :: Int -> [String] -> ProgSettings
parseArgs argc argv
    | argc /= 2 && argc /= 3 = error "Error: You must use either 2 or 3 arguments."
    | argc == 3 && head argv /= "--dont-relate-diagonals" = 
        error "Error: With 3 arguments, the first can only be `--dont-relate-diagonals`."
    | argc == 2 = 
        ProgSettings {dontRelateDiagonals = False, imageFileArg = argv !! 0, resultFileArg = argv !! 1}
    | argc == 3 = 
        ProgSettings {dontRelateDiagonals = True, imageFileArg = argv !! 1, resultFileArg = argv !! 2}
    | otherwise = error ("BUG: This error should never have happened. This is the `otherwise` in parseArgs, "
        ++ "and the first guard checked that the number of arguments was 2 or 3, and then other guards"
        ++ "treated both the case for 2 and 3. Or should have, because obviously something went wrong.")


computeAdjacencies :: Image PixelRGBA8 -> [(PixelRGBA8, PixelRGBA8)]
computeAdjacencies = undefined

{-TODO: Write a custom sort
    The default sort that we get from the JuicyPixels package gives us
        exactly the result we want,
    But it would be preferable to implement that behaviour ourselves,
        in case a future update changes that, since we made promises
        in our own API about the sort order.
-}
sortAdjacencies :: [(PixelRGBA8, PixelRGBA8)] -> [(PixelRGBA8, PixelRGBA8)]
sortAdjacencies = sort

{-DO NOT CHANGE THESE HEADERS

These headers are part of the API defined in the Readme.
They MUST NOT be changed unless the major version number is incremented.

The outputted files are meant to be parsed by programs that rely on
   hardcoded column names.

THE NAMES OF THE COLUMNS, AND THE ORDER IN WHICH THEY ARE WRITTEN,
   ARE THE MOST CRITICAL PART OF THE API.

DO NOT CHANGE

DO NOT CHANGE
-}
rgbAlphaHeader :: String
rgbAlphaHeader = intercalate "\t" ["r", "g", "b", "a", "adj_r", "adj_g", "adj_b", "adj_a"]

stringify :: [(PixelRGBA8, PixelRGBA8)] -> String
stringify adjacencies = 
    let
        tsved = [ (rgbAlphaToTsv colour1, rgbAlphaToTsv colour2) | (colour1, colour2) <- adjacencies ]
        allDataLines = [ colour1 ++ "\t" ++ colour2 | (colour1, colour2) <- tsved ]
        allLines = rgbAlphaHeader : allDataLines
        addedNewLines = unlines allLines
    in addedNewLines

rgbAlphaToTsv :: PixelRGBA8 -> String
rgbAlphaToTsv (PixelRGBA8 r g b a) = intercalate "\t" (map show [r, g, b, a])

pixelAtRowCol :: Pixel a => Image a -> Int -> Int -> a
pixelAtRowCol img row col = pixelAt img col row 
-- pixelAt uses (x,y), which is the opposite of our usual (row,col) 
-- https://hackage.haskell.org/package/JuicyPixels-3.3.7/docs/Codec-Picture.html#v:pixelAt

-- For debug
extractPixels :: Image PixelRGBA8 -> [(Int, Int, PixelRGBA8)]
extractPixels img = 
    let
        nbRows = imageHeight img
        nbCols = imageWidth img
        maxRow = nbRows - 1
        maxCol = nbCols - 1
        allRows = [0..maxRow]
        allCols = [0..maxCol]
        allRowCols = [(row,col) | row <- allRows, col <- allCols]
        allPixels = [(row,col, pixelAtRowCol img row col ) | (row,col) <- allRowCols]
    in allPixels

-- For debug
pixelsHeader :: String
pixelsHeader = intercalate "\t" ["row", "col", "r", "g", "b", "a"]

-- For debug
stringifyPixels :: [(Int, Int, PixelRGBA8)] -> String
stringifyPixels pixels = 
    let 
        flattened = [(row, col, r, g, b, a) | (row, col, PixelRGBA8 r g b a) <- pixels ]
        toString = [ [show row, show col, show r, show g, show b, show a] | (row, col, r, g, b, a) <- flattened ]
        addedTabulation = [ intercalate "\t" field | field <- toString]
        allLines = pixelsHeader : addedTabulation
        addedNewLines = unlines allLines
    in addedNewLines