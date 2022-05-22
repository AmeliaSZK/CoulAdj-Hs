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
    --writeFile (resultFileArg settings) stringifiedPixels
    -- # 3) COMPUTE ADJACENCIES #
    let unsortedAdjacencies = computeAdjacencies img (dontRelateDiagonals settings)
    -- # 4) SORT ADJACENCIES #
    let adjacencies = sortAdjacencies unsortedAdjacencies
    -- # 5) STRINGIFY #
    let stringified = stringify adjacencies
    -- # 6) WRITE TO FILE #
    writeFile (resultFileArg settings) stringified
    -- START OF DEBUG
    let dontDiags = dontRelateDiagonals settings
    let relateDiagonals = not dontDiags
    let nbRows = imageHeight img
    let nbCols = imageWidth img
    let maxRow = nbRows - 1
    let maxCol = nbCols - 1
    let allRows = [0..maxRow]
    let allCols = [0..maxCol]
    let allRowCols = [(row,col) | row <- allRows, col <- allCols]
    let rowColIsInBounds (row,col) = 
            0 <= row && row <= maxRow &&
            0 <= col && col <= maxCol
    let evaledR3C3 = evalOnePixel img 3 3 dontDiags rowColIsInBounds
    putStrLn (stringify (sortAdjacencies evaledR3C3))
    let midCent = (3,3)
    let topLeft = neighRowColFromOffsets midCent (-1, -1)
    let topCent = neighRowColFromOffsets midCent (-1,  0)
    let topRigh = neighRowColFromOffsets midCent (-1,  1)
    let midLeft = neighRowColFromOffsets midCent ( 0, -1)
    let midRigh = neighRowColFromOffsets midCent ( 0,  1)
    let botLeft = neighRowColFromOffsets midCent ( 1, -1)
    let botCent = neighRowColFromOffsets midCent ( 1,  0)
    let botRigh = neighRowColFromOffsets midCent ( 1,  1)
    print midCent 
    print topLeft 
    print topCent 
    print topRigh 
    print midLeft 
    print midRigh 
    print botLeft 
    print botCent 
    print botRigh 
    let allNeighOffsets = 
            if relateDiagonals then
                [midRigh, botLeft, botCent, botRigh]
            else
                [midRigh,          botCent]
    print allNeighOffsets
    let calculatedNeighRowCols = map (neighRowColFromOffsets midCent) allNeighOffsets
    print calculatedNeighRowCols
    -- END OF DEBUG
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


computeAdjacencies :: Image PixelRGBA8 -> Bool -> [(PixelRGBA8, PixelRGBA8)]
computeAdjacencies img dontRelateDiagonals = 
    let
        nbRows = imageHeight img
        nbCols = imageWidth img
        maxRow = nbRows - 1
        maxCol = nbCols - 1
        allRows = [0..maxRow]
        allCols = [0..maxCol]
        allRowCols = [(row,col) | row <- allRows, col <- allCols]
        rowColIsInBounds (row,col) = 
            0 <= row && row <= maxRow &&
            0 <= col && col <= maxCol
        evaluatedAllPixels = [ evalOnePixel img row col dontRelateDiagonals rowColIsInBounds 
                                | (row, col) <- allRowCols ]
        allAdjacencies = nub (concat evaluatedAllPixels)
    in allAdjacencies

evalOnePixel :: Image PixelRGBA8 -> Int -> Int -> Bool -> ((Int, Int) -> Bool) -> [(PixelRGBA8, PixelRGBA8)]
evalOnePixel img row col dontRelateDiagonals rowColIsInBounds = 
    let
        relateDiagonals = not dontRelateDiagonals
        {-The API option is named "dont...", because years ago, we thought
            it was a good idea to make all options have a default value
            of False, and we wanted the diagonals to be related by default.
        And now, all our other implemations of CoulAdj have been using that
            name, so we're keeping it.
        Funny story: This project was (and still is, lol) intended to parse
            geographic maps from Total War video games, to know which
            regions the game considered adjacent to each others.
            At the time, we had no way to know if the game was using the
                diagonals, because to test it, you'd need 2 regions that would
                "touch" by only a corner of a pixel.
            Anywayz, when an update finally released a map where we had such
                a situation (in Three Kingdoms), it turned out that the game
                does NOT considers the diagonals, lol woops -}

        midCent = (row,col) -- Also called the "hotspot"

        topLeft = neighRowColFromOffsets midCent (-1, -1)
        topCent = neighRowColFromOffsets midCent (-1,  0)
        topRigh = neighRowColFromOffsets midCent (-1,  1)
        midLeft = neighRowColFromOffsets midCent ( 0, -1)
        midRigh = neighRowColFromOffsets midCent ( 0,  1)
        botLeft = neighRowColFromOffsets midCent ( 1, -1)
        botCent = neighRowColFromOffsets midCent ( 1,  0)
        botRigh = neighRowColFromOffsets midCent ( 1,  1)

        calculatedNeighRowCols = 
            if relateDiagonals then
                [midRigh, botLeft, botCent, botRigh]
            else
                [midRigh,          botCent]
        
        --calculatedNeighRowCols = map (neighRowColFromOffsets midCent) allNeighOffsets
        allNeighRowCols = filter rowColIsInBounds calculatedNeighRowCols
        hotspotPixel  = [pixelAtRowCol img r c | (r,c) <- [midCent]]
        neighbrPixels = [pixelAtRowCol img r c | (r,c) <- allNeighRowCols]
        allAdjacencies = [ (hotspot, neighbr) | hotspot <- hotspotPixel, neighbr <- neighbrPixels ]
        removedSameColours = [(a, b) | (a,b) <- allAdjacencies, a /= b]
        removedDuplicates = nub removedSameColours
        addedSymmetricals = concat [ [(a,b),(b,a)] | (a,b) <- removedDuplicates ]
    in addedSymmetricals

neighRowColFromOffsets :: (Int, Int) -> (Int, Int) -> (Int, Int)
neighRowColFromOffsets (row,col) (rowOffset,colOffset) =
    (row+rowOffset, col+colOffset)

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