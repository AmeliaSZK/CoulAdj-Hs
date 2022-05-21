{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-} -- compiler was whining at `argv !! 0`... -_-
module Main where

import System.Environment
import Codec.Picture
import Data.Either

main :: IO ()
main = do
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
    imgEither <- readImage (imageFileArg settings)
    let imgDyn = either error id imgEither
    let img = convertRGBA8 imgDyn -- TODO: Check & crash if img isn't RGB8 or RGBA8
    putStr "imageWidth = "
    print (imageWidth img)
    putStr "imageHeight = "
    print (imageHeight img)
    writeFile (resultFileArg settings) "Hi! :D"

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