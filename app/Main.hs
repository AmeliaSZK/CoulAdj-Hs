module Main where

import System.Environment

main :: IO ()
main = do
    putStrLn "Start of CoulAdj-Hs"
    argv <- getArgs
    let argc = length argv
    putStr "argc = "
    print argc

