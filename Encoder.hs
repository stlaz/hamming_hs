module Main where

import System.IO as IO
import System.Environment ( getArgs )
import System.Exit ( exitSuccess )
import Data.ByteString.Lazy.Char8 as BIN

import Hamming ( encodeStr, decodeStr )

getUserStr [] = ""
getUserStr [x] = x
getUserStr (x:xs) =  x ++ " " ++ getUserStr xs

main = do
    args <- getArgs
    BIN.writeFile "encoded.out" $ BIN.pack $ encodeStr $ getUserStr args
    exitSuccess

