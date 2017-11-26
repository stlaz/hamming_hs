module Main where

import System.IO as IO
import System.Exit ( exitSuccess )
import Data.ByteString.Lazy.Char8 as BIN

import Hamming ( decodeStr )

main = do
    fh <- BIN.readFile "rnd.data"
    let bitstr = BIN.unpack fh
    IO.putStrLn $ decodeStr bitstr
    exitSuccess

