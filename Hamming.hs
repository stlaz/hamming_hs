{-
 - file: hamming.hs
 - author: Stanislav Laznicka
 -}
module Hamming where

import System.Exit ( exitSuccess )

import Data.Bits ( testBit )
import Data.Char ( ord, chr )

-- Matrix vals rows cols
data Matrix = Matrix [Integer] Integer Integer
                deriving Eq

getMatrixList (Matrix vals _ _) = vals

-- Hamming code generator matrix
g :: Matrix
g =  Matrix [
        1, 1, 0, 1,
        1, 0, 1, 1,
        1, 0, 0, 0,
        0, 1, 1, 1,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    ] 7 4

-- Hamming code parity check matrix
h :: Matrix
h = Matrix [
        1, 0, 1, 0, 1, 0, 1,
        0, 1, 1, 0, 0, 1, 1,
        0, 0, 0, 1, 1, 1, 1
    ] 3 7


-- Get element in a matrix
getElem :: Matrix -> (Integer, Integer) -> Integer
getElem (Matrix v r c) (x,y) = v !! fromIntegral (x * c + y)


-- Get a column of a matrix
getCol :: Matrix -> Integer -> [Integer]
getCol mt colnum = getCol' mt colnum 0
    where
        getCol' mt@(Matrix lst rows cols) colnum rownum
            | rownum == rows = []
            | otherwise = getElem mt (rownum, colnum) :
                            getCol' mt colnum (rownum + 1)


-- Get a row of a matrix
getRow :: Matrix -> Integer -> [Integer]
getRow (Matrix lst _ cols) rownum =
        take (fromInteger cols) $ drop (fromInteger (rownum * cols)) lst


-- Display the matrix by its columns and rows
-- FIXME: ends with \n\t\n
instance Show Matrix where
    show mt@(Matrix lst rows cols) = "[\n\t" ++ showRows mt 0 0 ++ "]"
            where
                showRows :: Matrix -> Integer -> Integer -> String
                showRows mt@(Matrix lst rows cols) rnum cnum
                    | cnum == cols = "\n\t" ++ showRows mt (rnum + 1) 0
                    | rnum < rows = show (getElem mt (rnum, cnum)) ++ " "
                                    ++ showRows mt rnum (cnum + 1)
                    | otherwise = "\n"


-- Matrix multiplication function
mmult :: Matrix -> Matrix -> Matrix
-- mmult [] _ = []
mmult mata@(Matrix lsta rowsa colsa) matb@(Matrix lstb rowsb colsb)
    | not (canMultiply mata matb) =
        error "Cannot multiply matrices with such dimensions"
    | otherwise = Matrix (countRows mata matb 0) rowsa colsb
    where
        countRows :: Matrix -> Matrix -> Integer -> [Integer]
        countRows mata@(Matrix _ rowsa _) matb n
            | n < rowsa = countRow (getRow mata n) matb 0 ++ countRows mata matb (n+1)
            | otherwise = []
        countRow :: [Integer] -> Matrix -> Integer -> [Integer]
        countRow row mat@(Matrix _ _ cols) colnum
            | colnum < cols = countElem row (getCol mat colnum) :
                                countRow row mat (colnum + 1)
            | otherwise =  []
        countElem :: Integral a => [a] -> [a] -> a
        countElem row col = sum $ zipWith (*) row col
        -- can only multiply if cols of A equals rows of B
        canMultiply (Matrix _ ra ca) (Matrix _ rb cb) = ca == rb


-- Matrix multiplication cropping the data to bits
mmultBits :: Matrix -> Matrix -> Matrix
mmultBits mata matb = matMod $ mmult mata matb
    where
        matMod (Matrix vals rows cols) =
            Matrix (map (`mod` 2) vals) rows cols


-- Convert each char of a list to bits
stringToBits :: String -> [[Integer]]
stringToBits = map charToBits


-- Convert a character to list of bits
charToBits :: Char -> [Integer]
charToBits a = conv a 0
    where
        conv a 8 = []
        conv a i = conv a (i+1) ++ [ boolToInt $ testBit (ord a) i ]

-- Convert a boolean value to 1 / 0
boolToInt b
    | b         = 1
    | otherwise = 0


-- Applies the Hamming generator matrix to the input string
encodeStr a = matListToStr $ map (mmultBits g) $ prepData a
    where
        -- append a 0 at the end of each encoded half-byte to get 8 bits
        matListToStr [] = []
        matListToStr (Matrix vals _ _ : xs) =
            bitsToChar (vals ++ [0]) : matListToStr xs
        prepData a = listToMatrixList $ splitToHalves a
        listToMatrixList = map (\x -> Matrix x 4 1)
        splitToHalves ll =
            concatMap ((\(a,b) -> [a,b]) . splitAt 4) $ stringToBits ll

-- Convert list of 1 / 0s to an Integer
bitsToInt :: [Integer] -> Integer
bitsToInt [] = 0
bitsToInt (b:bs) = b * 2 ^ length bs + bitsToInt bs

-- Convert list of 1 / 0s to a Char
bitsToChar bs = chr $ fromIntegral $ bitsToInt bs

-- Split a list to multiple lists of n elements
splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy n ls = take n ls : splitBy n (drop n ls)


-- Decode the input string to the original string, correcting errors
decodeStr a = bitsToStr $ map correctAndGetData $ prepData a
    where
        bitsToStr :: [[Integer]] -> String
        bitsToStr [] = []
        bitsToStr bs = bitsToChar (concat $ take 2 bs) : bitsToStr (drop 2 bs)
        correctAndGetData (b,s) = getDataBits $ fixBits (bitsToInt s) b
        getDataBits bs = [bs !! 2, bs !! 4, bs !! 5, bs !! 6]
        -- FIXME: untested!
        fixBits :: Integer -> [Integer] -> [Integer]
        fixBits 0 bs = bs
        fixBits 1 (b:bs) =
            if b == 1 then 0:bs
                      else 1:bs
        fixBits s (b:bs) = b:fixBits (s-1) bs
        prepData d = addSyndromes $ listToMatrixList $ stringToBits d
        addSyndromes [] = []
        addSyndromes (m@(Matrix vals _ _):ms) =
            (vals, countSyndrome m) : addSyndromes ms
        listToMatrixList = map (\x -> Matrix (take 7 x) 7 1)


-- Count the syndrome and reverse it so that we can convert it to an index
countSyndrome :: Matrix -> [Integer]
countSyndrome m = reverse $ getMatrixList $ mmultBits h m
