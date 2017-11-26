import Control.Exception ( assert )
import System.Exit ( exitSuccess )
import Hamming

assertEqual a b
    |  a == b = return True
    | otherwise = error $ show a ++ " /= " ++ show b


testM = Matrix [1..20] 4 5
-- FIXME: the \n\t\n is ugly
testMString = "[\n\t1 2 3 4 5 \n\t6 7 8 9 10 \n\t11 12 13 14 15 \n\t16 17 18 19 20 \n\t\n]"

testString = "Slunicko"
testStringBits = [
        [0,1,0,1,0,0,1,1],
        [0,1,1,0,1,1,0,0],
        [0,1,1,1,0,1,0,1],
        [0,1,1,0,1,1,1,0],
        [0,1,1,0,1,0,0,1],
        [0,1,1,0,0,0,1,1],
        [0,1,1,0,1,0,1,1],
        [0,1,1,0,1,1,1,1]
    ]
encodedTestString = "J\134\204x\RSJ\204,\204\&2\204\134\204f\204\254"
-- A string with 1 bit error in every Byte
encodedErrString = "\202\130\236p\USZ\206.\220:\200\166Lb\236\238"

testM1 = Matrix [1..16] 4 4
testM2 = Matrix [17..36] 4 5
multResult = Matrix [
         270,  280,  290,  300,  310,
         662,  688,  714,  740,  766,
        1054, 1096, 1138, 1180, 1222,
        1446, 1504, 1562, 1620, 1678
    ] 4 5

main = do
    assertEqual 1 $ getElem g (3,1)
    assertEqual testMString $ show testM
    assertEqual [11..15] $ getRow testM 2
    assertEqual [4, 9, 14, 19] $ getCol testM 3
    assertEqual [0, 1, 1, 0, 0, 0, 1, 1] $ charToBits 'c'
    assertEqual 'c' $ bitsToChar [0, 1, 1, 0, 0, 0, 1, 1]
    assertEqual '?' $ bitsToChar $ charToBits '?'
    assertEqual testStringBits $ stringToBits testString
    assertEqual multResult $ mmult testM1 testM2
    assertEqual encodedTestString $ encodeStr testString
    assertEqual "coTo_je?" $ decodeStr $ encodeStr "coTo_je?"
    assertEqual testString $ decodeStr encodedErrString

    putStrLn "All tests succeeded"
    exitSuccess
