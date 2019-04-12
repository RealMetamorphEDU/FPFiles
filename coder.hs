import Data.Char
import Data.List
import System.Environment



genStrHelper sym1 sym2 acc | sym1 == sym2 = acc ++ [sym1]
                           | sym1 > 'z' = genStrHelper (' ') sym2 (acc ++ ['!'])
                           | True = genStrHelper (succ sym1) sym2 (acc ++ [sym1]) 
genStr sym = genStrHelper (succ sym) sym []

genTableHelper acc [] = acc
genTableHelper acc (x:xs) = genTableHelper (acc ++ [genStr x]) xs

genTable pass = genTableHelper [genStrHelper ' ' 'z' []] pass


getEncodedSym table num sym = let 
                                 index = elemIndex sym (table!!0)
                              in
                                 (table!!num)!!(case index of 
                                    Just a -> a
                                    Nothing -> 0
                                 )
getDecodedSym table num sym = let 
                                 index = elemIndex sym (table!!num)
                              in
                                 (table!!0)!!(case index of 
                                    Just a -> a
                                    Nothing -> 0
                                 )

getEncodedText acc ind len [] table = acc 
getEncodedText acc ind len (x:xs) table | ind > len = getEncodedText (acc ++ [getEncodedSym table 1 x]) (2) len xs table
                                        | True = getEncodedText (acc ++ [getEncodedSym table ind x]) (ind + 1) len xs table

getDecodedText acc ind len [] table = acc 
getDecodedText acc ind len (x:xs) table | ind > len = getDecodedText (acc ++ [getDecodedSym table 1 x]) (2) len xs table
                                        | True = getDecodedText (acc ++ [getDecodedSym table ind x]) (ind + 1) len xs table

encode pass text = let
                      table = genTable pass
                      len = length pass
                   in
                      getEncodedText [] 1 len text table




decode pass text = let
                      table = genTable pass
                      len = length pass
                   in
                      getDecodedText [] 1 len text table


main = do 
    [input, password, output, method] <- getArgs
    file_input <- readFile input
    if method == "encode" then do
        let file_ouput = encode password file_input
        writeFile output file_ouput
     else do
    if method == "decode" then do
        let file_ouput = decode password file_input
        writeFile output file_ouput
     else do
        putStrLn "invalid method, use 'encode' or 'decode'"
        return ()
