import Data.Char
import Data.List
import System.Environment

helper :: Integer -> Integer -> [String] -> Bool
helper acc1 acc2 [] = acc1 == acc2
helper acc1 acc2 (x:xs) = let
                            typl = break (== ' ') x
                            comm = fst typl
                            val = read $ snd typl :: Integer
                          in
                            if comm == "inc" then helper (acc1 + val) acc2 xs else helper acc1 (acc2 + val) xs

main = do
[filename] <- getArgs
file <- readFile filename
let line_arr = lines file
let result = helper 0 0 line_arr
putStrLn $ show result