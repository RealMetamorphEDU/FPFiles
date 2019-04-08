import Data.Char
transform :: String -> String
transform str = map toUpper str

main = do
file <- readFile "test2.hs"
let str_lst = lines file
let up_lst = map transform str_lst
writeFile "test2OUT.txt" (unlines up_lst)