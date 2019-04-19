import Data.Char
import Data.List
import System.Environment


sumFiles :: [String] -> String -> String
sumFiles [] acc = acc
sumFiles (x:xs) acc = sumFiles xs (acc ++ x)

main = do
    lst <- getArgs
    let files = mapM (readFile) lst
    files_srt <- files
    let file = sumFiles files_srt []
    writeFile "summ.txt" file