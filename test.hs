main = do
file <- readFile "test.hs"
writeFile "testOUT.txt" (map toUpper file)