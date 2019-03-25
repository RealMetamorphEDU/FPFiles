--import Data.Tuple.Select

--a = (("PI"), 'V', ('H', 'J', 'K'))
--b = ('R', "YU", ('H', "KJ", "KL"))
--c = ('U', "II", "OO", "LL", "PP", (3, 4, 5))
--connect1 = \a b c -> (sel1 a, sel1 b, sel1 c)
--connect2 a b c = (sel1 a, sel3 b, sel6 c)

import Data.Scientific

a = ["PI", "V", "H", "J", "K"]
b = ["R", "YU", "H", "KJ", "KL"]
c = ["U", "II", "OO", "LL", "PP", "3", "4", "5"]

connect1 = \a b c -> (head a, head b, head c)

connect2:: [a] -> [a] -> [a] -> [a]
connect2 a b c = [a!!1, b!!3, c!!6]


getBy f a [] = a
getBy f a (b:xs) = let r = f a b
                    in getBy f r xs


f a b c d e = let minV = getBy min a [b,c,d,e]
                  maxV = getBy max a [b,c,d,e]
              in 
                  if isInteger minV && isInteger maxV then maxV:minV:[] else [(maxV + minV) / 2]