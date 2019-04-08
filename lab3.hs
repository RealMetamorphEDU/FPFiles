
import Data.Scientific


f a = map helper a

helper x = if isInteger x then fromInteger ((read (take (length (show x) - 2) (show x)) :: Integer) `mod` 2) else x


--f a = helper [] a
--helper acc [] = acc
--helper acc (x:xs) = if isInteger x then helper (acc ++ [fromInteger ((read (take (length (show x) - 2) (show x)) :: Integer) `mod` 2)]) xs else helper (acc ++ [x]) xs