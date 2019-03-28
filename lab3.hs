f_Eq :: Eq a => a -> a -> Bool
f_Eq a b = (a == b)

f_Ord :: Ord a => a -> a -> Bool
f_Ord a b = (a > b)

f_Show :: Show a => a -> [Char]
f_Show a = show a

f_Read :: (Read a, Num a) => [Char] -> a
f_Read a = read a * 2

f_Enum :: Enum a => a -> a -> [a]
f_Enum a b = [a .. b] ++ [succ a, pred b]

f_Bounded :: Bounded a => a
f_Bounded = minBound

f_Num :: (Num a, Show a) => a -> [Char]
f_Num a = show a

f_Integral :: Integral a => a -> a
f_Integral a = a ^ 2

f_Floating :: Floating a => a -> a
f_Floating a = a * 4

--replicate 3 7

--take 3 []   
--take 3 [2,3,5,6,7]

--reverse [1,2,3,4,5]

--repeat 3
--take 4 (repeat 3)

--zip [1,2,3] ['a','b']
--zip [1,2,3] ['a','b','c']

--elem 3 [2,2,1]
--elem 3 [2,3,1]