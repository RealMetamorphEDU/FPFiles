root a b c = let d_v = b ^ 2 - 4 * a * c
             in
                 if d_v > 0 then [((-b) + sqrt d_v) / (2 * a), ((-b) - sqrt d_v) / (2 * a)] else
                 if d_v == 0 then [(-b) / (2 * a), (-b) / (2 * a)] else [(-b)/(2 * a), sqrt (-d_v) / (2 * a), (-(sqrt (-d_v) / (2 * a)))]

print_root rs = if length rs == 2 then "x1 = " ++ show (rs!!0) ++ ", x2 = " ++ show (rs!!1) 
                else "x1 = " ++ show (rs!!0) ++ (if signum (rs!!1) == 1 then "+" else "-") ++ show (abs (rs!!1)) ++ "i, x2 = " ++ show (rs!!0) ++ (if signum (rs!!2) == 1 then "+" else "-") ++ show (abs (rs!!2)) ++ "i"