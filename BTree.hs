module BTree where

data BTree k v = Empty | Node k v (BTree k v) (BTree k v) deriving (Show)

isEmpty Empty = True
isEmpty _ = False

add :: Ord k => BTree k v -> k -> v -> BTree k v
add Empty k v = Node k v Empty Empty
add (Node nk nv l r) k v | k == nk = Node k v l r
                         | k > nk = Node nk nv l (add r k v)
                         | k < nk = Node nk nv (add l k v) r

get :: Ord k => BTree k v -> k -> Maybe v
get Empty _ = Nothing
get (Node nk nv l r) k | k == nk = Just nv
                       | k > nk = get r k
                       | k < nk = get l k
                       
getSubtree :: Ord k => BTree k v -> k -> BTree k v
getSubtree Empty _ = Empty
getSubtree (Node nk nv l r) k | k == nk = (Node nk nv l r)
                              | k > nk = getSubtree r k
                              | k < nk = getSubtree l k

getLeft :: Ord k => BTree k v -> BTree k v
getLeft (Node _ _ l _) = l

getRight :: Ord k => BTree k v -> BTree k v
getRight (Node _ _ _ r) = r

getKeyValue :: Ord k => BTree k v -> (k, v)
getKeyValue (Node nk nv _ _) = (nk, nv)

remove :: Ord k => BTree k v -> k -> BTree k v
remove Empty _ = Empty
remove (Node nk nv l r) k | k > nk = Node nk nv l (remove r k)
                          | k < nk = Node nk nv (remove l k) r
                          | k == nk = if isEmpty l then r else
                                        if isEmpty r then l else
                                            let
                                                list_kv = getKeyValueList l
                                            in
                                                insertTree r list_kv

insertTree :: Ord k => BTree k v -> [(k, v)] -> BTree k v
insertTree Empty [] = Empty
insertTree Empty (x:xs) = insertTree (Node (fst x) (snd x) Empty Empty) xs
insertTree (Node nk nv l r) [] = (Node nk nv l r)
insertTree (Node nk nv l r) (x:xs) = insertTree (insertTreeHelper (Node nk nv l r) x) xs

insertTreeHelper :: Ord k => BTree k v -> (k, v) -> BTree k v
insertTreeHelper Empty (k, v) = Node k v Empty Empty
insertTreeHelper (Node nk nv l r) (k, v) = add (Node nk nv l r) k v

getKeyValueList :: Ord k => BTree k v -> [(k, v)]
getKeyValueList (Node nk nv l r) = getKeyValueListHelper [] (Node nk nv l r)

getKeyValueListHelper :: Ord k => [(k, v)] -> BTree k v -> [(k, v)]
getKeyValueListHelper acc Empty = acc
getKeyValueListHelper acc (Node nk nv l r) = getKeyValueListHelper (acc ++ [(nk, nv)] ++ getKeyValueListHelper [] l) r