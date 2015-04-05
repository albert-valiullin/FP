module Maps where

-- map :: (i -> o) -> [i] -> [o]

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- filter :: (a -> Bool) [a] -> [a]
-- concat :: [[a]] -> [a]
-- concatMap :: (a -> [b]) -> [a] -> [b]

map' :: (a -> b) -> [a] -> [b]
map' fun ls = case ls of
				[] -> []
				x : xs -> (fun x) : (map' fun xs)

map1 :: (a -> b) -> [a] -> [b]
-- foldl :: ([b] -> a -> [b]) -> [b] -> [a] -> [b]
map1 fun ls = reverse (foldl f [] ls) where
				f list el = (fun el) : list

map2 :: (a -> b) -> [a] -> [b]
-- foldr :: (a -> [b] -> [b]) -> [b] -> [a] -> [b]
map2 fun ls = foldr f [] ls where
				f el list = (fun el) : list


concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' fun ls = case ls of
						[] -> []
						x : xs -> (fun x) ++ (concatMap' fun xs)

concatMap1 :: (a -> [b]) -> [a] -> [b]
-- foldl :: ([b] -> a -> [b]) -> [b] -> [a] -> [b]
concatMap1 fun ls = reverse (foldl f [] ls) where
				f list el = (reverse (fun el)) ++ list

concatMap2 :: (a -> [b]) -> [a] -> [b]
-- foldr :: (a -> [b] -> [b]) -> [b] -> [a] -> [b]
concatMap2 fun ls = foldr f [] ls where
						f el list = (fun el) ++ list

go :: Int -> [String]
go e = [show e, show (-e)]



