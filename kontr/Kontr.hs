module Kontr where

--1
getEl :: [a] -> Int -> a
getEl (e:ls) 0 = e
getEl (e:ls) i = getEl ls (i-1)


--2
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' fun (e:ls) = if (fun e)
                            then e : (takeWhile' fun ls)
                            else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' fun (e:ls) = if (fun e)
                            then dropWhile' fun ls
                            else e:ls



--3
interval3 :: Int -> Int -> Int -> [Int]
interval3 a b c = go (b-a) [] a c where
                    go st ls h t = if ((abs h) < (abs t))
                                    then h : (go st ls (h+st) c)
                                    else if ((abs h) > (abs t))
                                            then ls
                                            else h : ls


--4
data Tree a = Tip a | Bin (Tree a) (Tree a) deriving (Show)
tips :: [a] -> Tree a
tips (x:zs) = case zs of 
                [] -> Tip x
                _ -> undefined
                -- _ -> Bin (tips (takeWhile ()))