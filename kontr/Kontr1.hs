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
data Tree a = Nil | Tip a | Bin (Tree a) (Tree a) deriving (Show, Eq, Ord)
tips :: [a] -> Tree a
tips [] = Nil
tips (x:zs) = case zs of 
                [] -> Tip x
                _ -> Bin (tips (fst sp)) (tips (snd sp)) where
                        sp = halve (x:zs)

halve :: [a] -> ([a], [a])
halve x = (take half x, drop half x) where
                half :: Int
                half = div (length x) 2


--5
data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

addit :: Nat -> Nat -> Nat
addit a Zero = a
addit a (Succ c) = addit (Succ a) c

subst :: Nat -> Nat -> Nat
subst Zero b = Zero
subst a Zero = a
subst (Succ c) (Succ d) = subst c d

mult :: Nat -> Nat -> Nat
mult a Zero = Zero
mult Zero b = Zero
mult c d = go Zero c d where
                go r a Zero = r
                go r a (Succ b) = go (addit r a) a b
