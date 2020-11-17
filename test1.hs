import Data.List
import Data.Char
import Data.Either

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' n m
    | n < m = n : enumFromTo' (n+1) m
    | n == m = [n] -- ou o [m]
    | n > m = []

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' beg by fin
    | (beg <= fin && beg <= by) = beg : enumFromThenTo' by (2*by-beg) fin
    | (beg >= fin && beg >= by) = beg : enumFromThenTo' by (2*by-beg) fin
    | otherwise = [beg]
    
addLists :: [a] -> [a] -> [a]
addLists [] lis = lis
addLists l [] = l
addLists (a:as) lis = [a] ++ addLists as lis 

posInList :: [a] -> Int -> a
posInList [] _ = error "no elements"
posInList (h:t) 0 = h
posInList (h:t) pos = posInList t (pos-1)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h:t) = reverseList t ++ [h]

takeFromList :: Int -> [a] -> [a]
takeFromList _ [] = []
takeFromList 0 lis = []
takeFromList ele (x1:xs) = x1 : takeFromList (ele-1) xs

dropFromList :: Int -> [a] -> [a]
dropFromList _ [] = []
dropFromList 0 lis = lis
dropFromList ele (a1:as) = dropFromList (ele-1) as 

zip' :: [a] -> [b] -> [(a,b)]
zip' [] lis = []
zip' l [] = []
zip' (h:t) (h1:t1) = [(h,h1)] ++ zip' t t1 

elemInList :: Eq a => a -> [a] -> Bool
elemInList _ [] = False
elemInList a (h:t)
  | a == h = True
  | otherwise = elemInList a t

replicateElem :: Int -> a -> [a]
replicateElem 0 _ = []
replicateElem n h = h : replicateElem (n-1) h

betweenElem :: a -> [a] -> [a]
betweenElem _ [] = []
betweenElem _ (x:[]) = [x]
betweenElem n (h:t) = h : n : betweenElem n t

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (h:t) = group_acc [h] h t
  where 
    group_acc rep c [] = [rep]
    group_acc rep c (y:ys)
      | c == y = group_acc (rep ++ [y]) c ys
      | otherwise = rep : group_acc [y] y ys

concat' :: [[a]] -> [a]
concat' [[]] = []
concat' [(x:[])] = [x]
concat' (h:t) = h ++ concat' t 

inits' :: [a] -> [[a]]
inits' [] = []
inits' (h:t) = inits_acc [] h t 
  where 
    inits_acc rep c [] = [rep]
    inits_acc rep c (a:as) = rep : inits_acc [c] a as

tails' :: [a] -> [[a]]
tails' [] = []
tails' (h:t) = tails_acc (h:t) t
  where  
    tails_acc rep [] = [rep,[]]
    tails_acc rep (u:us) = rep : tails_acc (tail rep) us
