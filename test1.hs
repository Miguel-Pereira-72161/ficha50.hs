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

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] lis = True
isPrefixOf' l [] = False
isPrefixOf' (h:t) (x:xs)
  | h == x = isPrefixOf' t xs
  | otherwise = False 

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] lis = False 
isSuffixOf' l [] = True 
isSuffixOf' (h:t) (x:xs)
  | h /= x = isSuffixOf' (h:t) xs
  | otherwise = False

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' [] lis = True
isSubsequenceOf' l [] = False
isSubsequenceOf' (h:t) (a:as)
  | h == a = isSubsequenceOf' t as
  | otherwise = isSubsequenceOf' (h:t) as

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = error "list with no elements"
elemIndices' n (h:t) = elem_acc 0 n h t
  where 
    elem_acc cont ele c []
      | ele == c = [cont]
      | otherwise = []
    elem_acc cont ele c (y:ys)
      | ele == c = cont : elem_acc (cont+1) ele y ys
      | otherwise = elem_acc (cont+1) ele y ys

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (h:t)
  | n == h = t
  | otherwise = h : delete' n t

removeElems :: Eq a => [a] -> [a] -> [a]
removeElems lis [] = lis
removeElems (x:xs) (h:hs)
  | x == h = removeElems xs hs
  | otherwise = x : removeElems xs (h:hs)

union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l
union' lis [] = lis
union' (h:t) (a:y)
  | h == a = union' (h:t) y
  | otherwise = h : union' t (a:y)

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = []
intersect' lis [] = lis 
intersect' (x1:xs) (y1:ys)
  | x1 == y1 = x1 : intersect' xs (y1:ys)
  | otherwise = intersect' xs ys

insert' :: Ord a => a -> [a] -> [a]
insert' d [] = [d]
insert' d (x1:xs)
  | d > x1 = x1 : insert' d xs
  | otherwise = d : insert' x1 xs

unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:[]) = x 
unwords' (x1:xs) = x1 ++ " " ++ unwords' xs

unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:[]) = x 
unlines' (x1:xs) = x1 ++ "\n" ++ unlines' xs

majorAux :: Ord a => [a] -> a
majorAux [] = error "no elements"
majorAux (x:[]) = x
majorAux (x1:x2:xs)
  | x1 < x2 = majorAux (x2:xs)
  | otherwise = majorAux (x1:xs)

posMajor :: Ord a => [a] -> Int
posMajor [] = error "no elements"
posMajor (x:xs) 
  | x < (majorAux (x:xs)) = 1 + posMajor xs
  | otherwise = 0
  
repetidosAux :: Eq a => a -> [a] -> Bool
repetidosAux _ [] = False
repetidosAux n (x1:xs)
  | n == x1 = True
  | otherwise = repetidosAux n xs 

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x1:x2:t) = if ((repetidosAux x1 (x2:t)) == False)
                         then (repetidosAux x2 t)
                         else True 
                         
isDigit' :: Char -> Bool
isDigit' x
  | x >= '0' && x <= '9' = True
  | otherwise = False

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
  | isDigit' h == True = h : algarismos t
  | otherwise = algarismos t
