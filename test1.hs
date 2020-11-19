import Data.List
import Data.Char
import Data.Either

--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' n m
    | n < m = n : enumFromTo' (n+1) m
    | n == m = [n] -- ou o [m]
    | n > m = []

--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' beg by fin
    | (beg <= fin && beg <= by) = beg : enumFromThenTo' by (2*by-beg) fin
    | (beg >= fin && beg >= by) = beg : enumFromThenTo' by (2*by-beg) fin
    | otherwise = [beg]

--3
addLists :: [a] -> [a] -> [a]
addLists [] lis = lis
addLists l [] = l
addLists (a:as) lis = [a] ++ addLists as lis 

--4
posInList :: [a] -> Int -> a
posInList [] _ = error "no elements"
posInList (h:t) 0 = h
posInList (h:t) pos = posInList t (pos-1)

--5
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h:t) = reverseList t ++ [h]

--6
takeFromList :: Int -> [a] -> [a]
takeFromList _ [] = []
takeFromList 0 lis = []
takeFromList ele (x1:xs) = x1 : takeFromList (ele-1) xs

--7
dropFromList :: Int -> [a] -> [a]
dropFromList _ [] = []
dropFromList 0 lis = lis
dropFromList ele (a1:as) = dropFromList (ele-1) as 

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] lis = []
zip' l [] = []
zip' (h:t) (h1:t1) = [(h,h1)] ++ zip' t t1 

--9
elemInList :: Eq a => a -> [a] -> Bool
elemInList _ [] = False
elemInList a (h:t)
  | a == h = True
  | otherwise = elemInList a t

--10
replicateElem :: Int -> a -> [a]
replicateElem 0 _ = []
replicateElem n h = h : replicateElem (n-1) h

--11
betweenElem :: a -> [a] -> [a]
betweenElem _ [] = []
betweenElem _ (x:[]) = [x]
betweenElem n (h:t) = h : n : betweenElem n t

--12
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (h:t) = group_acc [h] h t
  where 
    group_acc rep c [] = [rep]
    group_acc rep c (y:ys)
      | c == y = group_acc (rep ++ [y]) c ys
      | otherwise = rep : group_acc [y] y ys

--13
concat' :: [[a]] -> [a]
concat' [[]] = []
concat' [(x:[])] = [x]
concat' (h:t) = h ++ concat' t 

--14
inits' :: [a] -> [[a]]
inits' [] = []
inits' (h:t) = inits_acc [] (h:t)
  where 
    inits_acc rep [] = [rep]
    inits_acc rep (y:ys) = rep : inits_acc (rep ++ [y]) ys 

--15
tails' :: [a] -> [[a]]
tails' [] = []
tails' (h:t) = tails_acc (h:t) t
  where  
    tails_acc rep [] = [rep,[]]
    tails_acc rep (u:us) = rep : tails_acc (tail rep) us

--16
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] lis = True
isPrefixOf' l [] = False
isPrefixOf' (h:t) (x:xs)
  | h == x = isPrefixOf' t xs
  | otherwise = False 

--17
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] lis = False 
isSuffixOf' l [] = True 
isSuffixOf' (h:t) (x:xs)
  | h /= x = isSuffixOf' (h:t) xs
  | otherwise = False

--18
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' [] lis = True
isSubsequenceOf' l [] = False
isSubsequenceOf' (h:t) (a:as)
  | h == a = isSubsequenceOf' t as
  | otherwise = isSubsequenceOf' (h:t) as

--19
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

--20
nubAux' :: Eq a => a -> [a] -> [a]
nubAux' _ [] = []
nubAux' n (x1:xs)
  | n == x1 = nubAux' n xs
  | otherwise = x1 : nubAux' n xs

nub' :: Eq a => [a] -> [a]
nub' [] = [] 
nub' (x:[]) = [x] 
nub' (x1:x2:xs) = x1 : (nubAux' x1 (nub'(x2:xs)))

--21
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (h:t)
  | n == h = t
  | otherwise = h : delete' n t

--22
removeElems :: Eq a => [a] -> [a] -> [a]
removeElems lis [] = lis
removeElems (x:xs) (h:hs)
  | x == h = removeElems xs hs
  | otherwise = x : removeElems xs (h:hs)

--23
union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l
union' lis [] = lis
union' (h:t) (a:y)
  | h == a = union' (h:t) y
  | otherwise = h : union' t (a:y)

--24
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = []
intersect' lis [] = lis 
intersect' (x1:xs) (y1:ys)
  | x1 == y1 = x1 : intersect' xs (y1:ys)
  | otherwise = intersect' xs ys

--25
insert' :: Ord a => a -> [a] -> [a]
insert' d [] = [d]
insert' d (x1:xs)
  | d > x1 = x1 : insert' d xs
  | otherwise = d : insert' x1 xs

--26
unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:[]) = x 
unwords' (x1:xs) = x1 ++ " " ++ unwords' xs

--27
unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:[]) = x 
unlines' (x1:xs) = x1 ++ "\n" ++ unlines' xs

--28
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
 
--29
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
 
 --30
isDigit' :: Char -> Bool
isDigit' x
  | x >= '0' && x <= '9' = True
  | otherwise = False

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
  | isDigit' h == True = h : algarismos t
  | otherwise = algarismos t

-- Auxiliar para 31 e 32
calPos :: Int -> [a] -> a
calPos 0 (h:t) = h
calPos n (h:t) = calPos (n-1) t 

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares (x:[]) = []
posImpares (x1:xs) = (calPos 1 (x1:xs)) : posImpares (tail xs)

--32
posPares :: [a] -> [a]
posPares [] = []
posPares (x:[]) = [x]
posPares (x1:xs) = (calPos 0 (x1:xs)) : posPares (tail xs)

--33
isSorted' :: Ord a => [a] -> Bool
isSorted' [] = True
isSorted' (x1:x2:xs)
  | x1 < x2 = isSorted' (x2:xs)
  | otherwise = False

--34
{- Definido em cima - fiz novamente para treino (outra maneira)
insert'' :: Ord a => a -> [a] -> [a]
insert'' n [] = [n]
insert'' n (x:xs)
  | n < x = n : (x:xs)
  | otherwise = x : insert'' n xs -}

iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (x1:xs) = (insert' x1 (iSort' xs))

--35
menor :: String -> String -> Bool
menor l [] = False
menor [] lis = True
menor (a:as) (h:hs)
  | a <= h = menor as hs
  | otherwise = False 

-- vamos criar um novo tipo Conjuntos
type Conjunto a = [(a,Int)]

--36
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' c [] = False
elemMSet' c (x:xs)
  | c == (fst x) = True
  | otherwise = elemMSet' c xs

-- vamos criar uma nova Data Movimento
data Movimento = Norte | Sul | Este | Oeste

vertical ::  [Movimento] -> Bool
vertical [] = False
vertical (h:[]) = case h of
  Norte -> True
  Sul -> True
  _ -> False
vertical (h:t) = case h of
  Norte -> vertical t
  Sul -> vertical t
  _ -> False 
