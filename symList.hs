module SymList where

-- Taken from the book by Bird and Gibbons.
--
-- 
type Nat = Int
-- 
-- single, is the list a singleton?
single :: [a] -> Bool
single [x] = True
single _   = False
-- 
-- The idea is that the symmetric list (xs, ys) represents the list xs ++ reverse ys.
type SymList a = ([a], [a])

-- Converts a symmetric list into a standard list.
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys

-- Invariants on (xs, ys)
-- null xs => null ys ∨ single ys
-- null ys => null xs ∨ single xs
--
-- consSL, snocSL, headSL, lastSL, tailSL, initSL, nilSL
--
-- cons
--
cons :: a -> [a] -> [a]
cons = (:)
--
-- snoc
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]
--
-- consSL 
--
consSL :: a -> SymList a -> SymList a
consSL x (xs, ys) = if null ys then ([x], xs) else (x:xs, ys)
--
-- snocSL
--
snocSL :: a -> SymList a -> SymList a
snocSL x (xs, ys) = if null xs then (ys, [x]) else (xs, x:ys)
--
-- headSL
--
headSL :: SymList a -> a
headSL (xs, ys) = if null xs then head ys else head xs

-- 
-- lastSL
--
lastSL :: SymList a -> a
lastSL (xs, ys) = if null ys then (if null xs then error "lastSL of empty list" else head xs) else head ys
--
-- tailSL
-- 
tailSL :: SymList a -> SymList a
tailSL (xs, ys)
  | null xs   = if null ys then error "tailSL of empty list" else nilSL
  | single xs = (reverse vs, us)
  | otherwise = (tail xs, ys)
  where (us, vs) = splitAt (length ys `div` 2) ys

-- initSL
--
initSL :: SymList a -> SymList a
initSL (xs, ys)
  | null ys   = if null xs then error "initSL of empty list" else nilSL
  | single ys = (us, reverse vs)
  | otherwise = (xs, tail ys)
  where (us, vs) = splitAt (length xs `div` 2) xs
--
-- nilSL, an empty symmetric list
nilSL :: SymList a
nilSL = ([], [])
--
-- nullSL, is the symmetric list empty ?
--
nullSL :: SymList [a] -> Bool
nullSL (xs, ys) = null xs && null ys
--
-- lengthSL
--
lengthSL :: SymList a -> Nat
lengthSL (xs, ys) = length xs + length ys
--
-- singleSL, is the symmetric list a singleton?
--
singleSL :: SymList a -> Bool
singleSL (xs, ys) = (null xs && single ys) || (null ys && single xs)
--
-- toSL
--
toSL :: [a] -> SymList a
toSL xs
  | null xs   = nilSL -- if xs is empty, so is toSL xs
  | single xs = (xs, []) -- when xs is a singleton, put it in the first part of the symmetric list
  | otherwise = (us, reverse vs)
  where (us, vs) = splitAt (length xs `div` 2) xs
--
-- reverseSL
--
reverseSL :: SymList a -> SymList a
reverseSL (xs, ys) = (ys, xs)
--
-- mapSL
--
mapSL :: (a -> b) -> SymList a -> SymList b
mapSL f ([], []) = ([], [])
mapSL f (x:xs, y:ys) = (f x:map f xs, f y:map f ys)
--
-- filterSL
--
filterSL :: (a -> Bool) -> SymList a -> SymList a
filterSL p xs = toSL $ filter p $ fromSL xs
--
-- foldrSL
--
foldrSL :: (a -> b -> b) -> b -> SymList a -> b
foldrSL f e xs = foldr f e $ fromSL xs
--
-- foldlSL
--
foldlSL :: (b -> a -> b) -> b -> SymList a -> b
foldlSL f e xs = foldl f e $ fromSL xs
--
-- label
--
label :: [a] -> [(Nat,a)]
label xs = zip [0..] xs
--
-- labelSL
--
labelSL :: SymList a -> SymList (Nat, a)
labelSL xs = toSL $ label $ fromSL xs
--
-- scanlSL
--
scanlSL :: (b -> a -> b) -> b -> SymList a -> SymList b
scanlSL f e ([], []) = ([e], [])
scanlSL f e xs       = toSL $ scanl f e $ fromSL xs