take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [a] -> [(a, a)]
zip' [] _ = []
zip' _ [] = []
zip' (x1:xs1) (x2:xs2) = (x1, x2) : zip' xs1 xs2


elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = x `elem` ys


qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (head:tail) =
  let smaller = qs [ x | x <- tail, x < head]
      bigger  = qs [ x | x <- tail, x >= head]
  in smaller ++ [head] ++ bigger

max' :: (Ord a) => a -> a -> a
max' x y
    | x >= y = x
    | otherwise = y


multThree :: (Num a) => a -> a -> a -> a
multThree a b c = a * b * c

multTwoBy9 :: (Num a) => a -> a -> a
multTwoBy9 = multThree 9


zippy :: (a -> b -> c) -> [a] -> [b] -> [c]
zippy _ [] _ = []
zippy _ _ [] = []
zippy f (x:xs) (y:ys) = f x y : zippy f xs ys

flippy :: (a -> b -> c) -> (b -> a -> c)
flippy f x y = f y x


mappy :: (a -> b) -> [a] -> [b]
mappy _ [] = []
mappy f (x:xs) = f x : mappy f xs

even' :: (Integral a) => a -> Bool
even' x = x `mod` 2 == 0

odd' x = x `mod` 2 /= 0

filty :: (a -> Bool) -> [a] -> [a]
filty _ [] = []
filty p (x:xs)
  | p x = x : filty p xs
  | otherwise = filty p xs


quick_sort [] = []
quick_sort (pivot:tail) =
  let smallerSorted = quick_sort (filter (< pivot) tail)
      largerSorted  = quick_sort (filter (>= pivot) tail)
  in smallerSorted ++ [pivot] ++ largerSorted


chain :: (Integral a, Num a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (3 * n + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong x = length x > 15


maxy :: (Ord a) => [a] -> a
maxy x = foldl1 (\acc x -> if x > acc then x else acc) x

reversy ::  [a] -> [a]
reversy = foldl (\acc e -> e : acc) []



























