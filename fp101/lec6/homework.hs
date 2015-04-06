twice :: (a -> a) -> a -> a
twice f x = f (f x)

rev = foldr (\ x xs -> xs ++ [x]) []

len  = foldr (\ x -> (+) 1) 0

map1 f = foldr (\ x xs -> f x : xs) []


takeWhile2 _ [] = []
takeWhile2 p (x:xs)
  |p x = x : takeWhile2 p xs
  |otherwise = []

compose :: [a ->a] -> (a->a)
compose = foldr (.) id

-- sumsqreven = compose [sum, map (^2), filter even]


unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chomp8 = unfold null (take 8) (drop 8)

altmap f = unfold null (f . head) tail

i2 f = unfold (const False) id f
