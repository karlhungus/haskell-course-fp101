-- sum100 = sum [[x * x] | x <- [1 .. 100]]
sum101 = sum [x ^ 2 | x <- [1 .. 100]]
sum102 = sum [const 2 x | x <- [1 .. 100]]
sum103 = foldl (+) 1 [x ^ 2 | x <- [1 .. 100]]


pyths n = [(x, y , z)|x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^2 ]


factors :: Int -> [Int]
factors n = [x|x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]


primes n = [x | x <- [1..n], prime x]
-- primes2 n = [x | x <- [1..n], not even x && prime x]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i| (x', i) <- zip xs [0..n], x == x']
          where n = length xs - 1

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions2 :: (Eq a) => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [1..])

scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
