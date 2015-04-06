fibs :: [Integer]
fibs = 0 : 1 :[ x + y | (x , y) <- zip fibs (tail fibs)]


fib :: Int -> Integer
--fib n = last (take n fibs) -- 1 indexed
--fib n = head (drop (n - 1) fibs) -- 1 indexed
fib n = fibs !! n -- 0 indexed


largeFib :: Integer
largeFib = head (dropWhile (<=1000) fibs)


data Tree a = Leaf 
            | Node (Tree a) a (Tree a)

repeatTree :: a -> Tree a

