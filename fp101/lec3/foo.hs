
foo n | n == 0 = "zero"
      | n == 1 = "one"
      | True = "otherwise"

-- halves xs = (take n xs, drop (n + 1) xs)
--       where n = length xs `div` 2

-- halves xs = splitAt (length xs `div` 2) xs



safetail xs = if null xs then [] else tail xs
