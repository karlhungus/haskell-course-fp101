import System.IO

hangman :: IO ()
hangman =
         do putStr "Think of a word: "
            word <- sgetLine
            putStrLn "Try to guess it:"
            guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)


getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

guess :: String -> IO ()
guess word =
   do putStr "> "
      xs <- getLine
      if xs == word then
         putStr "You got it!"
      else
         do putStrLn (diff word xs)
            guess word

diff :: String -> String -> String
diff xs ys =
  [if elem x ys then x else '-' | x <- xs]


putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLen' :: String -> IO ()
putStrLen' [] = putChar '\n'
putStrLen' xs = putStr' xs >> putStrLen' ""

--putStrLen' [] = putChar '\n'
--putStrLen' xs = putStr' xs >> putChar 'a'

--putStrLen' [] = putChar '\n'
--putStrLen' xs = putStr' xs >>= \x -> putChar '\n'

-- wrong
--putStrLen' [] = putChar '\n'
--putStrLen' xs = putStr' xs >> \x -> putChar '\n'

--putStrLen' [] = putChar '\n'
--putStrLen' xs = putStr' xs >> putStr' "\n"

-- wrong
--putStrLen' [] = putChar '\n'
--putStrLen' xs = putStr' xs >> putStrLen' "\n"

-- wrong
--putStrLen' [] = return ""
--putStrLen' xs = putStrLen' xs >> putStr' "\n"

-- wrong
--putStrLen' [] = putChar '\n'
--putStrLen' xs = putStr' xs >> putStrLen' "\n"

getLine' :: IO String
getLine' = get ""

get :: String -> IO String
get xs
  = do x <- getChar
       case x of
           '\n' -> return xs
           _ -> get (xs ++ [x])


interact' :: (String -> String) -> IO ()
interact' f
  = do input <- getLine'
       putStrLen' (f input)


sequence_' :: Monad m => [m a] -> m ()
--sequence_' [putChar 'a', putChar 'b', putChar 'c']
--x
--sequence_' [] = return []
--sequence_' (m:ms) = m >> \_ -> sequence_' ms


sequence_' [] = return ()
sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

--x
--sequence_' ms = foldl (>>) (return ()) ms

--sequence_' [] = return ()
--sequence_' (m:ms) = m >> sequence_' ms

--
--sequence_' [] = return ()
--sequence_' (m:ms) = m >>= \_ -> sequence_' ms

--x
--sequence_' ms = foldr (>>=) (return ()) ms


--sequence_' ms = foldr (>>) (return ()) ms

--x
--sequence_' ms = foldr (>>) (return []) ms


sequence' :: Monad m => [m a] -> m [a]
--sequence [getChar, getChar, getChar]

--sequence' [] = return []
--sequence' (m : ms)
--          = m >>=
--              \ a ->
--                do as <- sequence' ms
--                   return (a:as)

--x
--sequence' ms = foldr func (return ()) ms
--  where
--       func :: (Monad m) => m a -> m [a] -> m [a]
--       func m acc
--          = do x <- m
--               xs <- acc
--               return (x:xs)

--x
--sequence' ms = foldr func (return []) ms
--  where
--       func :: (Monad m) => m a -> m [a] -> m [a]
--       func m acc = m : acc


--x
--sequence' [] = return []
--sequence' (m : ms) = return (a : as)
--  where
--       a <- m
--       as <- sequence' ms

--sequence' ms = foldr func (return []) ms
--  where
--       func :: (Monad m) => m a -> m [a] -> m [a]
--       func m acc
--         = do x <- m
--              xs <- acc
--              return (x:xs)

--x
--sequence' [] = return []
--sequence' (m : ms)
--  = m >>
--      \ a ->
--        do as <- sequence' ms
--           return (a : as)

--x
--sequence' [] = return []
--sequence' (m : ms) = m >>= \a ->
--  as <- sequence' ms
--  return (a : as)

sequence' [] = return []
sequence' (m : ms) =
  do a <- m
     as <- sequence' ms
     return (a : as)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM' (\ a ->  return a) [1,2,3]

--mapM' f as = sequence' (map f as)


--mapM' f [] = return []
--mapM' f (a : as)
--  = f a >>= \ b -> mapM' f as >>= \ bs -> return (b :bs)

--x
--mapM' f [] = return []
--mapM' f (a : as) =
--    do
--       f a -> b
--       mapM' f as -> bs
--       return (b : bs)

--mapM' f [] = return []
--mapM' f (a : as) =
--    do
--       b <- f a
--       bs <- mapM' f as
--       return (b : bs)

--mapM' f [] = return []
--mapM' f (a : as)
--  = f a >>=
--      \ b ->
--        do bs <- mapM' f as
--           return (b : bs)

mapM' f [] = return []
mapM' f (a : as)
  = f a >>=
      \ b ->
        do bs <- mapM' f as
           return (bs ++ [b])


filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return (x : ys) else return ys



-- foldl f z []     = z
-- foldl f z (x:xs) = foldl f (f z x) xs
foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
--foldLeftM f a [] = a
--foldLeftM f a (x : xs) = foldLeftM f ( f a (>>=) x) xs
--foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrln r
foldLeftM _ a []      =  return a
foldLeftM f a (x:xs)  =  f a x >>= \fax -> foldLeftM f fax xs

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f d []     = return d
foldRightM f d (x:xs) = (\z -> f x z) <<= foldRightM f d xs
    where (<<=) = flip (>>=)


liftM :: Monad m => (a -> b) -> m a -> m b


--liftM f m
--  = do x <- m
--       return (f x)

--x
--liftM f m = m >>= \a -> f a

--liftM f m = m >>= \a -> return (f a)

--x
--liftM f m = return (f m)


--liftM f m = m >>= \a -> m >>= \ b -> return (f a)

--x
--liftM f m = m >>= \a -> m >>= \ b -> return (f b)

--x
--liftM f m = mapM f [m]

liftM f m = m >> \ a -> return (f a)



