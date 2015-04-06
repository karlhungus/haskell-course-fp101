module Lab5 where

import Control.Monad
import Control.Applicative

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

action :: Concurrent a -> Action
action (Concurrent c) = action' c

action' :: ((a -> Action) -> Action) -> Action
action' c = c $ const Stop


-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent stop'

stop' :: (a -> Action) -> Action
stop' = const Stop

-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom = Concurrent . atom'

atom' :: IO a -> (a -> Action) -> Action
atom' ioAction c = Atom $ liftM c ioAction

-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork c = Concurrent $ \f -> Fork (action c) (f ())

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent c) (Concurrent c') = Concurrent $ \f -> Fork (c f) (c' f)


-- ===================================
-- Ex. 4
-- ===================================

instance Functor Concurrent where
    fmap = (<$>)


instance Applicative Concurrent where
    pure = return
    (<*>) = ap

-- bind :: ((a -> Action) -> Action)
--              -> (a -> ((b -> Action) -> Action))
--              -> (b -> Action) -> Action
-- bind f g bToAction = f $ \a -> g a bToAction

instance Monad Concurrent where
    -- (>>=) :: Concurrent a -> (a -> Concurrent b) -> Concurrent b
    (Concurrent f) >>= g = Concurrent $ \bToAction -> f $ \a ->
                                            (\(Concurrent g') -> g' bToAction) $ g a
    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin []     = return ()
roundRobin (x:xs) = case x of
    (Atom ioAction) -> do
                            a <- ioAction
                            roundRobin $ xs ++ [a]
    (Fork a b)      -> roundRobin $ xs ++ [a,b]
    Stop            -> roundRobin xs

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop = mapM_ (atom . putStr . show)
