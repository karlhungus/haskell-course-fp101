{-# LANGUAGE RankNTypes #-}

type Set a = a -> Bool


union :: Set a -> Set a -> Set a
union x y =  \ a -> (x a) || (y a)


inter :: Set a -> Set a -> Set a
inter x y = \ a ->(x a) && (y a)

--injective every element has a mapping
injective = (+ 3)

--non injective not every element has a mapping
noninjective x = 0

--surjective if codomain and range are equal (i.e. even, but not times2)
--bijective injective and surjective (domain and codomain are mapped for all elements) -- sometimes called equalivence or isomorphism


-- higher order functions -- functions that take functions
-- i.e. identity id x = x, composition (f . g) x = f(g(x))

-- idempotent: f . f = f
idempotent = (* 1)
-- involutive f . f = id (applying itself to itself undoes what it did)
involutive = (* (-1))
-- section f . s = id
sectionf = (+ 1)
sections = (- 1)
-- retract r . f = id
retractionr = (/ 2)
retractionf = (* 2)


-- isomorphism -- pair of functions satisfying:
--  f . g = id
--  g . f = id
-- g : A -> B, f: B -> A  --- A is isomorphic to B (exchangeable not necessarily equal)
--


--data Unit = Unit
-- data Maybe a = Nothing | Just a


-- toMaybe :: Integer -> Maybe Unit
--toMaybe 0 = Just Unit
--toMaybe x = Just $ toMaybe (x - 1)

--fromMaybe :: Maybe Unit -> Integer
--fromMaybe Just Unit =  0
--fromMaybe Just x = 1 + fromMaybe (x - 1)
--fromMaybe Nothing = 0

data Identity a  = Identity a


toIdentity :: (forall r. (a -> r) -> r) -> Identity a
toIdentity f = f $ \a -> Identity a

fromIdentity :: Identity a -> (a -> r) -> r
fromIdentity (Identity x) f = f x



--- Beginning Haskell
--

-- CH 2
--

firstElementEmptyOrAllEmpty [] = True
firstElementEmptyOrAllEmpty x:xs = if x == []
    then True
    else False


-- Ackermann

ackermann :: Integer -> Integer -> Integer
ackermann m n
    | m == 0 = n + 1
    | m > 0 && n == 0 = ackermann (m - 1) 1
    | m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))


--unzipg :: [(Integer, Integer)] -> ([Integer],[Integer])
--unzipg [] = ([],[])
--unzipg l = foldr (\ t (l1, l2) -> ((fst t):l1, (snd t):l2)) ([],[]) l

--unzipg [] = []--([],[])
--unzipg x:xs = unzipg xs


