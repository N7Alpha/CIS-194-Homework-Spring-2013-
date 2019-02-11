{-# LANGUAGE FlexibleInstances #-}
-- Exercise 1

fib :: Integer -> Integer
fib x
    | x > 1 = fib (x-1) + fib (x-2)
    | otherwise = x


fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fib2 :: Integer -> Integer -> [Integer]
fib2 x y = x : fib2 y (x+y)

fibs2 :: [Integer]
fibs2 = fib2 0 1

-- Exercise 3
data Stream a = Cons a (Stream a)
   deriving (Eq)

instance Show a => Show (Stream a) where
    show = show . streamTake 20

streamTake :: Integer -> Stream a -> [a]
streamTake count (Cons val next) 
    | count > 0 = val : streamTake (count-1) next
    | otherwise = []

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat = streamFromSeed id

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons val next) = Cons (f val) (streamMap f next)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a next) other = Cons a (interleaveStreams other next)

streamFoldr :: (a -> b -> b) -> Stream a -> b
streamFoldr f (Cons val next) = val `f` streamFoldr f next

ruler :: Stream Integer
ruler = streamFoldr interleaveStreams (streamMap streamRepeat nats)

-- Exercise 6
x :: Stream Integer