import Data.List

main = print (sieveSundaram 100)

fun1 :: [Integer] -> Integer 
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . takeWhile (>0) 
    . iterate (\n ->
        if even n
            then n `div` 2
            else 3 * n + 1)

data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

treeLevel :: Tree a -> Integer
treeLevel Leaf           = 0
treeLevel (Node n _ _ _) = n

{-foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr treeInsert Leaf
  where
    treeInsert x Leaf = Node 0 Leaf x Leaf
    treeInsert x (Node n left root right)
      | left > right = Node (treeLevel newRight + 1) left root newRight
      | otherwise = Node (treeLevel newLeft + 1) newLeft root right
        where
          newRight = treeInsert x right
          newLeft = treeInsert x left-}


xor :: [Bool] -> Bool
xor = foldr xor' False
 where xor' p q = (p || q) && not (p && q)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) ([1..n] \\ sieve)
  where sieve = takeWhile (<=n) (map (\(i,j) -> i + j + 2*i*j) (cartProd [1..] [1..]))