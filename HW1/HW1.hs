main = print exercise

toDigits    :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev (x `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherHelper . reverse

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper (x:y:zs) = x:2*y : doubleEveryOtherHelper zs
doubleEveryOtherHelper x = x


sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigits) 0

validate :: Integer -> Bool
validate =  (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

exercise = validate 4012888888881881

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
    | n > 0 = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
    | otherwise = []