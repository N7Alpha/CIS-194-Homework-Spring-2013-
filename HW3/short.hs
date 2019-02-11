
main = print (skips "Hello!")

skips :: [a] -> [[a]]
skips l = [every nth l | nth <- [1..length l]]

every :: Int -> [a] -> [a]
every nth l = [j | (i, j) <- zip [0 ..] l, i `mod` nth == nth - 1]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:lst)
    | y > x && y > z = y : localMaxima (y:z:lst)
    | otherwise = localMaxima (y:z:lst)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

-- returns one * line from the above function
line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

-- counts occurence of numbers in [0..9] in the input list.
count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]