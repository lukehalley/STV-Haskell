import Data.List(sortBy, sort, group)
import Data.Map (fromListWith, toList)
import Data.Function (on)
weight = 1000

votes :: [([String], Int)]
votes = [
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Daxter", "Sly Racoon","Rachet","Clank","Jak"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Daxter", "Clank","Rachet","Sly Racoon","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Sly Racoon","Jak","Daxter","Clank","Rachet"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], weight),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], weight),
            (["Jak","Daxter", "Rachet","Clank","Sly Racoon"], weight),
            (["Jak", "Clank","Rachet","Sly Racoon","Daxter"], weight),
            (["Jak","Daxter", "Clank","Rachet","Sly Racoon"], weight),
            (["Jak","Daxter", "Rachet","Clank","Sly Racoon"], weight),
            (["Daxter", "Rachet","Clank","Sly Racoon","Jak"], weight),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], weight)
        ]





firstVote = head votes

seats = 0

candidates = length (fst (votes !! 1))

quota = ((length votes * weight) `div` (candidates + 1)) + 1

count = length (fst (head votes))

candVote xs n = sum [if x == n then 1 else 0 | x <- xs]

-- First past the post

count1 :: Eq a => a -> [a] -> Int
count1 x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count1 v vs, v) | v <- rmdups vs]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner :: Ord a => [[a]] -> a
winner bs = case rank (rmempty bs) of
                [c]    -> c
                (c:cs) -> winner (elim c bs)

-- WORK

getRank = rank (map fst votes)

-- main = do
--     let firstWinner = winner (map fst votes)
--     print firstWinner
--     let currRank = rank (map fst votes)
--     print currRank
--     let flipper = reverse (rank (map fst votes))
--     print flipper
--     let actualWinner = head flipper
--     print actualWinner

firstPref :: [([String], Int)] -> [(String, Int)]
firstPref xs = [( head (fst x), snd x)  | x <- xs]

groupCand :: [([String], Int)] -> [[(String, Int)]]
groupCand xs = group (sort (firstPref xs))

addWeights :: [([String], Int)] -> [(String, Int)]
addWeights xs = [( fst (head x), sum [snd y | y <- x ]) | x <- groupCand xs]

sortByWeight :: Ord b => [(a, b)] -> [(a, b)]
sortByWeight = sortBy (flip compare `on` snd)

collectedBallots = sortByWeight (addWeights votes)