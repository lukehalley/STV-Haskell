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

seats = 5

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

ballots :: [[String]]
ballots = [["Red","Green"],
           ["Blue"],
           ["Green","Red","Blue"],
           ["Blue","Green","Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c]    -> c
                (c:cs) -> winner' (elim c bs)

-- WORK

whoWon = winner' (map fst votes)