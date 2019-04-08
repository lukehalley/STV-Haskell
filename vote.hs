import Data.List(sortBy, sort, group)
import Data.Map (fromListWith, toList)
import Data.Function (on)
weight = 1000

elected :: [String]
elected = []

eleminated :: [String]
eleminated = []

stillRunning :: [String]
stillRunning = []

seats :: Int
seats = 3

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

candidates = length (fst (votes !! 1))

quota = ((length votes * weight) `div` (candidates + 1)) + 1

count = length (fst (head votes))

candVote xs n = sum [if x == n then 1 else 0 | x <- xs]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

firstPref :: [([String], Int)] -> [(String, Int)]
firstPref xs = [( head (fst x), snd x)  | x <- xs]

groupCand :: [([String], Int)] -> [[(String, Int)]]
groupCand xs = group (sort (firstPref xs))

addWeights :: [([String], Int)] -> [(String, Int)]
addWeights xs = [( fst (head x), sum [snd y | y <- x ]) | x <- groupCand xs]

sortByWeight :: Ord b => [(a, b)] -> [(a, b)]
sortByWeight = sortBy (flip compare `on` snd)

sortedVotes = sortByWeight (addWeights votes)

elimCand :: String -> [([String], Int)] -> [([String], Int)]
elimCand cand [] = []
elimCand cand (v:vs) = (filter (/= cand) (fst v), snd v ) : elimCand cand vs

firstElected = fst (head sortedVotes)

newVotes = elimCand firstElected votes

decide x
    | x >= quota = -1
    | otherwise = 0

-- --          |votes                 |elected      |eliminated   |running      |seats |outcome
-- doCounts :: [([String], Int)]  ->  [String]  ->  [String]  ->  [String]  ->  Int -> [String]
-- doCounts [([], weight)] [] [] [] seats = []
-- doCounts votes elected eliminated running seats =
--                                             | (length elected) + (length running) >= seats = []
--                                             | otherwise = []


--          |votes                 |elected      |eliminated   |running      |seats |outcome
doCounts :: [([String], Int)]  ->  [String]  ->  [String]  ->  [String]  ->  Int -> [String]
doCounts [([], weight)] [] [] [] seats = []
doCounts votes elected eliminated running seats = if length elected + length running >= seats
                                                    then ["Were Done"]
                                                    else ["Were Not Done"]

-- PUT THE QUOTA ON THE ELECTED AS ITS WEIGHT