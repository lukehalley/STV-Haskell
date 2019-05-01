-- Import the sorting libraries and the group library.
import Data.List(sortBy, sort, group)
-- Import the map library.
import Data.Map (fromListWith, toList)
-- Import the on library.
import Data.Function (on)

-- Set the weight of each vote to 1000.
weight = 1000

-- Create an empty list of Strings which will contain the elected candidates.
-- This will be returned at the end of the program, this will contain 3 people as seats = 3.
elected :: [String]
elected = []

-- Create an empty list of Strings which will contain the eleminated candidates.
eleminated :: [String]
eleminated = []

-- Create an empty list of Strings which will contain the candidates who are still running.
-- This is list will contain the candidates who are still running in the election.
stillRunning :: [String]
stillRunning = []

-- The number of available seats.
seats :: Int
seats = 3

-- The list of Tuples which contain Tuples with the following = [a list of votes in Strings], the weight of the votes (always 1000).
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

-- Get the first vote of the votes list.
firstVote = head votes

-- Count the number of candidates that are running for the election.
candidates = length (fst (votes !! 1))

-- Variable which contains the calculated quota. 
quota = ((length votes * weight) `div` (candidates + 1)) + 1

-- Get the number of votes in a vote by counting how many Strings are in the first item of the tuple.
count = length (fst (head votes))

-- 
candVote xs n = sum [if x == n then 1 else 0 | x <- xs]

-- Function to remove any empty votes from the list of votes. 
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- Function to get the first perference for every single vote in the voting along with the votes weight and return it in a list of tuples.
firstPref :: [([String], Int)] -> [(String, Int)]
firstPref xs = [( head (fst x), snd x)  | x <- xs]

-- Function to return a list of lists which contain tuples of first preference votes grouped by their candidate.
groupCand :: [([String], Int)] -> [[(String, Int)]]
groupCand xs = group (sort (firstPref xs))

-- Add the number of votes each candidate has and return a list of tuples containing the candidate and his/her total votes.
addWeights :: [([String], Int)] -> [(String, Int)]
addWeights xs = [( fst (head x), sum [snd y | y <- x ]) | x <- groupCand xs]

-- Sort the votes by their weight.
sortByWeight :: Ord b => [(a, b)] -> [(a, b)]
sortByWeight = sortBy (flip compare `on` snd)

-- Return a sorted list of tuples of candiates and their total votes. The tuple is sorted by the number of votes a candidates has.
sortedVotes = sortByWeight (addWeights votes)

-- Eliminate a candidate from the election.
elimCand :: String -> [([String], Int)] -> [([String], Int)]
elimCand cand [] = []
elimCand cand (v:vs) = (filter (/= cand) (fst v), snd v ) : elimCand cand vs

-- Get the highest current weight a candidate currently has to check if we can elect him/her.
topWeight = snd (head sortedVotes)

-- Get the first elected candidate.
firstElected = fst (head sortedVotes)

-- Create a new votes list with the first elected removed.
newVotes = elimCand firstElected votes

-- Function which takes in a candidate's current number of votes and returns -1 if they can be elected and 0 if they cannot.
decide x
    | x >= quota = -1
    | otherwise = 0

--          |votes                 |elected      |eliminated   |running      |seats |outcome
-- doCounts :: [([String], Int)]  ->  [String]  ->  [String]  ->  [String]  ->  Int -> [String]
-- doCounts [([], weight)] [] [] [] seats = []
-- doCounts votes elected eliminated running seats = if length elected + length running >= seats
--                                                     then 
--                                                         ["My results"] 
--                                                         elected
--                                                     else if topWeight >= quota
--                                                             then 
--                                                                 ["Were Have someone elected !"]
--                                                                 votes <- transferVotes
--                                                                 doCounts firstElected : elected
--                                                     else