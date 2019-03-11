import Data.List(sort, group)
import Data.Map (fromListWith, toList)

-- List of 6 votes
votes = [
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Daxter", "Sly Racoon","Rachet","Clank","Jak"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Daxter", "Clank","Rachet","Sly Racoon","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Sly Racoon","Jak","Daxter","Clank","Rachet"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000),
            (["Rachet","Clank","Sly Racoon","Jak","Daxter"], 1000),
            (["Clank","Rachet","Sly Racoon","Jak","Daxter"], 1000),
            (["Jak","Daxter", "Rachet","Clank","Sly Racoon"], 1000),
            (["Jak", "Clank","Rachet","Sly Racoon","Daxter"], 1000),
            (["Jak","Daxter", "Clank","Rachet","Sly Racoon"], 1000),
            (["Jak","Daxter", "Rachet","Clank","Sly Racoon"], 1000),
            (["Daxter", "Rachet","Clank","Sly Racoon","Jak"], 1000),
            (["Sly Racoon","Rachet","Clank","Daxter","Jak"], 1000)
        ]

pref1 (p1,p2,p3,p4,p5) = p1
pref2 (p1,p2,p3,p4,p5) = p2
pref3 (p1,p2,p3,p4,p5) = p3
pref4 (p1,p2,p3,p4,p5) = p4
pref5 (p1,p2,p3,p4,p5) = p5

firstVote = head votes

getQuota = (length votes `div` (5 + 1)) + 1

getFirstPref :: String -> Int  
getFirstPref x = length [pref1 p | p <- votes, pref1 p == x]

getSecondPref :: String -> Int  
getSecondPref x = length [pref2 p | p <- votes, pref2 p == x]

getThirdPref :: String -> Int  
getThirdPref x = length [pref3 p | p <- votes, pref3 p == x]

getFourthPref :: String -> Int  
getFourthPref x = length [pref4 p | p <- votes, pref4 p == x]

getFifthPref :: String -> Int  
getFifthPref x = length [pref5 p | p <- votes, pref5 p == x]

slyVote = getFirstPref "Sly Racoon"
rachetVote = getFirstPref "Rachet"
daxterVote = getFirstPref "Daxter"
clankVote = getFirstPref "Clank"
jakVote = getFirstPref "Jak"
