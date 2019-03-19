import Data.List(sort, group)
import Data.Map (fromListWith, toList)

-- List of 6 votes
votes = [
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Daxter", "Sly Racoon","Rachet","Clank","Jak"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Daxter", "Clank","Rachet","Sly Racoon","Jak"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Daxter", "Rachet","Clank","Sly Racoon","Jak"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Sly Racoon","Jak","Daxter","Clank","Rachet"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Jak","Daxter", "Rachet","Clank","Sly Racoon"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Jak","Daxter", "Clank","Rachet","Sly Racoon"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Jak","Daxter", "Rachet","Clank","Sly Racoon"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Jak", "Clank","Rachet","Sly Racoon","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak")
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
