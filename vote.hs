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
            ("Sly Racoon","Rachet","Clank","Daxter","Jak"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
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
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
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
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
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
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
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
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Rachet","Clank","Sly Racoon","Jak","Daxter"),
            ("Clank","Rachet","Sly Racoon","Jak","Daxter"),
            ("Sly Racoon","Rachet","Clank","Daxter","Jak")
        ]

pref1 (p1,p2,p3, p4, p5) = p1
pref2 (p1,p2,p3, p4, p5) = p2
pref3 (p1,p2,p3, p4, p5) = p3
pref4 (p1,p2,p3, p4, p5) = p4
pref5 (p1,p2,p3, p4, p5) = p5

firstVote = head votes

getQuota = (length votes `div` (5 + 1)) + 1

rCount = length [pref1 p | p <- votes, pref1 p == "Rachet"]

getFirstPref :: String -> Int  
getFirstPref x = length [pref1 p | p <- votes, pref1 p == x]