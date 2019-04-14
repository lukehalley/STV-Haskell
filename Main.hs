module Main where

    -- Import the sorting libraries and the group library.
    import Data.List(sortBy, sort, group, partition)
    -- Import the map library.
    import Data.Map (fromListWith, toList)
    -- Import the on library.
    import Data.Function (on)
    
    import Data.Maybe (isJust)
    import Debug.Trace
    
    type Person = String
    type People = [Person]
    type Vote = (People, Int)
    type Votes = [Vote]
    
    data ExitingCandidate = Eliminated Person | Elected Person

    -- Set the weight of each vote to 1000.
    weight :: Int
    weight = 1000
    
    -- Create an empty list of Strings which will contain the elected candidates.
    -- This will be returned at the end of the program, this will contain 3 people as seats = 3.
    elected :: People
    elected = []
    
    -- Create an empty list of Strings which will contain the eleminated candidates.
    eleminated :: People
    eleminated = []
    
    -- Create an empty list of Strings which will contain the candidates who are still running.
    -- This is list will contain the candidates who are still running in the election.
    stillRunning :: People
    stillRunning = []
    
    -- The number of available seats.
    seats :: Int
    seats = 3
    
    -- Create an empty list of Strings which will contain the elected candidates.
    -- This will be returned at the end of the program, this will contain 3 people as seats = 3.
    electedCand :: Person
    electedCand = ""
    
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
    firstVote :: ([String], Int)
    firstVote = head votes
    
    -- Count the number of candidates that are running for the election.
    candidates :: Int
    candidates = length (fst (votes !! 1))
    
    -- Variable which contains the calculated quota.
    quota :: Int
    quota = ((length votes * weight) `div` (candidates + 1)) + 1
    
    -- Get the number of votes in a vote by counting how many Strings are in the first item of the tuple.
    count :: Int
    count = length (fst (head votes))
    
    -- Function to remove any empty votes from the list of votes. 
    rmempty :: Eq a => [[a]] -> [[a]]
    rmempty = filter (/= [])
    
    -- Function to get the first perference for every single vote in the voting along with the votes weight and return it in a list of tuples.
    firstPref :: [([String], Int)] -> [(String, Int)]
    firstPref xs = [( head (fst x), snd x)  | x <- xs]
    
    -- Sort the votes by their weight.
    sortByWeight :: Ord b => [(a, b)] -> [(a, b)]
    sortByWeight = sortBy (flip compare `on` snd)
    
    -- Return a sorted list of tuples of candiates and their total votes. The tuple is sorted by the number of votes a candidates has.
    sortedVotes :: [([String], Int)] -> [(String, Int)]
    sortedVotes v = sortByWeight (addWeights v)
    
    -- Eliminate a candidate from the election.
    elimCand :: Person -> Votes -> Votes
    elimCand _ [] = []
    elimCand cand (v:vs) = (filter (/= cand) (fst v), snd v ) : elimCand cand vs
    
    -- Get the highest current weight a candidate currently has to check if we can elect him/her.
    topWeight :: Votes -> Int
    topWeight votes = snd (head (sortedVotes votes))
    
    bottomWeight :: Votes -> Int
    bottomWeight votes = snd . last . sortedVotes $ votes
    
    -- If we can elect a candidate, we use this to get the next candidate to elect.
    nextToElect :: Votes -> Person
    nextToElect votes = fst (head (sortedVotes votes))
    
    -- If we have to eliminate a candidate, we use this to get the candidate with the lowest number of votes to eliminate.
    nextToEliminate :: Votes -> Person
    nextToEliminate votes = fst (last (sortedVotes votes))
    
    -- Group each unique candidate with their total votes.
    groupCand :: [([String], Int)] -> [[(String, Int)]]
    groupCand xs = group (sort (firstPref xs))
    
    -- Add the number of votes each candidate has and return a list of tuples containing the candidate and his/her total votes.
    addWeights :: [([String], Int)] -> [(String, Int)]
    addWeights xs = [( fst (head x), sum [snd y | y <- x ]) | x <- groupCand xs]
    
    -- Create a new votes list with the first elected removed.
    newVotes votes = elimCand (nextToElect votes) votes
    
    -- Main function which takes in the list of Votes and the number of seats and calls the election function.
    runElection :: Votes -> Int -> People
    runElection v s = election v [] [] (fst . head $ v) s
    
    {-# ANN module "Hlint: Use guards." #-}
    -- Recusive function which iterates through the votes list until all seats have been filled. 
    election :: Votes -> People -> People -> People -> Int -> People
    election votes elected eliminated running seatsNum =
        -- First recursive stop condition:
        -- Check if we have enough people elected to fill the seats we have passed in.
        if length elected == seatsNum then
            -- Display the elected candidates.
           reverse elected
        else
            -- Second recursive stop condition:
            -- We have eliminated enough candidates that the remaining candidates (running) fill the specified seats.
            if length elected + length running == seatsNum
            then
                -- Place the remaining running candidates into the elected list.
                elected ++ running
            else
                -- If we havent filled the seats do another step of elimination and recurse.
                election votes' elected' eliminated' running' seatsNum
                where
                    -- here's where one person is either elected or eliminated
                    (votes', exitingCandidate, running') = doElectOrElim votes running seatsNum
                    -- and basing on the result, we append that person to appropriate list
                    (elected', eliminated') = case exitingCandidate of
                        Elected p -> (p:elected, eliminated)
                        Eliminated p -> (elected, p:eliminated)
    
    -- Computes the next step, which is either eliminating or electing someone.
    -- Also provides the updated vote list and updated running list
    doElectOrElim :: Votes -> People -> Int -> (Votes, ExitingCandidate, People)
    doElectOrElim votes running seatsNum = (votes', exitingCandidate, running')
        where
            exitingCandidate =
                -- If the candidate with the highest amount of votes can be elected, elect that candidate.
                if topWeight votes > quota then
                    Elected $ nextToElect votes
                -- Otherwise eliminate the lowest candidate and distribute that candidates votes.
                else
                    Eliminated $ nextToEliminate votes
    
            -- After we have either elected or eliminated a candidate, remove the candidate from the list of running participants.
            running' = filter (/= exitingPerson) running
                where
                    exitingPerson = case exitingCandidate of
                        Elected p -> p
                        Eliminated p -> p
    
            -- After a candidate is removed from the list of running participants, we redistribute the votes of an exiting candidate.
                -- If the candidate was elected, then reduce the vote weight with the formula.
                -- If the candidate was eliminated, then keep weights.
                --   All other votes stay unchanged.
            votes' = unchangedVotes ++ redistributedVotes
                where 
                    -- Partition the list votes into two list of votes:
                        -- 1) votes that will be redistributed (votesToRedistribute).
                        -- 2) votes that don't affect currently exiting candidate (unchangedVotes).
                    (votesToRedistribute, unchangedVotes) = partition belongsToExitingCandidate votes
                        where
                            belongsToExitingCandidate :: Vote -> Bool
                            belongsToExitingCandidate = case exitingCandidate of
                                -- If we are electing use a lambda function to get the first candidate of the list people.
                                Elected elected -> \(people, _) -> elected == head people
                                -- If we are eliminating use a lambda function to get the last candidate of the list people.
                                Eliminated eliminated -> \(people, _) -> eliminated == last people
    
                    -- After partitioning the list votes the votes to redistribute needs to be then calculated.
                    -- The following:
                        -- If a candidate was elected reduces their weight.
                        -- * If a candidate was:
                            -- elected remove that candidate from the front of the the vote list.
                            -- eliminated remove that candidate from the back of the the vote list.
                    redistributedVotes = map redistribute votesToRedistribute
                        where
                            -- We pick the redistribution strategy basing on the type of exit:
                            redistribute = case exitingCandidate of
                                -- If the candidate was elected call the redistributeElected function.
                                Elected _ -> redistributeElected
                                -- If the candidate was elected call the redistributeEliminated function.
                                Eliminated _ -> redistributeEliminated
                            
                            -- remove that elected candidate from the front of the the vote list by using
                            -- tail to remove the first candidate in the people list.
                            redistributeElected (people, weight) = (map tail people, reduceWeight weight)

                            -- remove that eliminated candidate from the back of the the vote list by using
                            -- init to remove the last candidate in the people list.
                            redistributeEliminated (people, weight) = (map init people, weight)
    
                            -- The formula to reduce a votes weight for a candidate who was elected.
                            reduceWeight w = round $ fromIntegral w * (fromIntegral surplus / fromIntegral totalVoteWeight)
    
                            -- Calculate the surplus.
                            surplus = totalVoteWeight - quota

                            -- Calculate the total weight of all the current votes.
                            totalVoteWeight = sum . map snd $ votesToRedistribute
    
    main :: IO ()
    main = print "Haskell Implmentation of the Single Tranfer Vote system."