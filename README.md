# STV-Haskell
Implementation of the "Single Transferable Vote" system in Haskell.

A list (*votes*) of Tuples which contain Tuples with the following = [a list of votes in Strings], the weight of the votes (always 1000) is included in this repository:

![image](https://user-images.githubusercontent.com/5617407/57231751-10cbec00-7013-11e9-8639-dc1754aecbe3.png)

To get the election results fun the following in GHCI:

`runElection votes 3`

where:
- runElection: the main function whihc runs the voting system.
- votes: is the list of votes (described above) which the candidates will be picked from.
- 3: the number of available seats.

after running the above, the three candidates which where elected will be displayed **in the order they were elected in**:

![image](https://user-images.githubusercontent.com/5617407/57231540-99965800-7012-11e9-8a8b-b5beb450c5a1.png)
