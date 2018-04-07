#R Simulation of Borda Count Election
# The goal of this simulation is to see how often the Borda
# Count winner loses in pairwise votes
# IN this simulation each preference ordering is equally likely


### Function: prefOrder
### Inputs: X, a vector of values
### Output: a random preference ordering of X
prefOrder <- function(x) {
  sample(x) #return a random preference ordering
}

### Function: Trial
### Inputs: N, the number of candidates that can be voted for
###         X, the number of voters
### Output: Random perference orderings for X voters, voting for N candidates
trial <- function(n, x) {
  base <- c(1:n) #Create a vector with values from 1 to n
  #set.seed(001)
  voters <- list()
  for (i in 1:x)
    voters[[i]] = prefOrder(base)
  voters
}

### Function: findBordaCount
### Inputs: LISTY, a list of voters and their preferences
### Output: The Borda Counts for each candidate
findBordaCount <- function(listy) {
  f = length(listy[[1]])
  voteCount <- integer(f)
  for (i in 1:length(listy)) {
    print(i)
    for (j in 1:f) {
      voteCount[j] = voteCount[j] + listy[[i]][j]
    }
  }
 voteCount
}

### Function: bordaCountWinner
### returns the Borda Count Winner
bordaCountWinner <- function(listy) {
  which.max(findBordaCount(listy))
}

### Function: findPluralityWinner
### Inputs: LISTY, a list of voters and their preferences
### Output: The Plurality winner/winners (if there is a tie)
findPluralityWinner <- function(listy) {
  len = length(listy)
  f = length(listy[[1]])
  winnerVec <- integer(f)
  for (i in 1:len) {
    maxIndex = which.max(listy[[i]])
    winnerVec[maxIndex] = winnerVec[maxIndex] + 1 #add 1 to the candidate with the most votes
  }
  which(winnerVec == max(winnerVec)) #returns the indices of the candidates with the most votes
}

tri <- trial(3,1000)
tri
samp <- findPluralityWinner(tri)
samp

