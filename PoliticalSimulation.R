# R Simulation of Elections
# The goal of this simulation is to see how changing the number of
# voters and candidates changes relationships between voting methods
# For example: As the number of candidates n --> inf, what is the probability that the 
#              Borda Count Winner is the same as the Plurality Winner

#---------------------------------------------------------------------------------------#
########      TO DO:                                                             ########
########            - define functions to simulate other voting systems          ########
########            - determine what to do in the case of a tie                  ########
########            - write function to find pairwise winner                     ########
########            - change storage of voters from lists of vectors to a matrix ########
########            - find a way to simulate strategic voting (teaming in Borda) ########
#---------------------------------------------------------------------------------------#


### Function: prefOrder
### Inputs: X, a vector of values
### Output: a random preference ordering of X, size(X) is most preferred and 1 is least preferred 
prefOrder <- function(x) {
  sample(x) #return a random preference ordering
}


### Function: Trial
### Inputs: N, the number of candidates that can be voted for
###         X, the number of voters
### Output: Random perference orderings for X voters, voting for N candidates
trial <- function(n, x) {
  base <- c(1:n) #Create a vector with values from 1 to n
  voters <- matrix(0, nrow = x, ncol = length(prefOrder(base)), byrow = TRUE)
  for (row in 1:nrow(voters)) {
    voters[row,] = prefOrder(base)
  }
  voters
}

### Function: findBordaCount
### Inputs: LISTY, a list of voters and their preferences
### Output: The Borda Counts for each candidate
findBordaCount <- function(matrixy) {
  len = ncol(matrixy)
  voteCount <- integer(len) #make a vector to keep track of vote totals for each candidate
  numVoters <- nrow(matrixy)
  for (i in 1:numVoters) {
    for (j in 1:len) {
      voteCount[j] = voteCount[j] + matrixy[i,j] #add the current voter's points
    }
  }
 voteCount #return the vector containing each candidates point count
}

### Function: bordaCountWinner
### returns the Borda Count Winner
bordaCountWinner <- function(matrixy) {
  which.max(findBordaCount(matrixy)) #returns a randomly chosen winner if there is a tie
}

### Function: findPluralityWinner
### Inputs: LISTY, a list of voters and their preferences
### Output: The Plurality winner/winners (if there is a tie)
findPluralityWinner <- function(matrixy) {
  len = nrow(matrixy)
  f = ncol(matrixy)
  winnerVec <- integer(f) #create a vector keeping track of how many votes each candidate has
  for (i in 1:len) {
    maxIndex = which.max(matrixy[i,])
    winnerVec[maxIndex] = winnerVec[maxIndex] + 1 #add 1 to the voter's top candidate
  }
  which.max(winnerVec)
  # which(winnerVec == max(winnerVec)) #returns the indices of the candidates with the most votes
}


isWinnerSame <- function(n,x)
{
  myTrial <- trial(n,x)
  findPluralityWinner(myTrial) == bordaCountWinner(myTrial)
}


xtc <- NULL
buildMatrix <- function(n,x) {
mat <- matrix(0, nrow = n, ncol = x + 1)
vec <- 10^(0:x)
for (i in 1:n) {
  t = 1
  for (j in vec) {
    test <-  t(replicate(1000,isWinnerSame(i,j)))
    mat[i, t] = mean(test)
    t = t + 1
  }
}
mat
}


test <-  t(replicate(10000,isWinnerSame(3,10))) #test with 10,000 trials
 mean(test)  #returns proportion of trials that had same Borda Count winner and Plurality winner
