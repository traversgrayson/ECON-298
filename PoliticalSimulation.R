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
install.packages("xtable")
library("xtable")

### Function: prefOrder
### Inputs: X, some integer
### Output: a vector of X values between 0 and 100 
prefOrder <- function(x) {
  runif(n = x, min = 0, max = 100) #return random utilities for candidates
}


### Function: Trial
### Inputs: N, the number of candidates that can be voted for
###         X, the number of voters
### Output: Random utility values for X voters, voting for N candidates
trial <- function(n, x) {
  base <- c(1:n) #Create a vector with values from 1 to n
  voters <- matrix(0, nrow = x, ncol = n, byrow = TRUE)
  for (row in 1:nrow(voters)) {
    voters[row,] = prefOrder(base) # Give each voter random utilities for each candidate
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
    matrixy[i,] = rank(matrixy[i,])
    for (j in 1:len) {
      voteCount[j] = (voteCount[j] - 1) + matrixy[i,j] #add the current voter's points
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
findPluralityCount <- function(matrixy) {
  len = nrow(matrixy)
  f = ncol(matrixy)
  winnerVec <- integer(f) #create a vector keeping track of how many votes each candidate has
  for (i in 1:len) {
    maxIndex = which.max(matrixy[i,])
    winnerVec[maxIndex] = winnerVec[maxIndex] + 1 #add 1 to the voter's top candidate
  }
  winnerVec
  ##which.max(winnerVec)
  # which(winnerVec == max(winnerVec)) #returns the indices of the candidates with the most votes
}

findPluralityWinner <- function(matrixy) {
  which.max(findPluralityCount(matrixy))
}

makeProbVec <- function(matrixy,func) {
  vec <- func(matrixy)
  prob <- c()
  len <- length(vec)
  summy <- sum(vec)
  for (i in 1:len) {
    prob[i] = vec[i]/summy
    prob[i] = min(max(rnorm(1,mean = prob[i], sd = prob[i]/2),0), 1)
  }
  summy <- sum(prob)
  prob = prob/summy
  
  prob
}

createDishonestVoters <- function(matrixy,probVec) {
  for (i in 1: nrow(matrixy)) {
    matrixy[i,] = matrixy[i,] * probVec
  }
  matrixy
}

isWinnerSame <- function(n,x)
{
  myTrial <- trial(n,x)
  findPluralityWinner(myTrial) == bordaCountWinner(myTrial)
}

isWinnerOptimal <- function(n,x,func)
{
  t <- trial(n,x)
  func(t) == findOptimalWinner(t)
}


buildMatrix <- function(n,x) {
mat <- matrix(0, nrow = n, ncol = x + 1)
vec <- 10^(0:x)
for (i in 1:n) {
  t = 1
  for (j in vec) {
    test <-  t(replicate(1000,isWinnerOptimal(i,j,findScoreWinner)))
    mat[i, t] = mean(test)
    t = t + 1
  }
}
mat
}


findOptimalWinner <- function(matrixy) {
  len = ncol(matrixy)
  voteCount <- integer(len) #make a vector to keep track of vote totals for each candidate
  numVoters <- nrow(matrixy)
  for (i in 1:numVoters) {
    for (j in 1:len) {
      voteCount[j] = voteCount[j] + matrixy[i,j] #add the current voter's points
    }
  }
  which.max(voteCount) #return the vector containing each candidates point count
}

findScoreCount <- function(matrixy) {
  len = ncol(matrixy)
  scoreCount <- integer(len) #make a vector to keep track of score total
  numVoters <- nrow(matrixy)
  for (i in 1:numVoters) {
    for (j in 1:len) {
      scoreCount[j] = scoreCount[j] + matrixy[i,j] #add the current voter's utility
    }
  }
  scoreCount #return the vector containing each candidates point count
}

findScoreWinner <- function(matrixy) {
  which.max(findScoreCount(matrixy))
}



butt <- function() {
  t <- trial(4,101000)
  myVec <- makeProbVec(t,findPluralityCount)
  d <- createDishonestVoters(t,myVec)
  return (findPluralityWinner(d) == findPluralityWinner(t))
}

mean(t(replicate(100,butt())))
