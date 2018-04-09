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
#---------------------------------------------------------------------------------------#


### Function: prefOrder
### Inputs: X, a vector of values
### Output: a random preference ordering of X, 5 is most preferred and 1 is least preferred 
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
    voters[[i]] = prefOrder(base) #give the voter a random preference
  voters
}

### Function: findBordaCount
### Inputs: LISTY, a list of voters and their preferences
### Output: The Borda Counts for each candidate
findBordaCount <- function(listy) {
  f = length(listy[[1]])
  voteCount <- integer(f) #make a vector to keep track of vote totals for each candidate
  for (i in 1:length(listy)) {
    for (j in 1:f) {
      voteCount[j] = voteCount[j] + listy[[i]][j] #add the current voter's points
    }
  }
 voteCount #return the vector containing each candidates point count
}

### Function: bordaCountWinner
### returns the Borda Count Winner
bordaCountWinner <- function(listy) {
  which.max(findBordaCount(listy)) #returns a randomly chosen winner if there is a tie
}

### Function: findPluralityWinner
### Inputs: LISTY, a list of voters and their preferences
### Output: The Plurality winner/winners (if there is a tie)
findPluralityWinner <- function(listy) {
  len = length(listy)
  f = length(listy[[1]])
  winnerVec <- integer(f) #create a vector keeping track of how many votes each candidate has
  for (i in 1:len) {
    maxIndex = which.max(listy[[i]])
    winnerVec[maxIndex] = winnerVec[maxIndex] + 1 #add 1 to the voter's top candidate
  }
  which(winnerVec == max(winnerVec)) #returns the indices of the candidates with the most votes
}

isWinnerSame <- function(listy)
{
  findPluralityWinner == bordaCountWinner
}

#test <-  t(replicate(10000,isWinnerSame(3,10))) #test with 10,000 trials
# mean(test)  #returns proportion of trials that had same Borda Count winner and Plurality winner