
booleanValue <- function(booleanVector){
  value <- 0
  for (i in length(booleanVector):1){
    if(booleanVector[i]){
      value <- value + 2 ^ (length(booleanVector)-i)
    }
  }
  value
}

countValue <- function(booleanVector){
  sum(booleanVector)
}

addOne <- function(booleanVector){
  for (i in length(booleanVector):1){
    if(booleanVector[i] == FALSE){
      booleanVector[i] <- TRUE
      return(booleanVector)
    }else{
      booleanVector[i] <- FALSE
    }
  }
}

#strong won't allow for ties
createIndependentRankColumn <- function(data, numCol = 10, strong = TRUE){
  for(i in 2:(numCol + 1)){
    data[i] <- rank(data[[i]])
  }
  pointSums <- rowSums(data[seq(2,numCol+1,by=1)])
  if(strong){
    rankings <- rank(pointSums, ties.method = "first") #no ties, will be in order of original list
  }else{
    rankings <- rank(pointSums) #allow ties
  }
  return(rankings)
}


checkForIndependenceViolationsAndCyclesRandomColumns <- function(subset, setSize, numCol = 10){
  randomSelection <- sample(1:10,numCol)
  randomSelection <- c(1,randomSelection+1) #include names
  randomSubset <- subset[,randomSelection]
  return(checkForIndependenceViolationsAndCycles(randomSubset, setSize, numCol))
}

# returnGlobalScores <- function(subset, numCol = 10, strong = TRUE){
#   subset$GlobalRank <- createIndependentRankColumn(subset, numCol, strong)
#   return(subset$GlobalRank)
# }

#strong won't allow for ties
returnGlobalScores <- function(data, numCol = 10, strong = TRUE){
  for(i in 2:(numCol + 1)){
    data[i] <- rank(data[[i]])
  }
  pointSums <- rowSums(data[seq(2,numCol+1,by=1)])

  return(pointSums)
}

checkForIndependenceViolationsAndCycles <- function(subset, setSize, numCol = 10){
  results <- c(independenceViolations = 0, cycles = 0)

    
    if(independenceViolationWithinSet(subset,setSize,numCol,TRUE)){
      results["independenceViolations"] <- 1
    }
    if(cycleViolation(subset,numCol)){
      results["cycles"] <- 1
    }

  return(results)
}

checkForIndependenceViolationsAndCyclesOptimized <- function(subset, setSize, numCol = 10){
  results <- c(independenceViolations = 0, cycles = 0)
  if (independenceViolationWithinSet(subset,setSize,numCol,FALSE)){ #strong independence or cycle implies weak independence
    
    if(independenceViolationWithinSet(subset,setSize,numCol,TRUE)){
      results["independenceViolations"] <- 1
    }
    if(cycleViolation(subset,numCol)){
      results["cycles"] <- 1
    }
  }
  return(results)
}
containsTies <- function(column){
  return(length(unique(column)) != length((column)))
}

independenceViolationWithinSet <- function(fullSet, setSize, numCol = 10, strong = TRUE){
  fullSet$GlobalRank <- createIndependentRankColumn(fullSet, numCol, FALSE)

  for(i in 1:setSize){
    subset <- fullSet[-i,]
    subset$IndependentRank <- createIndependentRankColumn(subset, numCol,FALSE)
    subset$DependentRank <- rank(subset$GlobalRank)
    if(!all(subset$DependentRank == subset$IndependentRank)){
      if(strong){
        if(containsTies(subset$DependentRank) | containsTies(subset$IndependentRank)){ #no weak violations
          #do nothing because there could still be violatins on other pairings
        }else{
          return(TRUE)
        }
      } else{
        return(TRUE)
      }
    }
  }
   return(FALSE)
}



independenceViolationWithinSetFLAWEDProducesWeakViolations <- function(fullSet, setSize, numCol = 10, strong = TRUE){
  fullSet$GlobalRank <- createIndependentRankColumn(fullSet, numCol, strong)
  for(i in 1:setSize){
    subset <- fullSet[-i,]
    subset$IndependentRank <- createIndependentRankColumn(subset, numCol,strong)
    if(strong){
      subset$DependentRank <- rank(subset$GlobalRank, ties.method = "first")
    }else{
      subset$DependentRank <- rank(subset$GlobalRank)
    }
    if(!all(subset$DependentRank == subset$IndependentRank)){
      return(TRUE)
    }
  }
   return(FALSE)
}

# independenceViolation <- function(subset){
#   subset$IndependentRank <- createIndependentRankColumn(subset)
#   subset$DependentRank <- rank(subset$GlobalRank, ties.method = "first")
#   return(!all(subset$DependentRank == subset$IndependentRank))
# }

participant1BeatsParticipant2 <- function(participant1, participant2,numCol = 10){
  participants <- bind_rows(participant1,participant2)
  participants$IndependentRank <- createIndependentRankColumn(participants,numCol, FALSE)
  return(participants$IndependentRank[1] < participants$IndependentRank[2]) #< means 1 beats 2
}

cycleViolation <- function(subset, numCol = 10){
  participantA <- subset[1,]
  participantB <- subset[2,]
  participantC <- subset[3,]
  
  wins <- c(0,0,0)
  losses <- c(0,0,0)
  if(participant1BeatsParticipant2(participantA,participantB, numCol)){
    wins[1] <- wins[1] + 1
    losses[2] <- losses[2] + 1
  } else if(participant1BeatsParticipant2(participantB,participantA, numCol)){
    wins[2] <- wins[2] + 1
    losses[1] <- losses[1] + 1
  }
  if(participant1BeatsParticipant2(participantA,participantC, numCol)){
    wins[1] <- wins[1] + 1
    losses[3] <- losses[3] + 1
  } else if(participant1BeatsParticipant2(participantC,participantA, numCol)){
    wins[3] <- wins[3] + 1
    losses[1] <- losses[1] + 1
  }
  if(participant1BeatsParticipant2(participantB,participantC, numCol)){
    wins[2] <- wins[2] + 1
    losses[3] <- losses[3] + 1
  } else if(participant1BeatsParticipant2(participantC,participantB, numCol)){
    wins[3] <- wins[3] + 1
    losses[2] <- losses[2] + 1
  }
  if((losses[1]==0) | (losses[2]==0) | (losses[3]==0)){
    return(FALSE)
  }
  return(TRUE)
  
}

getNextColumn3 <- function(startingIterators){
  iStart <- startingIterators[1]
  jStart <- startingIterators[2]
  kStart <- startingIterators[3]
  firstTime <- TRUE
  i <- iStart
  while(i <= 3){
    j <- 1
    while(j <= 3){
      if(firstTime){
        j <- jStart
      }
      k <- 1
      while(k <= 3){
        if((i != j) & (j != k) & (k != i)){
          if(firstTime){
            k <- kStart
            firstTime <- FALSE
          }else{
            values <- c(i,j,k)
            return (values)
          }
        }
        k <- k + 1
      }
      j <- j + 1
    }
    i <- i + 1
  }
}



