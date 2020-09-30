
library(tidyverse)
library(RJSONIO)
library(data.table)
library(foreach)
library(doSNOW)

workingDirectory <- "S:/Shared drives/SU Sport Analytics Shared Drive/RankSumProjectShaneJustin/parallel"
setwd(workingDirectory)
numCores <- 24
cl<-makeCluster(numCores) #change to your number of CPU cores
registerDoSNOW(cl)

source("../lib/rankSum.R")








total <- 6 ^ 10
n <- 1
subsetsIndependenceViolations <- list()
subsetsCycleViolations <- list()


#n <- 2 #don't start with empty set
start_time <- Sys.time()







matrix<- foreach(i=1:numCores) %dopar% {
countNumberIndependenceViolations3 <- 0
countCycleViolations3 <- 0
totalPossible3 <- 0

countNumberIndependenceViolations4 <- 0
countCycleViolations4 <- 0
totalPossible4 <- 0

countNumberIndependenceViolations5 <- 0
countCycleViolations5 <- 0
totalPossible5 <- 0

countNumberIndependenceViolations6 <- 0
countCycleViolations6 <- 0
totalPossible6 <- 0

countNumberIndependenceViolations7 <- 0
countCycleViolations7 <- 0
totalPossible7 <- 0

countNumberIndependenceViolations8 <- 0
countCycleViolations8 <- 0
totalPossible8 <- 0

countNumberIndependenceViolations9 <- 0
countCycleViolations9 <- 0
totalPossible9 <- 0

countNumberIndependenceViolations10 <- 0
countCycleViolations10 <- 0
totalPossible10 <- 0

library(tidyverse)



names <- c("posA","posB","posC")
core <- i - 1 #need core to start at 0 for mod to work
#setwd("G:/Shared drives/SU Sport Analytics Shared Drive/RankSumProjectShaneJustin/parallel")
#setwd("G:/Shared drives/SU Sport Analytics Shared Drive/RankSumProjectShaneJustin/parallel")
c1 <- c(1,2,3)
c2 <- c(1,2,3)
c3 <- c(1,2,3)
c4 <- c(1,2,3)
c5 <- c(1,2,3)
c6 <- c(1,2,3)
c7 <- c(1,2,3)
c8 <- c(1,2,3)
c9 <- c(1,2,3)
c10<- c(1,2,3)
for(i1 in 1:6){
  c2 <- c(1,2,3)
  for(i2 in 1:6){
    c3 <- c(1,2,3)
    for(i3 in 1:6){
      totalPossible3 <- totalPossible3 + 1
      if((totalPossible3 %% numCores) == core){
        subset <- tibble(names,c1,c2,c3)
        violations <- checkForIndependenceViolationsAndCycles(subset,3,3)
        countNumberIndependenceViolations3 <- countNumberIndependenceViolations3 + violations["independenceViolations"]
        countCycleViolations3 <- countCycleViolations3 + violations["cycles"]
      }
      c4 <- c(1,2,3)
      for(i4 in 1:6){
        totalPossible4 <- totalPossible4 + 1
        if((totalPossible4 %% numCores) == core){
          subset <- tibble(names,c1,c2,c3,c4)
          violations <- checkForIndependenceViolationsAndCycles(subset,3,4)
          countNumberIndependenceViolations4 <- countNumberIndependenceViolations4 + violations["independenceViolations"]
          countCycleViolations4 <- countCycleViolations4 + violations["cycles"]
        }
        c5 <- c(1,2,3)
        for(i5 in 1:6){
          totalPossible5 <- totalPossible5 + 1
          if((totalPossible5 %% numCores) == core){
            subset <- tibble(names,c1,c2,c3,c4,c5)
            violations <- checkForIndependenceViolationsAndCycles(subset,3,5)
            countNumberIndependenceViolations5 <- countNumberIndependenceViolations5 + violations["independenceViolations"]
            countCycleViolations5 <- countCycleViolations5 + violations["cycles"]
          }
          c6 <- c(1,2,3)
          for(i6 in 1:6){
            totalPossible6 <- totalPossible6 + 1
            if((totalPossible6 %% numCores) == core){
              subset <- tibble(names,c1,c2,c3,c4,c5,c6)
              violations <- checkForIndependenceViolationsAndCycles(subset,3,6)
              countNumberIndependenceViolations6 <- countNumberIndependenceViolations6 + violations["independenceViolations"]
              countCycleViolations6 <- countCycleViolations6 + violations["cycles"]
            }
            c7 <- c(1,2,3)
            for(i7 in 1:6){
              totalPossible7 <- totalPossible7 + 1
              if((totalPossible7 %% numCores) == core){
                subset <- tibble(names,c1,c2,c3,c4,c5,c6,c7)
                violations <- checkForIndependenceViolationsAndCycles(subset,3,7)
                countNumberIndependenceViolations7 <- countNumberIndependenceViolations7 + violations["independenceViolations"]
                countCycleViolations7 <- countCycleViolations7 + violations["cycles"]
              }
              c8 <- c(1,2,3)
              for(i8 in 1:6){
                totalPossible8 <- totalPossible8 + 1
                if((totalPossible8 %% numCores) == core){
                  subset <- tibble(names,c1,c2,c3,c4,c5,c6,c7,c8)
                  violations <- checkForIndependenceViolationsAndCycles(subset,3,8)
                  countNumberIndependenceViolations8 <- countNumberIndependenceViolations8 + violations["independenceViolations"]
                  countCycleViolations8 <- countCycleViolations8 + violations["cycles"]
                }
                c9<- c(1,2,3)
                for(i9 in 1:6){
                  totalPossible9 <- totalPossible9 + 1
                  if((totalPossible9 %% numCores) == core){
                    subset <- tibble(names,c1,c2,c3,c4,c5,c6,c7,c8,c9)
                    violations <- checkForIndependenceViolationsAndCycles(subset,3,9)
                    countNumberIndependenceViolations9 <- countNumberIndependenceViolations9 + violations["independenceViolations"]
                    countCycleViolations9 <- countCycleViolations9 + violations["cycles"]
                  }
                  c10 <- c(1,2,3)
                  for(i10 in 1:6){
                    totalPossible10 <- totalPossible10 + 1
                    if((totalPossible10 %% 100000)==0){ #limit times progress bar is set
                      cat(paste(totalPossible10/total," % finished and found ", countNumberIndependenceViolations10 , " independence violations and " , countCycleViolations10, " cycles out of ", totalPossible10, sep = ""), file = paste(core,"coreStatusTripletSimulation.txt", sep = ""))
                    }
                    if((totalPossible10 %% numCores) == core){
                      subset <- tibble(names,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)
                      violations <- checkForIndependenceViolationsAndCycles(subset,3,10)
                      countNumberIndependenceViolations10 <- countNumberIndependenceViolations10 + violations["independenceViolations"]
                      countCycleViolations10 <- countCycleViolations10 + violations["cycles"]
                    }

                    c10 <- getNextColumn3(c10)
                  }
                  c9 <- getNextColumn3(c9)
                }
                c8 <- getNextColumn3(c8)
              }
              c7 <- getNextColumn3(c7)
            }
            c6 <- getNextColumn3(c6)
          }
          c5 <- getNextColumn3(c5)
        }
        c4 <- getNextColumn3(c4)
      }
      c3 <- getNextColumn3(c3)
    }
    c2 <- getNextColumn3(c2)
  }
  c1 <- getNextColumn3(c1)
}
#c(countNumberIndependenceViolations=countNumberIndependenceViolations,countCycleViolations=countCycleViolations,totalPossible=totalPossible)

c(countNumberIndependenceViolations3,countCycleViolations3,totalPossible3,
  countNumberIndependenceViolations4,countCycleViolations4,totalPossible4,
  countNumberIndependenceViolations5,countCycleViolations5,totalPossible5,
  countNumberIndependenceViolations6,countCycleViolations6,totalPossible6,
  countNumberIndependenceViolations7,countCycleViolations7,totalPossible7,
  countNumberIndependenceViolations8,countCycleViolations8,totalPossible8,
  countNumberIndependenceViolations9,countCycleViolations9,totalPossible9,
  countNumberIndependenceViolations10,countCycleViolations10,totalPossible10)

}




# while(n < total){
#   subset <- cityDataRanks[selection,]
#   if((n %% 100000)==0){ #limit times progress bar is set
#     setTxtProgressBar(pb, n)
#     print(paste(n/total," % finished and found ", countNumberIndependenceViolations , " independence violations and " , countCycleViolations, " cycles out of ", n , " possible.", sep = ""))
#   }
#   if(nrow(subset)==3){
#     totalPossible <- totalPossible + 1
#     if (independenceViolation(subset)){
#       countNumberIndependenceViolations <- countNumberIndependenceViolations + 1
#       subsetsIndependenceViolations[[length(subsetsIndependenceViolations)+1]] <- subset
#       if(cycleViolation(subset)){
#         countCycleViolations <- countCycleViolations + 1
#         subsetsCycleViolations[[length(subsetsCycleViolations)+1]] <- subset
#       }
#     }
#   }
#   selection <- addOne(selection)
#   n <- n + 1
# }


end_time <- Sys.time()
end_time - start_time



as.data.frame(matrix) -> results
results <- rowSums(results)

countNumberIndependenceViolations3 <- results[[1]]
countCycleViolations3 <- results[[2]]
totalPossible3 <- results[[3]] / numCores

countNumberIndependenceViolations4 <- results[[4]]
countCycleViolations4 <- results[[5]]
totalPossible4 <- results[[6]] / numCores

countNumberIndependenceViolations5 <- results[[7]]
countCycleViolations5 <- results[[8]]
totalPossible5 <- results[[9]] / numCores

countNumberIndependenceViolations6 <- results[[10]]
countCycleViolations6 <- results[[11]]
totalPossible6 <- results[[12]] / numCores

countNumberIndependenceViolations7 <- results[[13]]
countCycleViolations7 <- results[[14]]
totalPossible7 <- results[[15]] / numCores

countNumberIndependenceViolations8 <- results[[16]]
countCycleViolations8 <- results[[17]]
totalPossible8 <- results[[18]] / numCores

countNumberIndependenceViolations9 <- results[[19]]
countCycleViolations9 <- results[[20]]
totalPossible9 <- results[[21]] / numCores

countNumberIndependenceViolations10 <- results[[22]]
countCycleViolations10 <- results[[23]]
totalPossible10 <- results[[24]] / numCores

results <- tibble(NumberIndependenceViolations3 = countNumberIndependenceViolations3, NumberCycleViolations3 = countCycleViolations3, NumberPossible3 = totalPossible3,
                  NumberIndependenceViolations4 = countNumberIndependenceViolations4, NumberCycleViolations4 = countCycleViolations4, NumberPossible4 = totalPossible4,
                  NumberIndependenceViolations5 = countNumberIndependenceViolations5, NumberCycleViolations5 = countCycleViolations5, NumberPossible5 = totalPossible5,
                  NumberIndependenceViolations6 = countNumberIndependenceViolations6, NumberCycleViolations6 = countCycleViolations6, NumberPossible6 = totalPossible6,
                  NumberIndependenceViolations7 = countNumberIndependenceViolations7, NumberCycleViolations7 = countCycleViolations7, NumberPossible7 = totalPossible7,
                  NumberIndependenceViolations8 = countNumberIndependenceViolations8, NumberCycleViolations8 = countCycleViolations8, NumberPossible8 = totalPossible8,
                  NumberIndependenceViolations9 = countNumberIndependenceViolations9, NumberCycleViolations9 = countCycleViolations9, NumberPossible9 = totalPossible9,
                  NumberIndependenceViolations10 = countNumberIndependenceViolations10, NumberCycleViolations10 = countCycleViolations10, NumberPossible10 = totalPossible10)
results %>% write_csv("../data/resultsTheoreticalParallelTripletsOnly.csv")


stopCluster(cl)


