
library(foreach)
library(doSNOW)
library(tidyverse)

source("../lib/rankSum.R")
numCores <- 24
cl<-makeCluster(numCores) #change to your number of CPU cores
registerDoSNOW(cl)

numRandomColumnSelections <- 2




cityData2017 <- read_csv("../data/MSA2017.csv")


#n <- 2 #don't start with empty set
start_time <- Sys.time()

matrix<- foreach(i=1:numCores) %dopar% {
  core <- i - 1 #need core to start at 0 for mod to work
  library(tidyverse)

  setwd(workingDirectory)
  
  

 
  

  countNumberIndependenceViolations <- rep(0,10)
  countCycleViolations <- rep(0,10)
  totalPossible <- rep(0,10)
  n <- 0
  total <- ncol(combn(40, 3))
  
  #cityDataRanks should only have columns that have rankings and whose first column is MSA
  cityDataRanks <- cityData2017[,c(1,seq(4,22,by=2))]
  
  
  #remove column ties by random
  for(i in 2:11){
    cityDataRanks[,i] <- rank(cityDataRanks %>% select(i) %>% pull(), ties.method = "random")
  }
  
  
  for(i in 1:40){
    for(j in i:40){
      for(k in j:40){
        if((i != j) & (j != k) & (k != i)){
          n <- n + 1 
          subset <- cityDataRanks[c(i,j,k),]
           
          if((n %% 100)==0){ #limit times progress bar is set
            cat(paste(n/total," % finished and found ", countNumberIndependenceViolations[10] , " independence violations and " , countCycleViolations[10], " cycles out of ", totalPossible[10], sep = ""), file = paste("status/",core, "coreEmpericalFullStatusTriplet.txt", sep = ""))
          }
          
          if((n %% numCores) == core){
            for(nColumns in 3:10){
              combinationsOfColumns <- combn(10, nColumns)
              for(combinationN in 1:ncol(combinationsOfColumns)){
                totalPossible[nColumns] <- totalPossible[nColumns] + 1
                selection <- combinationsOfColumns[,combinationN]
                selection <- c(1, selection + 1) #include city names
                selectedColumnsSubset <- subset[selection]
                
                #violations <- checkForIndependenceViolationsAndCycles(subset,3,nColumns)
                violations <- checkForIndependenceViolationsAndCycles(selectedColumnsSubset,3,nColumns)
                countNumberIndependenceViolations[nColumns] <- countNumberIndependenceViolations[nColumns] + violations["independenceViolations"]
                countCycleViolations[nColumns] <- countCycleViolations[nColumns] + violations["cycles"]
              }
            }
          }
        }
      }
    }
  }
  
  

  c(countNumberIndependenceViolations[3],countCycleViolations[3],totalPossible[3],
    countNumberIndependenceViolations[4],countCycleViolations[4],totalPossible[4],
    countNumberIndependenceViolations[5],countCycleViolations[5],totalPossible[5],
    countNumberIndependenceViolations[6],countCycleViolations[6],totalPossible[6],
    countNumberIndependenceViolations[7],countCycleViolations[7],totalPossible[7],
    countNumberIndependenceViolations[8],countCycleViolations[8],totalPossible[8],
    countNumberIndependenceViolations[9],countCycleViolations[9],totalPossible[9],
    countNumberIndependenceViolations[10],countCycleViolations[10],totalPossible[10])
}
end_time <- Sys.time()
end_time - start_time


as.data.frame(matrix) -> results
results <- rowSums(results)


countNumberIndependenceViolations3 <- results[[1]]
countCycleViolations3 <- results[[2]]
totalPossible3 <- results[[3]]

countNumberIndependenceViolations4 <- results[[4]]
countCycleViolations4 <- results[[5]]
totalPossible4 <- results[[6]]

countNumberIndependenceViolations5 <- results[[7]]
countCycleViolations5 <- results[[8]]
totalPossible5 <- results[[9]]

countNumberIndependenceViolations6 <- results[[10]]
countCycleViolations6 <- results[[11]]
totalPossible6 <- results[[12]]

countNumberIndependenceViolations7 <- results[[13]]
countCycleViolations7 <- results[[14]]
totalPossible7 <- results[[15]]

countNumberIndependenceViolations8 <- results[[16]]
countCycleViolations8 <- results[[17]]
totalPossible8 <- results[[18]]

countNumberIndependenceViolations9 <- results[[19]]
countCycleViolations9 <- results[[20]]
totalPossible9 <- results[[21]]

countNumberIndependenceViolations10 <- results[[22]]
countCycleViolations10 <- results[[23]]
totalPossible10 <- results[[24]]

results <- tibble(NumberIndependenceViolations3 = countNumberIndependenceViolations3, NumberCycleViolations3 = countCycleViolations3, NumberPossible3 = totalPossible3,
                  NumberIndependenceViolations4 = countNumberIndependenceViolations4, NumberCycleViolations4 = countCycleViolations4, NumberPossible4 = totalPossible4,
                  NumberIndependenceViolations5 = countNumberIndependenceViolations5, NumberCycleViolations5 = countCycleViolations5, NumberPossible5 = totalPossible5,
                  NumberIndependenceViolations6 = countNumberIndependenceViolations6, NumberCycleViolations6 = countCycleViolations6, NumberPossible6 = totalPossible6,
                  NumberIndependenceViolations7 = countNumberIndependenceViolations7, NumberCycleViolations7 = countCycleViolations7, NumberPossible7 = totalPossible7,
                  NumberIndependenceViolations8 = countNumberIndependenceViolations8, NumberCycleViolations8 = countCycleViolations8, NumberPossible8 = totalPossible8,
                  NumberIndependenceViolations9 = countNumberIndependenceViolations9, NumberCycleViolations9 = countCycleViolations9, NumberPossible9 = totalPossible9,
                  NumberIndependenceViolations10 = countNumberIndependenceViolations10, NumberCycleViolations10 = countCycleViolations10, NumberPossible10 = totalPossible10)
results %>% write_csv("../data/resultsFUllAnalysisParallelTripletsOnly.csv")


stopCluster(cl)
