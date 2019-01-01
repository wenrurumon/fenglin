
########################
# Processing
########################

rm(list=ls())
library(data.table)
library(dplyr)
library(HMM)
setwd('/Users/wenrurumon/Downloads/data')
load('4hmm.rda')

state <- paste(unique(data2day$trate))

Symbols <- paste(unique(data2day$obs))

startprob <- prob.orihid$n

transpro <- do.call(cbind,lapply(0:2,function(i){
  filter(prob.hid2hid,lrate==i)$n
})); transpro
transpro <- transpro / rowSums(transpro)
transpro[2:3] <- transpro[2:3] / sum(transpro[2:3]) * (1-transpro[1])
transpro[6] <- 1 - transpro[3] - transpro[9] 
transpro[8] <- 1 - transpro[9] - transpro[7]
transpro[5] <- 1 - rowSums(transpro)[2] + transpro[5]

emisspr <- do.call(rbind,lapply(0:2,function(i){
  x <- filter(prob.hid2obs,trate==i)
  x <- x$n[match(Symbols,x$obs)]
  x[is.na(x)] <- 0
  x/sum(x)
}))

system.time(hmm <- initHMM(States = state,Symbols = Symbols,startProbs = startprob,transProbs = transpro,
                           emissionProbs = emisspr))
pids <- unique(data2day$pid)

########################
# Modeling
########################

system.time(
  pred <- lapply(1:length(pids),function(j){
    print(j)
    oberi <- paste(filter(data2day,pid==pids[j])$obs)
    ratei <- paste(filter(data2day,pid==pids[j])$trate)
    cbind(pid=pids[j],trate=ratei,pred=viterbi(hmm,oberi))
  })
)

pred <- do.call(rbind,pred)
mean(pred[,2]==pred[,3])


########################
# HMM decoding
########################

hmm <- hmm
observation <- oberi
maxIterations <- 100
delta <- 1e-09
pseudoCount <- 0

tempHmm = hmm
tempHmm$transProbs[is.na(hmm$transProbs)] = 0
tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
diff = c()
for (i in 1:maxIterations) {
  bw = baumWelchRecursion(tempHmm, observation)
  T = bw$TransitionMatrix
  E = bw$EmissionMatrix
  T[!is.na(hmm$transProbs)] = T[!is.na(hmm$transProbs)] + 
    pseudoCount
  E[!is.na(hmm$emissionProbs)] = E[!is.na(hmm$emissionProbs)] + 
    pseudoCount
  T = (T/apply(T, 1, sum))
  E = (E/apply(E, 1, sum))
  d = sqrt(sum((tempHmm$transProbs - T)^2)) + sqrt(sum((tempHmm$emissionProbs - 
                                                          E)^2))
  diff = c(diff, d)
  tempHmm$transProbs = T
  tempHmm$emissionProbs = E
  if (d < delta) {
    break
  }
}
tempHmm$transProbs[is.na(hmm$transProbs)] = NA
tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = NA
return(list(hmm = tempHmm, difference = diff))

