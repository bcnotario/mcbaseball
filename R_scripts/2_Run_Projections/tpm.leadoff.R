#Expected TPM Leadoff for LIST of Lineup Matrices
#Inputs: tpm.list (List of 25x25 TPMs of Starting Lineup)
#Packages: expm

tpm.lineup.lead <- function(tpm.list){
  
  #Probability of i Batters Faced by Leadoff Position
  tpm.list <- c(tpm.list,tpm.list,tpm.list)
  nbat.mat <- matrix(0,9,9); colnames(nbat.mat)<-c(3:11)
  for(i in 1:9){
    prob.v <- vector()
    prob.v[1] <- (tpm.list[[i]]%*%tpm.list[[i+1]]%*%tpm.list[[i+2]])[1,25]
    prob.v[2] <- (tpm.list[[i]]%*%tpm.list[[i+1]]%*%tpm.list[[i+2]]%*%tpm.list[[i+3]])[1,25]
    prob.v[3] <- (tpm.list[[i]]%*%tpm.list[[i+1]]%*%tpm.list[[i+2]]%*%tpm.list[[i+3]]%*%tpm.list[[i+4]])[1,25]
    prob.v[4] <- (tpm.list[[i]]%*%tpm.list[[i+1]]%*%tpm.list[[i+2]]%*%tpm.list[[i+3]]%*%tpm.list[[i+4]]
                  %*%tpm.list[[i+5]])[1,25]
    prob.v[5] <- (tpm.list[[i]]%*%tpm.list[[i+1]]%*%tpm.list[[i+2]]%*%tpm.list[[i+3]]%*%tpm.list[[i+4]]
                  %*%tpm.list[[i+5]]%*%tpm.list[[i+6]])[1,25]
    prob.v[6] <- (tpm.list[[i]]%*%tpm.list[[i+1]]%*%tpm.list[[i+2]]%*%tpm.list[[i+3]]%*%tpm.list[[i+4]]
                  %*%tpm.list[[i+5]]%*%tpm.list[[i+6]]%*%tpm.list[[i+7]])[1,25]
    prob.v[7] <- (tpm.list[[i]]%*%tpm.list[[i+1]]%*%tpm.list[[i+2]]%*%tpm.list[[i+3]]%*%tpm.list[[i+4]]
                  %*%tpm.list[[i+5]]%*%tpm.list[[i+6]]%*%tpm.list[[i+7]]%*%tpm.list[[i+8]])[1,25]
    prob.v[8] <- (tpm.list[[i]]%*%tpm.list[[i+1]]%*%tpm.list[[i+2]]%*%tpm.list[[i+3]]%*%tpm.list[[i+4]]
                  %*%tpm.list[[i+5]]%*%tpm.list[[i+6]]%*%tpm.list[[i+7]]%*%tpm.list[[i+8]]%*%tpm.list[[i+9]])[1,25]
    prob.v[9] <- (tpm.list[[i]]%*%tpm.list[[i+1]]%*%tpm.list[[i+2]]%*%tpm.list[[i+3]]%*%tpm.list[[i+4]]
                  %*%tpm.list[[i+5]]%*%tpm.list[[i+6]]%*%tpm.list[[i+7]]%*%tpm.list[[i+8]]%*%tpm.list[[i+9]]
                  %*%tpm.list[[i+10]])[1,25]
    nbat.mat[i,] <- c(prob.v[1],prob.v[2]-prob.v[1],prob.v[3]-prob.v[2],prob.v[4]-prob.v[3],prob.v[5]-prob.v[4],
                      prob.v[6]-prob.v[5],prob.v[7]-prob.v[6],prob.v[8]-prob.v[7],prob.v[9]-prob.v[8])
  }
  nbat.mat[,9] <- 1-rowSums(nbat.mat[,1:8])
  
  #Transition Probability Matrix by Lineup Position
  tpm.mat <- matrix(0,9,9)
  for(LID in 1:6){tpm.mat[LID,]<-nbat.mat[LID,c((9-LID-1):9,1:(9-LID-2))]}
  tpm.mat[7,]<-nbat.mat[7,c(1:9)]
  tpm.mat[8,]<-nbat.mat[8,c(9,1:8)]
  tpm.mat[9,]<-nbat.mat[9,c(8:9,1:7)]
  
  #Probability of Leading Off by Lineup Position
  lead.temp <- tpm.mat+tpm.mat%^%2+tpm.mat%^%3+tpm.mat%^%4+tpm.mat%^%5+tpm.mat%^%6+tpm.mat%^%7+tpm.mat%^%8
  lead.prob <- c(lead.temp[1,1]+1,lead.temp[1,2:9])/9
  
  #OUTPUT - Leadoff Probabilities
  return(lead.prob)
}
