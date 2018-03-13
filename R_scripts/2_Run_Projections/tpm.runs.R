#Expected TPM Runs for LIST of Lineup Matrices (9x input 25x25)
#Inputs: tpm.list (TPMs of Starting Lineup)

tpm.lineup.runs <- function(tpm.list){

  #Convert Lineup Matrices
  tpmc.list <- list()
  for(i in 1:9){tpmc.list[[i]] <- tpm.convert(tpm.list[[i]])}

  #Matrix of Expected Runs per Batter
  score.mat <- matrix(0,9,11)
  
  #Expected Runs
  v <- c(rep(1:9,3))
  for(j in 1:9){
    #Runs from 1st Batter
    tpm.temp <- tpmc.list[[v[j]]]
    score.mat[j,1] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                         3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 2nd Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]])
    score.mat[j,2] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                         3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 3rd Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]]%*%tpmc.list[[v[j+2]]])
    score.mat[j,3] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                         3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 4th Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]]%*%tpmc.list[[v[j+2]]]%*%tpmc.list[[v[j+3]]])
    score.mat[j,4] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                         3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 5th Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]]%*%tpmc.list[[v[j+2]]]%*%tpmc.list[[v[j+3]]]
                 %*%tpmc.list[[v[j+4]]])
    score.mat[j,5] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                         3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 6th Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]]%*%tpmc.list[[v[j+2]]]%*%tpmc.list[[v[j+3]]]
                 %*%tpmc.list[[v[j+4]]]%*%tpmc.list[[v[j+5]]])
    score.mat[j,6] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                         3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 7th Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]]%*%tpmc.list[[v[j+2]]]%*%tpmc.list[[v[j+3]]]
                 %*%tpmc.list[[v[j+4]]]%*%tpmc.list[[v[j+5]]]%*%tpmc.list[[v[j+6]]])
    score.mat[j,7] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                         3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 8th Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]]%*%tpmc.list[[v[j+2]]]%*%tpmc.list[[v[j+3]]]
                 %*%tpmc.list[[v[j+4]]]%*%tpmc.list[[v[j+5]]]%*%tpmc.list[[v[j+6]]]%*%tpmc.list[[v[j+7]]])
    score.mat[j,8] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                         3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 9th Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]]%*%tpmc.list[[v[j+2]]]%*%tpmc.list[[v[j+3]]]
                 %*%tpmc.list[[v[j+4]]]%*%tpmc.list[[v[j+5]]]%*%tpmc.list[[v[j+6]]]%*%tpmc.list[[v[j+7]]]
                 %*%tpmc.list[[v[j+8]]])
    score.mat[j,9] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                         3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 10th Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]]%*%tpmc.list[[v[j+2]]]%*%tpmc.list[[v[j+3]]]
                 %*%tpmc.list[[v[j+4]]]%*%tpmc.list[[v[j+5]]]%*%tpmc.list[[v[j+6]]]%*%tpmc.list[[v[j+7]]]
                 %*%tpmc.list[[v[j+8]]]%*%tpmc.list[[v[j+9]]])
    score.mat[j,10] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                          3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
    #Runs from 11th Batter
    tpm.temp <- (tpmc.list[[v[j]]]%*%tpmc.list[[v[j+1]]]%*%tpmc.list[[v[j+2]]]%*%tpmc.list[[v[j+3]]]
                 %*%tpmc.list[[v[j+4]]]%*%tpmc.list[[v[j+5]]]%*%tpmc.list[[v[j+6]]]%*%tpmc.list[[v[j+7]]]
                 %*%tpmc.list[[v[j+8]]]%*%tpmc.list[[v[j+9]]]%*%tpmc.list[[v[j+10]]])
    score.mat[j,11] <- (1*sum(tpm.temp[1,c(9:16,37:44,65:72)]) + 2*sum(tpm.temp[1,c(17:23,45:51,73:79)]) + 
                          3*sum(tpm.temp[1,c(24:27,52:55,80:83)]) + 4*sum(tpm.temp[1,c(28,56,84)]))
  }
  
  #OUTPUT - Expected Runs per Lineup Position
  return(rowSums(score.mat))
}
