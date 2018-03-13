#Fixed Lineup Run Simulation
#Inputs: n (n Game Simulations), tpm.list (List of 25x25 TPMs of Starting Lineup)
#Functions: tpm.convert

sim.ngame <- function(n,tpm.list){
  
  #Batter Index; 10 Cycles Maximum through Lineup (90 batters)
  v <- rep(1:9,10)
  
  #Convert Lineup Matrices
  tpmc.list <- list()
  for(i in 1:9){tpmc.list[[i]] <- tpm.convert(tpm.list[[i]])}
  
  #n Games Simulated
  sim.n <- vector()
  for(i in 1:n){
    
    #Simulate 1st Batter
    sim.temp <- matrix(0,1,5); colnames(sim.temp) <- c("Lineup","Inning","Next_Row","Run_Row","Runs")
    init.row <- which(rmultinom(1,1,tpmc.list[[1]][1,])==1)
    #If No Leadoff HR
    if(init.row!=9){sim.temp[1,] <- c(1,1,init.row,init.row,0)
    #If Leadoff HR  
    } else {sim.temp[1,] <- c(1,1,1,9,0)}
    
    #Simulate Remaining Batters until 9 Full Innings Played
    while(sim.temp[dim(sim.temp)[1],2]!=9 | sim.temp[dim(sim.temp)[1],3]!=85){
      j <- dim(sim.temp)[1] + 1
      #If 9th Batter Not 3rd Out
      if(sim.temp[j-1,3]!=85){
        nrow <- which(rmultinom(1,1,tpmc.list[[v[j]]][sim.temp[j-1,3],])==1)
        sim.temp <- rbind(sim.temp,c(v[j],sim.temp[j-1,2],
                                     #If No Runs Scored
                                     if(sum(as.numeric(tpmc.list[[v[j]]][nrow,]==1))==0){
                                       c(nrow,nrow)
                                       #If Runs Scored  
                                     } else {c(which(rmultinom(1,1,tpmc.list[[v[j]]][nrow,])==1),nrow)},0))
        #If 9th Batter 3rd Out  
      } else {
        nrow <- which(rmultinom(1,1,tpmc.list[[v[j]]][1,])==1)
        sim.temp <- rbind(sim.temp,c(v[j],sim.temp[j-1,2]+1,
                                     #If No Runs Scored
                                     if(sum(as.numeric(tpmc.list[[v[j]]][nrow,]==1))==0){
                                       c(nrow,nrow)
                                       #If Runs Scored  
                                     } else {c(which(rmultinom(1,1,tpmc.list[[v[j]]][nrow,])==1),nrow)},0))
      }  
    }
    
    #Run Tally
    run.table <- c(rep(c(rep(0,8),rep(1,8),rep(2,7),rep(3,4),4),3),0)
    sim.temp[,5] <- run.table[sim.temp[,4]]
    
    #Total Runs per Game
    sim.n[i] <- sum(sim.temp[,5])
  }
  
  #Simulation Output
  return(sim.n)
}
