#Fixed Lineup Simulation for Extra Inning Remainder

rem.xinn <- function(tpm.list,currinn,currbat,currscore,currout,currbases){
  
  #INPUT - TPMs of Starting Lineup, Current Inning, Batter, Score, Inning Outs, Base Runners
  #Batter Index; 10 Cycles Maximum through Lineup (90 batters)
  v <- rep(1:9,10)[currbat:90]
  brs <- c("000","100","010","001","110","101","011","111")
  
  #Convert Lineup Matrices
  tpmc.list <- list()
  for(i in 1:9){tpmc.list[[i]] <- tpm.convert(tpm.list[[i]])}
  
  #Simulate 1st Batter
  sim.temp <- matrix(0,1,4); colnames(sim.temp) <- c("Lineup","Inning","Next_Row","Run_Row")
  init.row <- which(rmultinom(1,1,tpmc.list[[currbat]][currout*28+which(brs==currbases),])==1)
  
  #If No Runs Scored
  if(sum(as.numeric(tpmc.list[[currbat]][init.row,]==1))==0){sim.temp[1,] <- c(currbat,currinn,init.row,init.row)
  
  #If Runs Scored  
  } else {sim.temp[1,] <- c(currbat,currinn,which(rmultinom(1,1,tpmc.list[[currbat]][init.row,])==1),init.row)}
  
  #Simulate Remaining Batters until Full Inning Played
  while(sim.temp[dim(sim.temp)[1],3]!=85){
    j <- dim(sim.temp)[1] + 1
    
    #If 9th Batter Not 3rd Out
    if(sim.temp[j-1,3]!=85){
      nrow <- which(rmultinom(1,1,tpmc.list[[v[j]]][sim.temp[j-1,3],])==1)
      sim.temp <- rbind(sim.temp,c(v[j],sim.temp[j-1,2],
                                   
                                   #If No Runs Scored
                                   if(sum(as.numeric(tpmc.list[[v[j]]][nrow,]==1))==0){
                                     c(nrow,nrow)
                                     
                                     #If Runs Scored  
                                   } else {c(which(rmultinom(1,1,tpmc.list[[v[j]]][nrow,])==1),nrow)}))
      
      #If 9th Batter 3rd Out  
    } else {
      nrow <- which(rmultinom(1,1,tpmc.list[[v[j]]][1,])==1)
      sim.temp <- rbind(sim.temp,c(v[j],sim.temp[j-1,2]+1,
                                   
                                   #If No Runs Scored
                                   if(sum(as.numeric(tpmc.list[[v[j]]][nrow,]==1))==0){
                                     c(nrow,nrow)
                                     
                                     #If Runs Scored  
                                   } else {c(which(rmultinom(1,1,tpmc.list[[v[j]]][nrow,])==1),nrow)}))
    }  
  }
  sim.temp <- data.frame(sim.temp)
  
  #Run Tally
  run.table <- c(rep(c(rep(0,8),rep(1,8),rep(2,7),rep(3,4),4),3),0)
  sim.temp$Runs <- run.table[sim.temp[,4]]
  
  #Out Tally
  iout.table <- c(rep(0,28),rep(1,28),rep(2,28),0)
  rout.table <- c(rep(0,28),rep(1,28),rep(2,28),3)
  sim.temp$Init_Outs <- c(currout,iout.table[sim.temp[,4]][-nrow(sim.temp)])
  sim.temp$Res_Outs <- rout.table[sim.temp[,4]]
  
  #Base Runner Tally
  brs <- c("000","100","010","001","110","101","011","111")
  brs.table <- c(rep(c(brs,brs,brs[1:7],brs[1:4],brs[1]),3),"000")
  sim.temp$Init_BRs <- c(currbases,brs.table[sim.temp[,4]][-nrow(sim.temp)])
  sim.temp$Res_BRs <- brs.table[sim.temp[,4]]
  
  #Score Tally
  sim.temp$Score[1] <- sim.temp$Runs[1] + currscore
  if(nrow(sim.temp)>1){for(t in 2:nrow(sim.temp)){sim.temp$Score[t] <- sim.temp$Runs[t] + sim.temp$Score[t-1]}}
  
  #Outcome Tally
  label.mat <- tpm.label(0)
  sim.temp$Outcome[1] <- label.mat[currout*28+which(brs==currbases),sim.temp$Run_Row[1]]; 
  if(nrow(sim.temp)>1){
    for(t in 2:nrow(sim.temp)){
      ifelse(sim.temp$Inning[t]!=sim.temp$Inning[t-1],
             sim.temp$Outcome[t] <- label.mat[1,sim.temp$Run_Row[t]],
             sim.temp$Outcome[t] <- label.mat[sim.temp$Run_Row[t-1],sim.temp$Run_Row[t]])
      if(sim.temp$Runs[t-1]>0){sim.temp$Outcome[t] <- label.mat[sim.temp$Next_Row[t-1],sim.temp$Run_Row[t]]}
    }
  }
  
  #Output
  return(sim.temp)
}
