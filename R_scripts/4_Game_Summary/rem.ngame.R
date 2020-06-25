#Fixed Lineup Game Remainder Simulation

rem.ngame <- function(n,tpm.v,tpm.h,rem.vh,currinn,currbat.v,currbat.h,currscore.v,currscore.h,currout,currbases){
  
  #INPUT - n Simulations, TPMs of Starting Lineup Visiting & Home Teams, Current Inning, Batter, Score, Inning Outs, Base Runners
  ngame <- matrix(0,n,6); colnames(ngame) <- c("Win","Tie","Runs.h","Runs.v","Inn.Runs","Rem.Runs")
  ngame.list <- list()
  for(g in 1:n){
    
    #Visiting Team
    if(rem.vh=='v'){
      
      #1-9 Innings
      if(currinn<=9){
        
        #Remaining Inning Simulation
        sim.v <- rem.9inn(tpm.v,currinn,currbat.v,currscore.v,currout,currbases); bats.v <- nrow(sim.v)
        sim.h <- rem.9inn(tpm.h,currinn,currbat.h,currscore.h,0,'000'); bats.h <- nrow(sim.h)
      }
      
      #Extra Innings
      if(currinn>9){
        
        #Remaining Inning Simulation
        sim.v <- rem.xinn(tpm.v,currinn,currbat.v,currscore.v,currout,currbases); bats.v <- nrow(sim.v)
        sim.h <- rem.xinn(tpm.h,currinn,currbat.h,currscore.h,0,'000'); bats.h <- nrow(sim.h)
        }
      }
    
    #Home Team
    if(rem.vh=='h'){
      
      #1-8 Innings
      if(currinn<9){
        #Remaining Inning Simulation
        sim.v <- rem.9inn(tpm.v,currinn+1,currbat.v,currscore.v,0,'000'); bats.v <- nrow(sim.v)
        sim.h <- rem.9inn(tpm.h,currinn,currbat.h,currscore.h,currout,currbases); bats.h <- nrow(sim.h)
      }
      if(currinn==9){
        
        #Remaining Inning Simulation
        sim.v <- data.frame(currbat.v,9,85,85,currscore.v,2,3,'000','000',currscore.v,'O*'); bats.v <- nrow(sim.v)
        sim.h <- rem.9inn(tpm.h,currinn,currbat.h,currscore.h,currout,currbases); bats.h <- nrow(sim.h)
        colnames(sim.v) <- colnames(sim.h)
      }
      
      #Extra Innings
      if(currinn>9){
        sim.v <- data.frame(currbat.v,currinn,85,85,currscore.v,2,3,'000','000',currscore.v,'O*'); bats.v <- nrow(sim.v)
        sim.h <- rem.xinn(tpm.h,currinn,currbat.h,currscore.h,currout,currbases); bats.h <- nrow(sim.h)
        colnames(sim.v) <- colnames(sim.h)
      }
    }
    
    #Final Play-by-Play
    final.v <- sim.v
    final.h <- sim.h
    
    #Record Game
    ngame[g,] <- c(as.numeric(max(get(paste0('final.',rem.vh))$Score)>
                                max(get(paste0('final.',c('v','h')[which(rem.vh!=c('v','h'))]))$Score)),
                   as.numeric(max(final.h$Score)==max(final.v$Score)),
                   max(final.h$Score),max(final.v$Score),
                   sum(get(paste0('final.',rem.vh))[get(paste0('final.',rem.vh))$Inning==currinn,]$Runs),
                   sum(get(paste0('final.',rem.vh))$Runs))
    ngame.list[[g]] <- list(final.h,final.v)
  }
  
  #Output
  return(list(stats=list(p.win=mean(ngame[,1]),p.tie=mean(ngame[,2]),inn.r=mean(ngame[,5]),rem.r=mean(ngame[,6])),
              ngame=ngame,ngame.list=ngame.list))
}
