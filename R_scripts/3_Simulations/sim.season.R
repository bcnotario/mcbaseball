#Season Run Simulation
#Inputs: yyyy (Year)
#Data Files:  all"yyyy".csv, fields.csv, GL"yyyy".txt, game_log_header.csv
#Packages: expm, e1071, lme4, faraway
#Functions: retro.full, tpm.mlb, act.season, sim.runopt, tpm.player, OBP.glmm, sim.ngame, tpm.lineup.lead, tpm.lineup.runs, tpm.convert

sim.season <- function(yyyy){
  
  #Season Event File
  eventYY <- retro.full(yyyy)
  
  #GLMM
  glmm.temp <- glmer(BASE_ADV ~ OUTS_CT + BASE_STATE + (1|PIT_ID),family=binomial,
                     data=eventYY$event[eventYY$event$BAT_EVENT_FL==TRUE,])
  
  #Game Log Information & Actual Results
  gamestat.temp <- eventYY$gamelog
  gameact.temp <- act.season(yyyy)
  
  #AL & NL Optimal Lineups
  al.runopt <- sim.runopt(tpm.mlb(yyyy,"AL"))$line.opt
  nl.runopt <- sim.runopt(tpm.mlb(yyyy,"NL"))$line.opt
  
  #Simulation Matrix
  sim.mat <- matrix(0,nrow(gamestat.temp),11)
  colnames(sim.mat) <- c("ER.v","Sim.v","SimMax.v","Pred.v","PredMax.v",
                           "ER.h","Sim.h","SimMax.h","Pred.h","PredMax.h","ALNL")
  for(i in 1:nrow(gamestat.temp)){
    
    #TPMs of Visiting Starting Lineup
    TPM.v <- list(tpm.player(as.character(gamestat.temp[i,][[1]]),as.character(gamestat.temp[i,][[27]]),1,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[2]]),as.character(gamestat.temp[i,][[27]]),2,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[3]]),as.character(gamestat.temp[i,][[27]]),3,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[4]]),as.character(gamestat.temp[i,][[27]]),4,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[5]]),as.character(gamestat.temp[i,][[27]]),5,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[6]]),as.character(gamestat.temp[i,][[27]]),6,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[7]]),as.character(gamestat.temp[i,][[27]]),7,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[8]]),as.character(gamestat.temp[i,][[27]]),8,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[9]]),as.character(gamestat.temp[i,][[27]]),9,eventYY,glmm.temp))
    
    #TPMs of Home Starting Lineup
    TPM.h <- list(tpm.player(as.character(gamestat.temp[i,][[13]]),as.character(gamestat.temp[i,][[26]]),1,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[14]]),as.character(gamestat.temp[i,][[26]]),2,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[15]]),as.character(gamestat.temp[i,][[26]]),3,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[16]]),as.character(gamestat.temp[i,][[26]]),4,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[17]]),as.character(gamestat.temp[i,][[26]]),5,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[18]]),as.character(gamestat.temp[i,][[26]]),6,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[19]]),as.character(gamestat.temp[i,][[26]]),7,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[20]]),as.character(gamestat.temp[i,][[26]]),8,eventYY,glmm.temp),
                  tpm.player(as.character(gamestat.temp[i,][[21]]),as.character(gamestat.temp[i,][[26]]),9,eventYY,glmm.temp))
    
    #League Optimal Lineup
    line.opt <- if(gamestat.temp[i,]$HomeTeamLeague=="AL") {al.runopt} else {nl.runopt}
    
    #Visiting Team - Actual Runs, Simulated/Max Runs, Expected Simulated/Max Runs
    sim.mat[i,1] <- gameact.temp[i,6]
    sim.mat[i,2] <- sim.ngame(1,TPM.v)
    sim.mat[i,3] <- sim.ngame(1,TPM.v[line.opt])
    sim.mat[i,4] <- 9*tpm.lineup.lead(TPM.v)%*%tpm.lineup.runs(TPM.v)
    sim.mat[i,5] <- 9*tpm.lineup.lead(TPM.v[line.opt])%*%tpm.lineup.runs(TPM.v[line.opt])   
    
    #Home Team - Actual Runs, Simulated/Max Runs, Expected Simulated/Max Runs
    sim.mat[i,6] <- gameact.temp[i,12]
    sim.mat[i,7] <- sim.ngame(1,TPM.h)
    sim.mat[i,8] <- sim.ngame(1,TPM.h[line.opt])
    sim.mat[i,9] <- 9*tpm.lineup.lead(TPM.h)%*%tpm.lineup.runs(TPM.h)
    sim.mat[i,10] <- 9*tpm.lineup.lead(TPM.h[line.opt])%*%tpm.lineup.runs(TPM.h[line.opt])
  }
  
  #Season Simulated, Maximized, Expected Runs Output, GLMM Summary, AL/NL Optimal Lineup
  return(list(sim.match=sim.mat,glmm=summary(glmm.temp),AL.lineopt=al.runopt,NL.lineopt=nl.runopt))
}
