#Remaining Innning Runs & Game Probability per Batter per Game (5 min per game)

rem.pred <- function(yyyy,team.h,game.n,glmm){
  
  path <- "~/Documents/Retrosheets/download.folder/unzipped/"
  
  #All Events & Gamelog Data
  retroYY <- retro.full(yyyy)
  gameID <- unique(retroYY$event[retroYY$event$HOME_TEAM_ID==team.h,]$GAME_ID)[game.n]
  eventYY <- retroYY$event[retroYY$event$GAME_ID==gameID&retroYY$event$BAT_EVENT_FL==T,]
  gameYY <- retroYY$gamelog[retroYY$gamelog$GAME_ID==gameID,]
  bat.n <- 1:nrow(eventYY); rownames(eventYY) <- bat.n
  
  #Library
  library(Lahman)
  master <- Master[!is.na(Master$retroID),]
  
  #GLMM
  library(lme4)
  glmm <- glmer(BASE_ADV ~ OUTS_CT + BASE_STATE + (1|PIT_ID),family=binomial,data=eventYY[eventYY$BAT_EVENT_FL==TRUE,])
  
  #Visiting Starting Lineup & Pitcher
  line.1v <- c(as.character(gameYY[[1]]),paste0("1. ",toupper(substring(gameYY[[1]],5,5)),' ',master[master$retroID==gameYY[[1]],'nameLast']))
  line.2v <- c(as.character(gameYY[[2]]),paste0("2. ",toupper(substring(gameYY[[2]],5,5)),' ',master[master$retroID==gameYY[[2]],'nameLast']))
  line.3v <- c(as.character(gameYY[[3]]),paste0("3. ",toupper(substring(gameYY[[3]],5,5)),' ',master[master$retroID==gameYY[[3]],'nameLast']))
  line.4v <- c(as.character(gameYY[[4]]),paste0("4. ",toupper(substring(gameYY[[4]],5,5)),' ',master[master$retroID==gameYY[[4]],'nameLast']))
  line.5v <- c(as.character(gameYY[[5]]),paste0("5. ",toupper(substring(gameYY[[5]],5,5)),' ',master[master$retroID==gameYY[[5]],'nameLast']))
  line.6v <- c(as.character(gameYY[[6]]),paste0("6. ",toupper(substring(gameYY[[6]],5,5)),' ',master[master$retroID==gameYY[[6]],'nameLast']))
  line.7v <- c(as.character(gameYY[[7]]),paste0("7. ",toupper(substring(gameYY[[7]],5,5)),' ',master[master$retroID==gameYY[[7]],'nameLast']))
  line.8v <- c(as.character(gameYY[[8]]),paste0("8. ",toupper(substring(gameYY[[8]],5,5)),' ',master[master$retroID==gameYY[[8]],'nameLast']))
  line.9v <- c(as.character(gameYY[[9]]),paste0("9. ",toupper(substring(gameYY[[9]],5,5)),' ',master[master$retroID==gameYY[[9]],'nameLast']))
  spit.v <- c(as.character(gameYY[[26]]),paste0("SP. ",toupper(substring(gameYY[[26]],5,5)),' ',master[master$retroID==gameYY[[26]],'nameLast']))
  line.v <- c(line.1v[2],line.2v[2],line.3v[2],line.4v[2],line.5v[2],line.6v[2],line.7v[2],line.8v[2],line.9v[2])
  
  #Home Starting Lineup & Pitcher
  line.1h <- c(as.character(gameYY[[13]]),paste0("1. ",toupper(substring(gameYY[[13]],5,5)),' ',master[master$retroID==gameYY[[13]],'nameLast']))
  line.2h <- c(as.character(gameYY[[14]]),paste0("2. ",toupper(substring(gameYY[[14]],5,5)),' ',master[master$retroID==gameYY[[14]],'nameLast']))
  line.3h <- c(as.character(gameYY[[15]]),paste0("3. ",toupper(substring(gameYY[[15]],5,5)),' ',master[master$retroID==gameYY[[15]],'nameLast']))
  line.4h <- c(as.character(gameYY[[16]]),paste0("4. ",toupper(substring(gameYY[[16]],5,5)),' ',master[master$retroID==gameYY[[16]],'nameLast']))
  line.5h <- c(as.character(gameYY[[17]]),paste0("5. ",toupper(substring(gameYY[[17]],5,5)),' ',master[master$retroID==gameYY[[17]],'nameLast']))
  line.6h <- c(as.character(gameYY[[18]]),paste0("6. ",toupper(substring(gameYY[[18]],5,5)),' ',master[master$retroID==gameYY[[18]],'nameLast']))
  line.7h <- c(as.character(gameYY[[19]]),paste0("7. ",toupper(substring(gameYY[[19]],5,5)),' ',master[master$retroID==gameYY[[19]],'nameLast']))
  line.8h <- c(as.character(gameYY[[20]]),paste0("8. ",toupper(substring(gameYY[[20]],5,5)),' ',master[master$retroID==gameYY[[20]],'nameLast']))
  line.9h <- c(as.character(gameYY[[21]]),paste0("9. ",toupper(substring(gameYY[[21]],5,5)),' ',master[master$retroID==gameYY[[21]],'nameLast']))
  spit.h <- c(as.character(gameYY[[27]]),paste0("SP. ",toupper(substring(gameYY[[27]],5,5)),' ',master[master$retroID==gameYY[[27]],'nameLast']))
  line.h <- c(line.1h[2],line.2h[2],line.3h[2],line.4h[2],line.5h[2],line.6h[2],line.7h[2],line.8h[2],line.9h[2])
  
  #TPMs of Visiting & HOme Starting Lineups
  tpm.v <- list(tpm.player(line.1v[1],spit.h[1],1,retroYY,glmm),tpm.player(line.2v[1],spit.h[1],2,retroYY,glmm),
                tpm.player(line.3v[1],spit.h[1],3,retroYY,glmm),tpm.player(line.4v[1],spit.h[1],4,retroYY,glmm),
                tpm.player(line.5v[1],spit.h[1],5,retroYY,glmm),tpm.player(line.6v[1],spit.h[1],6,retroYY,glmm),
                tpm.player(line.7v[1],spit.h[1],7,retroYY,glmm),tpm.player(line.8v[1],spit.h[1],8,retroYY,glmm),
                tpm.player(line.9v[1],spit.h[1],9,retroYY,glmm))
  tpm.h <- list(tpm.player(line.1h[1],spit.v[1],1,retroYY,glmm),tpm.player(line.2h[1],spit.v[1],2,retroYY,glmm),
                tpm.player(line.3h[1],spit.v[1],3,retroYY,glmm),tpm.player(line.4h[1],spit.v[1],4,retroYY,glmm),
                tpm.player(line.5h[1],spit.v[1],5,retroYY,glmm),tpm.player(line.6h[1],spit.v[1],6,retroYY,glmm),
                tpm.player(line.7h[1],spit.v[1],7,retroYY,glmm),tpm.player(line.8h[1],spit.v[1],8,retroYY,glmm),
                tpm.player(line.9h[1],spit.v[1],9,retroYY,glmm))
  
  #At-Bat Inputs
  label.vh <- c('v','h')
  bat.vh <- label.vh[eventYY$BAT_HOME_ID + 1]
  bat.inn <- eventYY$INN_CT
  bat.vlin <- c(1,rep(0,max(bat.n)-1)); bat.hlin <- bat.vlin; m <- c(1:9,1)
  for(j in 2:max(bat.n)){ifelse(bat.vh[j]=='v',bat.vlin[j] <- eventYY$BAT_LINEUP_ID[j],bat.vlin[j] <- bat.vlin[j-1])}
  for(j in 2:max(bat.n)){ifelse(bat.vh[j]=='h',bat.vlin[j] <- m[bat.vlin[j] + 1], bat.vlin[j])}
  for(j in 2:max(bat.n)){ifelse(bat.vh[j]=='h',bat.hlin[j] <- eventYY$BAT_LINEUP_ID[j],bat.hlin[j] <- bat.hlin[j-1])}
  for(j in grep(1,eventYY$BAT_LINEUP_ID)[2]:max(bat.n)){ifelse(bat.vh[j]=='v',bat.hlin[j] <- m[bat.hlin[j] + 1], bat.hlin[j])}
  bat.vrun <- eventYY$AWAY_SCORE_CT
  bat.hrun <- eventYY$HOME_SCORE_CT
  bat.outs <- as.numeric(eventYY$OUTS_CT)-1
  bat.base <- eventYY$BASE_STATE
  bat.vrbi <- c(eventYY$AWAY_SCORE_CT[-1]-eventYY$AWAY_SCORE_CT[-max(bat.n)],0)
  bat.hrbi <- c(eventYY$HOME_SCORE_CT[-1]-eventYY$HOME_SCORE_CT[-max(bat.n)],eventYY$RBI_CT[max(bat.n)])
  
  #At-Bat Descriptors
  event.code <- c('O','O','O','SB','O','CS','O','O','O','O','Balk','Other','O','BB','IBB','HBP','O','E','FC','1B','2B','3B','HR')
  bat.id <- rep(0,max(bat.n)); for(j in 1:max(bat.n)){
    bat.id[j] <- paste0(toupper(substring(eventYY$BAT_ID[j],5,5)),' ',master[master$retroID==eventYY$BAT_ID[j],'nameLast'])}
  bat.ev <- event.code[eventYY$EVENT_CD]; bat.ev[eventYY$SF_FL==T] <- 'SF'; bat.ev[eventYY$SH_FL==T] <- 'SH'; bat.ev[bat.ev=='O'&eventYY$RBI_CT>0] <- 'FC'
  eventBX <- data.frame(TEAM=bat.vh,INN=bat.inn,LINE.v=bat.vlin,LINE.h=bat.hlin,RUN.v=bat.vrun,RUN.h=bat.hrun,
                       OUTS=bat.outs,BASE=bat.base,BATTER=bat.id,EVENT=bat.ev,P.WIN=0,P.TIE=0,INN.R=0,REM.R=0,
                       RBI.v=bat.vrbi,RBI.h=bat.hrbi)
  
  #Simulations by Batter
  for(k in 1:max(bat.n)){
    eventBX[k,11:14] <- rem.ngame(100,tpm.v,tpm.h,eventBX$TEAM[k],eventBX$INN[k],eventBX$LINE.v[k],eventBX$LINE.h[k],
                                  eventBX$RUN.v[k],eventBX$RUN.h[k],eventBX$OUTS[k],eventBX$BASE[k])$stats
  }
  
  #Plot Tables
  col.tbl <- data.frame(team=c("ANA","BAL","BOS","CHA","CLE","DET","HOU","KCA","MIN","NYA","OAK","SEA","TBA","TEX","TOR",
                               "ARI","ATL","CHN","CIN","COL","LAN","MIA","MIL","NYN","PHI","PIT","SDN","SFN","SLN","WAS"),
                        col=c('red','orange','red','black','red','navy','navy','blue','navy','navy','green','navy','navy','blue','blue',
                              'red','navy','blue','red','purple','blue','orange','navy','blue','red','black','navy','orange','red','red'))
  base.mat <- matrix(c('000',5,5,5,.5,.5,.5,'100',18,5,5,.75,.5,.5,'010',5,18,5,.5,.75,.5,
                       '001',5,5,18,.5,.5,.75,'110',18,18,5,.75,.75,.5,'101',18,5,18,.75,.5,.75,
                       '011',5,18,18,.5,.75,.75,'111',18,18,18,.75,.75,.75),byrow=T,8,7)
  outs.mat <- matrix(c(0,1,2,1,16,16,1,1,16),3,3)
  plotBX <- data.frame(P.WIN = abs(abs(as.numeric(eventBX$TEAM)-1)-eventBX$P.WIN),
                       INN.R = -sign(as.numeric(eventBX$TEAM)-1.5)*eventBX$INN.R,
                       DESC.V = paste0(eventBX$EVENT,'-',eventBX$BATTER),
                       DESC.H = paste0(eventBX$EVENT,'-',eventBX$BATTER),
                       O1.x = .25, O2.x = .75, D.x = .5, B1.x = .75, B2.x = .5, B3.x = .25,
                       O1.y = 0, O2.y = 0, D.y = 0, B1.y = 0, B2.y = 0, B3.y = 0,
                       O1.p = 0, O2.p = 0, D.p = 0, B1.p = 0, B2.p = 0, B3.p = 0,
                       O1.c = .75, O2.c = .75, D.c = 1.25, B1.c = 0, B2.c = 0, B3.c = 0,
                       EVENT = eventBX$EVENT, TEAM = eventBX$TEAM)
  plotBX$DESC.V <- as.character(plotBX$DESC.V); plotBX$DESC.H <- as.character(plotBX$DESC.H)
  
  for(k in 1:max(bat.n)){
    plotBX[k,5:10] <- k + plotBX[k,5:10]
    plotBX[k,11:16] <- if(plotBX$TEAM[k]=='h'){
      c(-.15,-.15,-.075,-.075,-.05,-.075)} else {c(.05,.05,.125,.125,.15,.125)}
    plotBX[k,17:22] <- c(outs.mat[outs.mat[,1]==eventBX$OUTS[k],2:3], 5,
                         as.numeric(base.mat[base.mat[,1]==eventBX$BASE[k],2:4]))
    plotBX[k,26:28] <- as.numeric(base.mat[base.mat[,1]==eventBX$BASE[k],5:7])
    plotBX[k,3] <- ifelse(eventBX$RBI.v[k]==0,plotBX$DESC.V[k],
                          ifelse(eventBX$RBI.v[k]==1,paste0('#',eventBX$RBI.v[k],' Run# ',plotBX$DESC.V[k]),
                                 paste0('#',eventBX$RBI.v[k],' Runs# ',plotBX$DESC.V[k])))
    plotBX[k,4] <- ifelse(eventBX$RBI.h[k]==0,plotBX$DESC.H[k],
                          ifelse(eventBX$RBI.h[k]==1,paste0(plotBX$DESC.H[k],' #',eventBX$RBI.h[k],' Run#'),
                                 paste0(plotBX$DESC.H[k],' #',eventBX$RBI.h[k],' Runs#')))
  }
  
  #Plot
  png(file=paste0('~/Documents/MLB/',team.h,'/',gameID,'.png'),width=max(bat.n)*36,height=1200,res=200)
  plot(c(plotBX$P.WIN,ifelse(gameYY$HomeRunsScore>gameYY$VisitorRunsScored,1,0))*3-1.5,type="l",ylim=c(-1.5,1.5),lwd=3,
       ylab='E(Remaining Inning Runs)',col='gray',xaxt = 'n',xlab='Batting Order #',
       main=paste0(eventYY[1,]$AWAY_TEAM_ID,'@',eventYY[1,]$HOME_TEAM_ID,' ',substring(gameID,8,9),'/',substring(gameID,10,11),'/',yyyy))
  axis(1,at=bat.n,labels=eventYY$BAT_LINEUP_ID,cex.axis=.65); #axis(3,bat.n)
  #Expected Remaining Inning Runs
  segments(bat.n,0,bat.n,(as.numeric(plotBX$TEAM)-1)*plotBX$INN.R,lwd=2,
           col=as.character(col.tbl[col.tbl$team==eventYY[1,]$AWAY_TEAM_ID,'col']))
  segments(bat.n,0,bat.n,abs(as.numeric(plotBX$TEAM)-2)*plotBX$INN.R,lwd=2,
           col=as.character(col.tbl[col.tbl$team==eventYY[1,]$HOME_TEAM_ID,'col']))
  
  #Play Descriptions
  text(which(plotBX$EVENT!='O'&plotBX$TEAM=='h'),0,plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='h',]$DESC.H,
       srt=90,cex=.75,pos=4,col=as.character(col.tbl[col.tbl$team==eventYY[1,]$HOME_TEAM_ID,'col']))
  text(which(plotBX$EVENT!='O'&plotBX$TEAM=='v')+1.25,0,plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='v',]$DESC.V,
       srt=90,cex=.75,pos=2,col=as.character(col.tbl[col.tbl$team==eventYY[1,]$AWAY_TEAM_ID,'col']))
  
  #Situation Descriptions
  points(x=unlist(plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='h',5:10]),
         y=unlist(plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='h',11:16]),
         pch=unlist(plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='h',17:22]),
         cex=unlist(plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='h',23:28]),
         col=as.character(col.tbl[col.tbl$team==eventYY[1,]$HOME_TEAM_ID,'col']))
  points(x=unlist(plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='v',5:10]),
         y=unlist(plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='v',11:16]),
         pch=unlist(plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='v',17:22]),
         cex=unlist(plotBX[plotBX$EVENT!='O'&plotBX$TEAM=='v',23:28]),
         col=as.character(col.tbl[col.tbl$team==eventYY[1,]$AWAY_TEAM_ID,'col']))
  abline(h=0,col='dark gray'); abline(v=which(eventBX$INN[-1]-eventBX$INN[-max(bat.n)]==1)+.5,lty=2,col='dark gray')
  
  #Innings
  text(x=if(max(eventBX$INN)>9){c(which(eventBX$INN[-1]-eventBX$INN[-max(bat.n)]==1)[1:9]+.85,max(bat.n)+3)} else {
    c(which(eventBX$INN[-1]-eventBX$INN[-max(bat.n)]==1)[1:8]+.85,max(bat.n)+3)},y=1.5,pos=2,
    labels=if(max(eventBX$INN)>9){c('1st Inning','2nd Inning','3rd Inning','4th Inning','5th Inning','6th Inning',
                                    '7th Inning','8th Inning','9th Inning','Extra Innings')} else {
                                      c('1st Inning','2nd Inning','3rd Inning','4th Inning','5th Inning','6th Inning',
                                        '7th Inning','8th Inning','9th Inning')},col='black')
  
  #Score Summaries
  text(x=if(max(eventBX$INN)>9){c(which(eventBX$INN[-1]-eventBX$INN[-max(bat.n)]==1)[1:9]+.85,max(bat.n)+3)} else {
    c(which(eventBX$INN[-1]-eventBX$INN[-max(bat.n)]==1)[1:8]+.85,max(bat.n)+3)},y=1.35,pos=2,cex=.6,col='black',
    label=if(max(eventBX$INN)>9){
      c(paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==2,]$RUN.v),' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==2,]$RUN.h)),
        paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==3,]$RUN.v),' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==3,]$RUN.h)),
        paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==4,]$RUN.v),' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==4,]$RUN.h)),
        paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==5,]$RUN.v),' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==5,]$RUN.h)),
        paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==6,]$RUN.v),' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==6,]$RUN.h)),
        paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==7,]$RUN.v),' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==7,]$RUN.h)),
        paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==8,]$RUN.v),' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==8,]$RUN.h)),
        paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==9,]$RUN.v),' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==9,]$RUN.h)),
        paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==10,]$RUN.v),' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==10,]$RUN.h)),
        paste0(eventYY[1,]$AWAY_TEAM_ID,' ',gameYY$VisitorRunsScore,' @ ',
               eventYY[1,]$HOME_TEAM_ID,' ',gameYY$HomeRunsScore))} else {
                 c(paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==2,]$RUN.v),' @ ',
                          eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==2,]$RUN.h)),
                   paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==3,]$RUN.v),' @ ',
                          eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==3,]$RUN.h)),
                   paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==4,]$RUN.v),' @ ',
                          eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==4,]$RUN.h)),
                   paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==5,]$RUN.v),' @ ',
                          eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==5,]$RUN.h)),
                   paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==6,]$RUN.v),' @ ',
                          eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==6,]$RUN.h)),
                   paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==7,]$RUN.v),' @ ',
                          eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==7,]$RUN.h)),
                   paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==8,]$RUN.v),' @ ',
                          eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==8,]$RUN.h)),
                   paste0(eventYY[1,]$AWAY_TEAM_ID,' ',min(eventBX[eventBX$INN==9,]$RUN.v),' @ ',
                          eventYY[1,]$HOME_TEAM_ID,' ',min(eventBX[eventBX$INN==9,]$RUN.h)),
                   paste0(eventYY[1,]$AWAY_TEAM_ID,' ',gameYY$VisitorRunsScore,' @ ',
                          eventYY[1,]$HOME_TEAM_ID,' ',gameYY$HomeRunsScore))})
  dev.off()
  
  #Output
  return(list(eventBX=eventBX,plotBX=plotBX))
}
