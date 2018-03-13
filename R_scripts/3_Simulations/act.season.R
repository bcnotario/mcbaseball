#Full Season Box Scores (w/in 9 Innings)
#Inputs: yyyy (Year)
#Data Files:  all"yyyy".csv, fields.csv, GL"yyyy".txt, game_log_header.csv

act.season <- function(yyyy){
  
  #Gamelog & Event Data
  path <- "/Users/Chilipino/Documents/Retrosheets/download.folder/unzipped/"
  gamelog <- read.csv(paste(path,"GL",yyyy,".txt",sep=""),header=FALSE)
  glfields <- read.csv(paste(path,"game_log_header.csv",sep=""))
  names(gamelog) <- names(glfields)
  event <- read.csv(paste(path,"all",yyyy,".csv",sep=""),header=FALSE)
  fields <- read.csv(paste(path,"fields.csv",sep=""))
  names(event) <- fields[,"Header"]
  
  #Season Game Results
  season.mat <- matrix(0,nrow(gamelog),15); 
  for(i in 1:nrow(gamelog)){
    
    #GameID & Starting Pitchers & Teams
    season.mat[i,1] <- paste(gamelog[i,]$HomeTeam,gamelog[i,]$Date,gamelog[i,]$DoubleHeader,sep="")
    season.mat[i,2] <- as.character(gamelog[i,]$VisitorStartingPitcherID)
    season.mat[i,8] <- as.character(gamelog[i,]$HomeStartingPitcherID)
    season.mat[i,14] <- as.character(gamelog[i,]$VisitingTeam)
    season.mat[i,15] <- as.character(gamelog[i,]$HomeTeam)
    
    #Events by Game & Starting Pitcher
    event.v <- event[event$GAME_ID==season.mat[i,1]&event$BAT_HOME_ID==0&event$INN_CT<10,]
    event.h <- event[event$GAME_ID==season.mat[i,1]&event$BAT_HOME_ID==1&event$INN_CT<10,]
    #Hits
    season.mat[i,3] <- sum(event.v$H_FL>0)*9/max(event.v$INN_CT)
    season.mat[i,9] <- sum(event.h$H_FL>0)*9/max(event.h$INN_CT)
    #Walks
    season.mat[i,4] <- sum(event.v$EVENT_CD==14|event.v$EVENT_CD==15)*9/max(event.v$INN_CT)
    season.mat[i,10] <- sum(event.h$EVENT_CD==14|event.h$EVENT_CD==15)*9/max(event.h$INN_CT)
    #RBIs
    season.mat[i,5] <- sum(event.v$RBI_CT)*9/max(event.v$INN_CT)
    season.mat[i,11] <- sum(event.h$RBI_CT)*9/max(event.h$INN_CT)
    #Innings
    season.mat[i,7] <- max(event.v$INN_CT)
    season.mat[i,13] <- max(event.h$INN_CT)
    #Earned Runs
    season.mat[i,6] <- (sum(event.v$BAT_DEST_ID==4)+sum(event.v$RUN1_DEST_ID==4)+sum(event.v$RUN2_DEST_ID==4)+
                        sum(event.v$RUN3_DEST_ID==4))*9/max(event.v$INN_CT)
    season.mat[i,12] <- (sum(event.h$BAT_DEST_ID==4)+sum(event.h$RUN1_DEST_ID==4)+sum(event.h$RUN2_DEST_ID==4)+
                         sum(event.h$RUN3_DEST_ID==4))*9/max(event.h$INN_CT)
  }
  season.mat <- data.frame(season.mat[,1:2],as.numeric(season.mat[,3]),as.numeric(season.mat[,4]),
                           as.numeric(season.mat[,5]),as.numeric(season.mat[,6]),as.numeric(season.mat[,7]),
                           season.mat[,8],as.numeric(season.mat[,9]),as.numeric(season.mat[,10]),
                           as.numeric(season.mat[,11]),as.numeric(season.mat[,12]),as.numeric(season.mat[,13]),
                           season.mat[,14:15])
  colnames(season.mat) <- c("GameID","Pitcher.v","H.v","BB.v","RBI.v","ER.v","IP.v",
                            "Pitcher.h","H.h","BB.h","RBI.h","ER.h","IP.h","Team.v","Team.h")
  
  #Output - Season Actual Box Score by Game
  return(season.mat)
}
