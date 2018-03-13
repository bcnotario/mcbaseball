#Season NL/AL Lineup Statistics TPM (pre-conversion)
#Inputs: yyyy (Year), ALNL ("AL" American League or "NL" National League)
#Data Files:  all"yyyy".csv, fields.csv

tpm.mlb <- function(yyyy,ALNL){
  
  #Location of Retrosheet files
  path <- "/Users/Chilipino/Documents/Retrosheets/download.folder/unzipped/"
  
  #All Events File
  event <- read.csv(paste(path,"all",yyyy,".csv",sep=""),header=FALSE)
  fields <- read.csv(paste(path,"fields.csv",sep=""))
  names(event) <- fields[,"Header"]
  event$SCORE_DIF <- (event$HOME_SCORE_CT-event$AWAY_SCORE_CT)*sign(event$BAT_HOME_ID-.5)
  event$BASE_STATE <- paste(as.numeric(event$BASE1_RUN_ID!=""),as.numeric(event$BASE2_RUN_ID!=""),
              			    as.numeric(event$BASE3_RUN_ID!=""),sep="")
  event$BASE_ADV <- as.numeric(event$EVENT_CD==14|event$EVENT_CD==15|event$EVENT_CD==16|event$EVENT_CD==20|
                               event$EVENT_CD==21|event$EVENT_CD==22|event$EVENT_CD==23)
  event$HOME_TEAM_ID <- substr(event[,"GAME_ID"],1,3)
  
  #League Designation
  team <- c("ANA","BAL","BOS","CHA","CLE","DET","HOU","KCA","MIN","NYA","OAK","SEA","TBA","TEX","TOR",
            "ARI","ATL","CHN","CIN","COL","LAN","MIA","MIL","NYN","PHI","PIT","SDN","SFN","SLN","WAS")
  league <- c(rep("AL",15),rep("NL",15))
  team.df <- data.frame(HOME_TEAM_ID=as.character(team),LEAGUE=league)
  event <- merge(event,team.df,by="HOME_TEAM_ID")
  event <- event[event$LEAGUE==ALNL,]
  
  #Stolen Base Events
  x1play <- data.frame(GAME_ID=event[event$BASE1_RUN_ID!="",]$GAME_ID,BASE1_RUN_ID=event[event$BASE1_RUN_ID!="",]$BASE1_RUN_ID,
                       REM_FOR_PR=event[event$BASE1_RUN_ID!="",]$REMOVED_FOR_PR_RUN1_ID,
                       LINEUP_ID_BAT=event[event$BASE1_RUN_ID!="",]$BAT_LINEUP_ID,
                       SB=event[event$BASE1_RUN_ID!="",]$RUN1_SB_FL,CS=event[event$BASE1_RUN_ID!="",]$RUN1_CS_FL,
                       RUN1_ID=vector(mode="character",length=nrow(event[event$BASE1_RUN_ID!="",])),
                       LINEUP_ID_RUN1=vector(mode="integer",length=nrow(event[event$BASE1_RUN_ID!="",])),
                       stringsAsFactors=FALSE)
  x1play.rep <- x1play[x1play$REM_FOR_PR!="",1:3]
  splay <- x1play[x1play$SB==T|x1play$CS==T,]
  for(i in 1:nrow(splay)){
    splay$RUN1_ID[i] <- ifelse(
      nrow(x1play.rep[x1play.rep$GAME_ID==splay$GAME_ID[i]&x1play.rep$BASE1_RUN_ID==splay$BASE1_RUN_ID[i],])==0,
      as.character(splay$BASE1_RUN_ID[i]),
      as.character(x1play.rep[x1play.rep$GAME_ID==splay$GAME_ID[i]&x1play.rep$BASE1_RUN_ID==splay$BASE1_RUN_ID[i],3]))
    splay$LINEUP_ID_RUN1[i] <- min(event[event$GAME_ID==splay$GAME_ID[i]&event$BAT_ID==splay$RUN1_ID[i],]$BAT_LINEUP_ID)
  }
  
  #Triple Play
  TP <- nrow(event[event$TP_FL==T,])/
  		nrow(event[event$OUTS_CT==0&event$BAT_EVENT_FL==T&(event$BASE_STATE=="110"|event$BASE_STATE=="111"),])
  
  #Create Event Subsets
  event.list <- list(); steal.list <- list()
  for(i in 1:9){
    event.list[[i]] <- event[event$BAT_LINEUP_ID==i&event$BAT_EVENT_FL==T,]
    steal.list[[i]] <- splay[splay$LINEUP_ID_RUN1==i,]}
  event.list[[10]] <- event[event$BAT_EVENT_FL==T,]; steal.list[[10]] <- splay
  
  #Create TPM for All Lineup Positions
  for(i in 1:10){
    rplay <- event.list[[i]]; spl <- steal.list[[i]]
    n <- nrow(rplay)
    
    #Batting Statistics
    BB <- nrow(rplay[rplay$EVENT_CD==14|rplay$EVENT_CD==15|rplay$EVENT_CD==16,])/n
    X1B <- nrow(rplay[rplay$H_FL==1,])/n
    X2B <- nrow(rplay[rplay$H_FL==2,])/n
    X3B <- nrow(rplay[rplay$H_FL==3,])/n
    HR <- nrow(rplay[rplay$H_FL==4,])/n
    SBC <- 1 - nrow(spl)/nrow(rplay[rplay$EVENT_CD==14|rplay$EVENT_CD==15|rplay$EVENT_CD==16|rplay$H_FL==1,])
    SB <- nrow(spl[spl$SB==T,])/n
    SF <- nrow(rplay[rplay$SF_FL==T&rplay$BASE3_RUN_ID!=""&rplay$RUN3_DEST_ID==4&rplay$OUTS_CT!=2&rplay$DP_FL==F&
                     rplay$ERR_CT==0,])/nrow(rplay[rplay$OUTS_CT!=2&rplay$BASE3_RUN_ID!="",])
    GDP <- nrow(rplay[grepl("GDP",rplay$EVENT_TX),])/nrow(rplay[(rplay$OUTS_CT==0|rplay$OUTS_CT==1)&
               (rplay$BASE_STATE=="100"|rplay$BASE_STATE=="110"|rplay$BASE_STATE=="101"|rplay$BASE_STATE=="111"),])
    LDDP <- (nrow(rplay[rplay$DP_FL==T,])-nrow(rplay[grepl("GDP",rplay$EVENT_TX),]))/
             nrow(rplay[(rplay$OUTS_CT==0|rplay$OUTS_CT==1)&
                 (rplay$BASE_STATE=="100"|rplay$BASE_STATE=="110"|rplay$BASE_STATE=="101"|rplay$BASE_STATE=="111"),])
    FC1 <- nrow(rplay[rplay$BAT_DEST_ID==0&rplay$RUN1_DEST_ID==2&rplay$H_FL==0&rplay$ERR_CT==0&rplay$EVENT_OUTS_CT==1&
                      rplay$OUTS_CT!=2&(rplay$BASE_STATE=="100"|rplay$BASE_STATE=="101"),])/
           nrow(rplay[rplay$OUTS_CT!=2&(rplay$BASE_STATE=="100"|rplay$BASE_STATE=="101"),])
    FC2 <- nrow(rplay[rplay$RUN2_DEST_ID==3&rplay$H_FL==0&rplay$ERR_CT==0&rplay$EVENT_OUTS_CT==1&rplay$OUTS_CT!=2&
	           (rplay$BASE_STATE=="010"|rplay$BASE_STATE=="110"),])/
      	   nrow(rplay[rplay$OUTS_CT!=2&(rplay$BASE_STATE=="010"|rplay$BASE_STATE=="110"),])
    FC3 <- nrow(rplay[rplay$SF_FL==F&rplay$BASE3_RUN_ID!=""&rplay$RUN3_DEST_ID==4&rplay$OUTS_CT!=2&rplay$ERR_CT==0&
					  rplay$EVENT_OUTS_CT==1,])/nrow(rplay[rplay$OUTS_CT!=2&rplay$BASE3_RUN_ID!="",])
    
    #Season MLB Base Running Splits
    #Single - Runner on 1st Advances to 2nd or 3rd
    p <- nrow(rplay[rplay$H_FL==1&rplay$EVENT_OUTS_CT==0&rplay$ERR_CT==0&rplay$BAT_DEST_ID==1&rplay$RUN1_DEST_ID==2,])
    q <- nrow(rplay[rplay$H_FL==1&rplay$EVENT_OUTS_CT==0&rplay$ERR_CT==0&rplay$BAT_DEST_ID==1&rplay$RUN1_DEST_ID==3,])
    p1 <- p/(p+q)
    #Single - Runner on 2nd Advance to 3rd or Home
    p <- nrow(rplay[rplay$H_FL==1&rplay$EVENT_OUTS_CT==0&rplay$ERR_CT==0&rplay$BAT_DEST_ID==1&rplay$RUN2_DEST_ID==3,])
    q <- nrow(rplay[rplay$H_FL==1&rplay$EVENT_OUTS_CT==0&rplay$ERR_CT==0&rplay$BAT_DEST_ID==1&rplay$RUN2_DEST_ID==4,])
    p2 <- p/(p+q)
    #Double - Runner on 1st Advances to 3rd or Home
    p <- nrow(rplay[rplay$H_FL==2&rplay$EVENT_OUTS_CT==0&rplay$ERR_CT==0&rplay$BAT_DEST_ID==2&rplay$RUN1_DEST_ID==3,])
    q <- nrow(rplay[rplay$H_FL==2&rplay$EVENT_OUTS_CT==0&rplay$ERR_CT==0&rplay$BAT_DEST_ID==2&rplay$RUN1_DEST_ID==4,])
    p3 <- p/(p+q)
    #Sacrifice Fly - Runner on 2nd Advances or Stays
    p <- nrow(rplay[rplay$SF_FL==T&rplay$EVENT_OUTS_CT==1&rplay$RUN3_DEST_ID==4&rplay$RUN2_DEST_ID==2,])
    q <- nrow(rplay[rplay$SF_FL==T&rplay$EVENT_OUTS_CT==1&rplay$RUN3_DEST_ID==4&rplay$RUN2_DEST_ID==3,])
    p4 <- p/(p+q)
    #Ground Double Play Split - Runner Out at 1st & 2nd or 1st & 3rd
    p <- nrow(rplay[rplay$DP_FL==T&rplay$BAT_DEST_ID==0&rplay$RUN1_DEST_ID==0&(rplay$BASE_STATE=="110"|rplay$BASE_STATE=="111"),])
    q <- nrow(rplay[rplay$DP_FL==T&rplay$BAT_DEST_ID==0&rplay$RUN2_DEST_ID==0&(rplay$BASE_STATE=="110"|rplay$BASE_STATE=="111"),])
    p5 <- p/(p+q)
    
    #TPM A, B, C
    tpm.A <- matrix(c(rep(HR,8),(BB+X1B)*SBC,0,X1B*(1-p1)*SBC,X1B*SBC,0,0,X1B*(1-p1)*SBC,0,
                      X2B+SB,X2B*(1-p3),X2B+SB,X2B+SB,X2B*(1-p3),X2B*(1-p3),X2B+SB,X2B*(1-p3),rep(X3B,8),
                      0,BB+X1B*p1,BB,0,X1B*p1*(1-p2),X1B*p1,0,X1B*p1*(1-p2),
                      0,X1B*(1-p1),X1B*p1,BB,X1B*(1-p1)*(1-p2),X1B*(1-p1),X1B*p1,X1B*(1-p1)*(1-p2),
                      0,X2B*p3,0,0,X2B*p3,X2B*p3,0,X2B*p3,rep(0,4),BB+X1B*p2,BB,BB,BB+X1B*p2),8,8)
    tpm.B <- matrix(c(0,0,0,SF+FC3,rep(0,9),SF+FC3,0,0,0,FC1,0,0,0,FC1/2,SF*p4,0,0,0,FC2,0,0,0,SF*(1-p4)+FC3,
                      rep(0,8),SF*p4,rep(0,4),FC2/2,0,0,SF*(1-p4)+FC3/2,rep(0,4),FC2/2,FC1/2,0,FC3/2,rep(0,8)),8,8)
    tpm.C <- matrix(c(0,GDP,LDDP,LDDP,0,GDP,LDDP/3,rep(0,5),LDDP/2,LDDP/3,rep(0,6),GDP*p5+LDDP/2,LDDP/3,LDDP/3,
                      GDP*p5,rep(0,4),GDP*(1-p5),LDDP/3,LDDP/3,GDP*(1-p5),rep(0,7),LDDP/3,rep(0,7),LDDP/3,rep(0,7),
                      LDDP/3,rep(0,8)),8,8)
    tpm.A[is.nan(tpm.A)] = 0; tpm.B[is.nan(tpm.B)] = 0; tpm.C[is.nan(tpm.C)] = 0 
    
    #TPM ABC
    tpm.ABC <- matrix(0,25,25)
    tpm.ABC[1:8,1:8] <- tpm.A; tpm.ABC[9:16,9:16] <- tpm.A; tpm.ABC[17:24,17:24] <- tpm.A
    tpm.ABC[1:8,9:16] <- tpm.B; tpm.ABC[9:16,17:24] <- tpm.B; tpm.ABC[1:8,17:24] <- tpm.C
    tpm.ABC[,25] <- c(0,0,0,0,TP,0,0,TP,rowSums(tpm.C)+c(0,0,0,0,TP,0,0,TP),1-rowSums(tpm.A),1)
    tpm.ABC[1:8,9:16] <- tpm.B + diag(1-c(rowSums(tpm.ABC[1:8,])))
    tpm.ABC[9:16,17:24] <- tpm.B + diag(1-c(rowSums(tpm.ABC[9:16,])))
    assign(paste0("P.",i),tpm.ABC)
  }
  return(list(P.1=P.1,P.2=P.2,P.3=P.3,P.4=P.4,P.5=P.5,P.6=P.6,P.7=P.7,P.8=P.8,P.9=P.9,P.all=P.10))
}
