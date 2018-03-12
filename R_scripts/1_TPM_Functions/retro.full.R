#Retrosheet Full Event Detail, Season Gamelog Detail, & Base Running Splits
#Inputs:  yyyy (Year)
#Data Files:  all"yyyy".csv, fields.csv, GL"yyyy".txt, game_log_header.csv

retro.full <- function(yyyy){
  #Location of Retrosheet files
  path <- "/Users/Documents/Retrosheets/download.folder/unzipped/"
  
  #All Events File
  event <- read.csv(paste(path,"all",yyyy,".csv",sep=""),header=FALSE)
  fields <- read.csv(paste(path,"fields.csv",sep=""))
  names(event) <- fields[,"Header"]
  event$SCORE_DIF <- (event$HOME_SCORE_CT-event$AWAY_SCORE_CT)*sign(event$BAT_HOME_ID-.5)
  event$BASE_STATE <- as.factor(paste(as.numeric(event$BASE1_RUN_ID!=""),as.numeric(event$BASE2_RUN_ID!=""),
                            as.numeric(event$BASE3_RUN_ID!=""),sep=""))
  event$BASE_ADV <- as.numeric(event$EVENT_CD==14|event$EVENT_CD==15|event$EVENT_CD==16|event$EVENT_CD==20|
                                 event$EVENT_CD==21|event$EVENT_CD==22|event$EVENT_CD==23)
  event$OUTS_CT <- as.factor(event$OUTS_CT)
  
  #Game Log File
  gamelog <- read.csv(paste(path,"GL",yyyy,".txt",sep=""),header=FALSE)
  glfields <- read.csv(paste(path,"game_log_header.csv",sep=""))
  names(gamelog) <- names(glfields)
  gamelog <- gamelog[,c(seq(106,130,3),10,23,31,seq(133,157,3),11,51,59,12,102,104,7,8)]
  
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
  
  #Base Running Splits, Outs, & Steals
  event.mat <- matrix(0,14,10)
  rownames(event.mat) <- c("p1","p2","p3","p4","p5","TP","FC1","FC2","FC3","LDDP","SF","GDP","SBC","SB")
  #Separate Statistics for Each Lineup Position
  for(i in 1:10){
    if(i < 10){evt <- event[event$BAT_LINEUP_ID==i,]; spl <- splay[splay$LINEUP_ID_RUN1==i,]} else {
      evt <- event; spl <- splay}
    #Season MLB Base Running Splits
    #Single - Runner on 1st Advances to 2nd or 3rd
    p <- nrow(evt[evt$H_FL==1&evt$EVENT_OUTS_CT==0&evt$ERR_CT==0&evt$BAT_DEST_ID==1&evt$RUN1_DEST_ID==2,])
    q <- nrow(evt[evt$H_FL==1&evt$EVENT_OUTS_CT==0&evt$ERR_CT==0&evt$BAT_DEST_ID==1&evt$RUN1_DEST_ID==3,])
    p1 <- p/(p+q)
    #Single - Runner on 2nd Advance to 3rd or Home
    p <- nrow(evt[evt$H_FL==1&evt$EVENT_OUTS_CT==0&evt$ERR_CT==0&evt$BAT_DEST_ID==1&evt$RUN2_DEST_ID==3,])
    q <- nrow(evt[evt$H_FL==1&evt$EVENT_OUTS_CT==0&evt$ERR_CT==0&evt$BAT_DEST_ID==1&evt$RUN2_DEST_ID==4,])
    p2 <- p/(p+q)
    #Double - Runner on 1st Advances to 3rd or Home
    p <- nrow(evt[evt$H_FL==2&evt$EVENT_OUTS_CT==0&evt$ERR_CT==0&evt$BAT_DEST_ID==2&evt$RUN1_DEST_ID==3,])
    q <- nrow(evt[evt$H_FL==2&evt$EVENT_OUTS_CT==0&evt$ERR_CT==0&evt$BAT_DEST_ID==2&evt$RUN1_DEST_ID==4,])
    p3 <- p/(p+q)
    #Sacrifice Fly - Runner on 2nd Advances or Stays
    p <- nrow(evt[evt$SF_FL==T&evt$EVENT_OUTS_CT==1&evt$RUN3_DEST_ID==4&evt$RUN2_DEST_ID==2,])
    q <- nrow(evt[evt$SF_FL==T&evt$EVENT_OUTS_CT==1&evt$RUN3_DEST_ID==4&evt$RUN2_DEST_ID==3,])
    p4 <- p/(p+q)
    #Ground Double Play Split - Runner Out at 1st & 2nd or 1st & 3rd
    p <- nrow(evt[evt$DP_FL==T&evt$BAT_DEST_ID==0&evt$RUN1_DEST_ID==0&(evt$BASE_STATE=="110"|evt$BASE_STATE=="111"),])
    q <- nrow(evt[evt$DP_FL==T&evt$BAT_DEST_ID==0&evt$RUN2_DEST_ID==0&(evt$BASE_STATE=="110"|evt$BASE_STATE=="111"),])
    p5 <- p/(p+q)
    #Triple Play
    TP <- nrow(event[event$TP_FL==T,])/nrow(event[event$OUTS_CT==0&event$BAT_EVENT_FL==T&
                                                    (event$BASE_STATE=="110"|event$BASE_STATE=="111"),])
    #Fielders Choice
    FC1 <- nrow(evt[evt$BAT_DEST_ID==0&evt$RUN1_DEST_ID==2&evt$H_FL==0&evt$ERR_CT==0&evt$EVENT_OUTS_CT==1&
                      evt$OUTS_CT!=2&(evt$BASE_STATE=="100"|evt$BASE_STATE=="101"),])/
      nrow(evt[evt$OUTS_CT!=2&(evt$BASE_STATE=="100"|evt$BASE_STATE=="101"),])
    FC2 <- nrow(evt[evt$RUN2_DEST_ID==3&evt$H_FL==0&evt$ERR_CT==0&evt$EVENT_OUTS_CT==1&
                      evt$OUTS_CT!=2&(evt$BASE_STATE=="010"|evt$BASE_STATE=="110"),])/
      nrow(evt[evt$OUTS_CT!=2&(evt$BASE_STATE=="010"|evt$BASE_STATE=="110"),])
    FC3 <- nrow(evt[evt$SF_FL==F&evt$BASE3_RUN_ID!=""&evt$RUN3_DEST_ID==4&evt$OUTS_CT!=2&evt$ERR_CT==0
                    &evt$EVENT_OUTS_CT==1,])/nrow(evt[evt$OUTS_CT!=2&evt$BASE3_RUN_ID!="",])
    #Line Drive Double Play
    LDDP <- (nrow(evt[evt$DP_FL==T,])-nrow(evt[grepl("GDP",evt$EVENT_TX),]))/
      nrow(evt[(evt$OUTS_CT==0|evt$OUTS_CT==1)&
                 (evt$BASE_STATE=="100"|evt$BASE_STATE=="110"|evt$BASE_STATE=="101"|evt$BASE_STATE=="111"),])
    #Sacrifice Fly & Ground Double Play
    SF <- nrow(evt[evt$SF_FL==T&evt$BASE3_RUN_ID!=""&evt$RUN3_DEST_ID==4&evt$OUTS_CT!=2&evt$DP_FL==F&
                     evt$ERR_CT==0,])/nrow(evt[evt$OUTS_CT!=2&evt$BASE3_RUN_ID!="",])
    GDP <- nrow(evt[grepl("GDP",evt$EVENT_TX),])/nrow(evt[(evt$OUTS_CT==0|evt$OUTS_CT==1)&
                                                            (evt$BASE_STATE=="100"|evt$BASE_STATE=="110"|evt$BASE_STATE=="101"|evt$BASE_STATE=="111"),])
    #Stolen Base Attempt Complement & Stolen Bases
    SBC <- 1 - nrow(spl)/nrow(evt[evt$EVENT_CD==14|evt$EVENT_CD==15|evt$EVENT_CD==16|evt$H_FL==1,])
    SB <- nrow(spl[spl$SB==T,])/nrow(evt[evt$BAT_EVENT_FL==T,])
    event.mat[,i] <- c(p1,p2,p3,p4,p5,TP,FC1,FC2,FC3,LDDP,SF,GDP,SBC,SB)
  }
  
  #OUTPUT
  return(list(event=event,gamelog=gamelog,splits=event.mat))
}
