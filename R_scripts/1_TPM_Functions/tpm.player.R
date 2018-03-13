#Season Player Statistics TPM (pre-conversion)
#Inputs: batter (Batter ID), pitcher (Pitcher ID), line (Lineup Position), event (Event File), glmm (GLMM)
#Packages:
#Functions Referenced: OBP.glmm.R

tpm.player <- function(batter,pitcher,line,event,glmm){
  
  #Filter Event Data by Batter or Pitcher
  retroYYYY <- event
  rplay <- retroYYYY$event[retroYYYY$event$BAT_ID==batter&retroYYYY$event$BAT_EVENT_FL==T,]
  splay <- retroYYYY$event[retroYYYY$event$BASE1_RUN_ID==batter,]
  n <- nrow(rplay)
  
  #League Season Splits from Lineup Position 'line'
  p1 <- retroYYYY$splits[1,line]; p2 <- retroYYYY$splits[2,line]; p3 <- retroYYYY$splits[3,line]; 
  p4 <- retroYYYY$splits[4,line]; p5 <- retroYYYY$splits[5,line]; TP <- retroYYYY$splits[6,line]; 
  FC1 <- retroYYYY$splits[7,line]; FC2 <- retroYYYY$splits[8,line]; FC3 <- retroYYYY$splits[9,line]; 
  LDDP <- retroYYYY$splits[10,line]; SF.pop <- retroYYYY$splits[11,line]; GDP.pop <- retroYYYY$splits[12,line]
  SBC.pop <- retroYYYY$splits[13,line]; SB.pop <- retroYYYY$splits[14,line]
  
  #Player Batting Statistics
  BB <- nrow(rplay[rplay$EVENT_CD==14|rplay$EVENT_CD==15|rplay$EVENT_CD==16,])/n
  X1B <- nrow(rplay[rplay$H_FL==1,])/n
  X2B <- nrow(rplay[rplay$H_FL==2,])/n
  X3B <- nrow(rplay[rplay$H_FL==3,])/n
  HR <- nrow(rplay[rplay$H_FL==4,])/n
  SBC <- 1 - nrow(splay[splay$RUN1_SB_FL==T|splay$RUN1_CS_FL==T,])/nrow(splay)
  SB <- nrow(splay[splay$RUN1_SB_FL==T,])/n
  SF <- nrow(rplay[rplay$SF_FL==T&rplay$BASE3_RUN_ID!=""&rplay$RUN3_DEST_ID==4&rplay$OUTS_CT!=2&rplay$DP_FL==F&
                   rplay$ERR_CT==0,])/nrow(rplay[rplay$OUTS_CT!=2&rplay$BASE3_RUN_ID!="",])
  GDP <- nrow(rplay[grepl("GDP",rplay$EVENT_TX),])/
    	 nrow(rplay[(rplay$OUTS_CT==0|rplay$OUTS_CT==1)&
             (rplay$BASE_STATE=="100"|rplay$BASE_STATE=="110"|rplay$BASE_STATE=="101"|rplay$BASE_STATE=="111"),])
  
  #Matrix Error Fixes
  if(BB+X1B+X2B+X3B+HR>=.6|BB+X1B+X2B+X3B+HR=="NaN"){BB <- 0; X1B <- 0; X2B <- 0; X3B <- 0; HR <- 0}
  if(SF=="NaN"){SF <- 0}; if(GDP=="NaN"){GDP <- 0}; 
  if(BB+X1B+X2B+X3B+HR+SF+FC3+FC1+GDP+LDDP>1|BB+X1B+X2B+X3B+HR+FC2+GDP+LDDP>1)
  {SF <- SF.pop; GDP <- GDP.pop; SBC <- SBC.pop; SB <- SB.pop}
  if(SF>.3){SF <- SF.pop}; if(GDP>.1){GDP <- GDP.pop}
  
  #Situational Scaling Vector
  Out0.v <- c(OBP.glmm(pitcher,"0","000",glmm),OBP.glmm(pitcher,"0","100",glmm),OBP.glmm(pitcher,"0","010",glmm),
              OBP.glmm(pitcher,"0","001",glmm),OBP.glmm(pitcher,"0","110",glmm),OBP.glmm(pitcher,"0","101",glmm),
              OBP.glmm(pitcher,"0","011",glmm),OBP.glmm(pitcher,"0","111",glmm))
  Out1.v <- c(OBP.glmm(pitcher,"1","000",glmm),OBP.glmm(pitcher,"1","100",glmm),OBP.glmm(pitcher,"1","010",glmm),
              OBP.glmm(pitcher,"1","001",glmm),OBP.glmm(pitcher,"1","110",glmm),OBP.glmm(pitcher,"1","101",glmm),
              OBP.glmm(pitcher,"1","011",glmm),OBP.glmm(pitcher,"1","111",glmm))
  Out2.v <- c(OBP.glmm(pitcher,"2","000",glmm),OBP.glmm(pitcher,"2","100",glmm),OBP.glmm(pitcher,"2","010",glmm),
              OBP.glmm(pitcher,"2","001",glmm),OBP.glmm(pitcher,"2","110",glmm),OBP.glmm(pitcher,"2","101",glmm),
              OBP.glmm(pitcher,"2","011",glmm),OBP.glmm(pitcher,"2","111",glmm))
  
  #TPM A,B,C
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
  tpm.ABC[1:8,1:8] <- tpm.A*Out0.v; tpm.ABC[9:16,9:16] <- tpm.A*Out1.v; tpm.ABC[17:24,17:24] <- tpm.A*Out2.v
  tpm.ABC[1:8,9:16] <- tpm.B; tpm.ABC[9:16,17:24] <- tpm.B; tpm.ABC[1:8,17:24] <- tpm.C
  tpm.ABC[,25] <- c(0,0,0,0,TP,0,0,TP,rowSums(tpm.C)+c(0,0,0,0,TP,0,0,TP),1-rowSums(tpm.A),1)
  tpm.ABC[1:8,9:16] <- tpm.B + diag(1-c(rowSums(tpm.ABC[1:8,])))
  tpm.ABC[9:16,17:24] <- tpm.B + diag(1-c(rowSums(tpm.ABC[9:16,])))
  
  #OUTPUT - 25x25 Player TPM
  return(tpm.ABC)
}
