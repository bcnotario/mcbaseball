#TPM Outcome Reference Matrix (85x85)

tpm.label <- function(x){
  
  #TPM A,B,C
  tpm.A <- matrix(c(rep("HR",8),"(BB+X1B)*SBC",0,"X1B*(1-p1)*SBC","X1B*SBC",0,0,"X1B*(1-p1)*SBC",0,
                    "X2B+SB","X2B*(1-p3)","X2B+SB","X2B+SB","X2B*(1-p3)","X2B*(1-p3)","X2B+SB","X2B*(1-p3)",rep("X3B",8),
                    0,"BB+X1B*p1","BB",0,"X1B*p1*(1-p2)","X1B*p1",0,"X1B*p1*(1-p2)",
                    0,"X1B*(1-p1)","X1B*p1","BB","X1B*(1-p1)*(1-p2)","X1B*(1-p1)","X1B*p1","X1B*(1-p1)*(1-p2)",
                    0,"X2B*p3",0,0,"X2B*p3","X2B*p3",0,"X2B*p3",rep(0,4),"BB+X1B*p2","BB","BB","BB+X1B*p2"),8,8)
  tpm.B <- matrix(c(0,0,0,"SF+FC3",rep(0,9),"SF+FC3",0,0,0,"FC1",0,0,0,"FC1/2","SF*p4",0,0,0,"FC2",0,0,0,"SF*(1-p4)+FC3",
                    rep(0,8),"SF*p4",rep(0,4),"FC2/2",0,0,"SF*(1-p4)+FC3/2",rep(0,4),"FC2/2","FC1/2",0,"FC3/2",rep(0,8)),8,8)
  diag(tpm.B) <- rep("O*",8)
  tpm.C <- matrix(c(0,"GDP","LDDP","LDDP",0,"GDP","LDDP/3",rep(0,5),"LDDP/2","LDDP/3",rep(0,6),"GDP*p5+LDDP/2","LDDP/3","LDDP/3",
                    "GDP*p5",rep(0,4),"GDP*(1-p5)","LDDP/3","LDDP/3","GDP*(1-p5)",rep(0,7),"LDDP/3",rep(0,7),"LDDP/3",rep(0,7),
                    "LDDP/3",rep(0,8)),8,8)
  
  #TPM ABC
  tpm.ABC <- matrix(0,25,25)
  tpm.ABC[1:8,1:8] <- tpm.A; tpm.ABC[9:16,9:16] <- tpm.A; tpm.ABC[17:24,17:24] <- tpm.A
  tpm.ABC[1:8,9:16] <- tpm.B; tpm.ABC[9:16,17:24] <- tpm.B; tpm.ABC[1:8,17:24] <- tpm.C
  tpm.ABC[,25] <- c(0,0,0,0,"TP",0,0,"TP",rep("DP",8),rep("O*",8),1)
  tpm.convert(tpm.ABC)
}
