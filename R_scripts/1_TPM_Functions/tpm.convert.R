#Convert 25x25 TPM to 85x85 Run Tracking TPM
#Inputs: tpm.mat (25x25 TPM matrix)
#Packages: 

tpm.convert <- function(tpm.mat){
  
  #Matrix A1
  A00 <- tpm.mat[1:8,1:8]
  A00[,1] <- 0; A00[-1,2:4] <- 0; A00[-c(2:4),5:7] <- 0; A00[-c(5:7),8] <- 0
  A01 <- tpm.mat[1:8,1:8]
  A01[-1,1] <- 0; A01[-c(2:4),2:4] <- 0; A01[-c(5:7),5:7] <- 0; A01[-8,8] <- 0
  A02 <- tpm.mat[1:8,1:7]
  A02[-c(2:4),1] <- 0; A02[-c(5:7),2:4] <- 0; A02[-8,5:7] <- 0
  A03 <- tpm.mat[1:8,1:4]
  A03[-c(5:7),1] <- 0; A03[1:7,2:4] <- 0; A03[8,-c(2:4)] <- 0
  A04 <- matrix(c(rep(0,7),tpm.mat[8,1]),8,1)
  tpm.A1 <- rbind(cbind(A00,A01,A02,A03,A04),cbind(diag(8),matrix(0,8,20)),cbind(diag(7),matrix(0,7,21)),
                  cbind(diag(4),matrix(0,4,24)),c(1,rep(0,27)))
  
  #Matrix A2
  A00 <- tpm.mat[9:16,9:16]
  A00[,1] <- 0; A00[-1,2:4] <- 0; A00[-c(2:4),5:7] <- 0; A00[-c(5:7),8] <- 0
  A01 <- tpm.mat[9:16,9:16]
  A01[-1,1] <- 0; A01[-c(2:4),2:4] <- 0; A01[-c(5:7),5:7] <- 0; A01[-8,8] <- 0
  A02 <- tpm.mat[9:16,9:15]
  A02[-c(2:4),1] <- 0; A02[-c(5:7),2:4] <- 0; A02[-8,5:7] <- 0
  A03 <- tpm.mat[9:16,9:12]
  A03[-c(5:7),1] <- 0; A03[1:7,2:4] <- 0; A03[8,-c(2:4)] <- 0
  A04 <- matrix(c(rep(0,7),tpm.mat[16,9]),8,1)
  tpm.A2 <- rbind(cbind(A00,A01,A02,A03,A04),cbind(diag(8),matrix(0,8,20)),cbind(diag(7),matrix(0,7,21)),
                  cbind(diag(4),matrix(0,4,24)),c(1,rep(0,27)))
  
  #Matrix A3
  A00 <- tpm.mat[17:24,17:24]
  A00[,1] <- 0; A00[-1,2:4] <- 0; A00[-c(2:4),5:7] <- 0; A00[-c(5:7),8] <- 0
  A01 <- tpm.mat[17:24,17:24]
  A01[-1,1] <- 0; A01[-c(2:4),2:4] <- 0; A01[-c(5:7),5:7] <- 0; A01[-8,8] <- 0
  A02 <- tpm.mat[17:24,17:23]
  A02[-c(2:4),1] <- 0; A02[-c(5:7),2:4] <- 0; A02[-8,5:7] <- 0
  A03 <- tpm.mat[17:24,17:20]
  A03[-c(5:7),1] <- 0; A03[1:7,2:4] <- 0; A03[8,-c(2:4)] <- 0
  A04 <- matrix(c(rep(0,7),tpm.mat[24,17]),8,1)
  tpm.A3 <- rbind(cbind(A00,A01,A02,A03,A04),cbind(diag(8),matrix(0,8,20)),cbind(diag(7),matrix(0,7,21)),
                  cbind(diag(4),matrix(0,4,24)),c(1,rep(0,27)))
  
  #Matrix B1
  B00 <- tpm.mat[1:8,9:16]
  B00[2:8,1] <- 0; B00[5:7,2:4] <- 0; B00[8,1:7] <- 0
  B01 <- tpm.mat[1:8,9:16]
  B01[-c(2:4),1] <- 0; B01[-c(5:7),2:4] <- 0; B01[c(5:7),5:7] <- 0; B01[8,-c(1:7)] <- 0
  B02 <- tpm.mat[1:8,9:15]
  B02[-c(5:7),1] <- 0; B02[1:7,2:7] <- 0; B02[8,-c(2:4)] <- 0
  B03 <- matrix(c(rep(0,7),tpm.mat[8,9],rep(0,24)),8,4)
  tpm.B1 <- rbind(cbind(B00,B01,B02,B03,matrix(0,8,1)),matrix(0,20,28))
  
  #Matrix B2
  B00 <- tpm.mat[9:16,17:24]
  B00[2:8,1] <- 0; B00[5:7,2:4] <- 0; B00[8,1:7] <- 0
  B01 <- tpm.mat[9:16,17:24]
  B01[-c(2:4),1] <- 0; B01[-c(5:7),2:4] <- 0; B01[c(5:7),5:7] <- 0; B01[8,-c(1:7)] <- 0
  B02 <- tpm.mat[9:16,17:23]
  B02[-c(5:7),1] <- 0; B02[1:7,2:7] <- 0; B02[8,-c(2:4)] <- 0
  B03 <- matrix(c(rep(0,7),tpm.mat[16,17],rep(0,24)),8,4)
  tpm.B2 <- rbind(cbind(B00,B01,B02,B03,matrix(0,8,1)),matrix(0,20,28))
  
  #Matrix C
  C00 <- tpm.mat[1:8,17:24]
  C00[c(6,7),1] <- 0; C00[8,c(3,4)] <- 0
  C01 <- tpm.mat[1:8,17:24]
  C01[1:7,2:8] <- 0; C01[-c(6,7),1] <- 0; C01[8,-c(3,4)] <- 0
  tpm.C <- rbind(cbind(C00,C01,matrix(0,8,12)),matrix(0,20,28))
  
  #Matrix ABC
  tpm.ABC <- rbind(cbind(tpm.A1,tpm.B1,tpm.C,c(tpm.mat[1:8,25],rep(0,20))),
                   cbind(matrix(0,28,28),tpm.A2,tpm.B2,c(tpm.mat[9:16,25],rep(0,20))),
                   cbind(matrix(0,28,56),tpm.A3,c(tpm.mat[17:24,25],rep(0,20))),c(rep(0,84),1))
  
  #OUTPUT - 85x85 TPM
  return(tpm.ABC)
}
