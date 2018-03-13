#OBP Pitcher & Situation Adjustment by GLMM for Run TPM
#Inputs: pitcher (Pitcher ID), outs (Out Situation), baserun (Base Runner Situation), glmm (GLMM)
#Packages: faraway

OBP.glmm <- function(pitcher,outs,baserun,glmm){
  ilogit(predict(glmm,newdata=data.frame(PIT_ID=pitcher,OUTS_CT=outs,BASE_STATE=baserun)))/
    ilogit(predict(glmm,newdata=data.frame(PIT_ID=pitcher,OUTS_CT="0",BASE_STATE="000"),re.form=~0))}
