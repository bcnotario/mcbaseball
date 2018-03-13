#Expected Runners for all Lineup Permutations
#Inputs: tpm.list (List of 25x25 TPMs of Starting Lineup)
#Packages: e1071
#Functions: tpm.lineup.lead, tpm.lineup.runs

sim.runopt <- function(tpm.list){
  
  #Permatutation Matrix (8 Factorial elements) w/9th place fixed (approx 3 hours)
  perm.mat <- cbind(permutations(8),rep(9,factorial(8)),vector(length=factorial(8)))
  
  for(i in 1:factorial(8)){
    tpm.plist <- list(tpm.list[[perm.mat[i,1]]],tpm.list[[perm.mat[i,2]]],tpm.list[[perm.mat[i,3]]],
                      tpm.list[[perm.mat[i,4]]],tpm.list[[perm.mat[i,5]]],tpm.list[[perm.mat[i,6]]],
                      tpm.list[[perm.mat[i,7]]],tpm.list[[perm.mat[i,8]]],tpm.list[[perm.mat[i,9]]])
    leadoff <- tpm.lineup.lead(tpm.plist)
    
    #Expected Runs per Lineup Order
    perm.mat[i,10] <- 9*tpm.lineup.runs(tpm.plist)%*%leadoff
  }
  
  #OUTPUT - Maximum Expected Runs, Optimal Lineup, All Expected Runs, Permutation Matrix
  return(list(r.max=max(perm.mat[,10]),line.opt=perm.mat[perm.mat[,10]==max(perm.mat[,10]),1:9],
              r.list=perm.mat[,10],perm.mat=perm.mat))
}
