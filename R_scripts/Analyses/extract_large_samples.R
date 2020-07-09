indeces <- matrix(1:1000,ncol=10)
apply(indeces,2,function(x){
    mcmc_params <- as.matrix(C_nbModelMCMC$mvSamples2[,x])
    save(mcmc_params,file=paste("../mcmc_params_",x[1],".RData",sep=""))
})
