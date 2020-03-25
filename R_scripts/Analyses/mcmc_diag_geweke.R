burnin <- 20000

#monotonic function
mcmc_results_files <- list.files("../Results/MCMC/Exp/")

for(j in mcmc_results_files){
   csv_file_name <- strsplit(j,split=".RData")[[1]]
   load(file=paste("../Results/MCMC/Exp/",j,sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_mat <- cbind(samples$samples,samples$samples2)
   geweke_z <- geweke.diag(mcmc_mat[burnin:niter,])$z
   print(j)
   print(geweke_z[which(geweke_z > 2.96)])
   write.csv(geweke_z,file=paste("../Results/MCMC/Exp/geweke_",csv_file_name,".csv",sep=""))
}
