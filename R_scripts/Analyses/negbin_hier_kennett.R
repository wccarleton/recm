nbCode <- nimbleCode({
   ###top-level regression
   B ~ dnorm(0,100)
   B0 ~ dnorm(0,100)
   sigB ~ dunif(1e-10,10)
   sigB0 ~ dunif(1e-10,10)
   for (k in 1:K) {
      ###low-level regression
      b[k] ~ dnorm(mean=B,sd=sigB)
      b0[k] ~ dnorm(mean=B0,sd=sigB0)
      for (n in 1:N){
        alpha[n,k] ~ dunif(1e-10,1)
        lambda[n,k] <- exp(b0[k] + X[n] * b[k])
        Y[n,k] ~ dnegbin(size=lambda[n,k],prob=alpha[n,k])
      }
   }
})

#DATA

##RECTS
Y <- rects_sample[,sample(2:101,size=50,replace=F)]#-1]
Y[which(is.na(Y))] <- 0
N <- dim(Y)[1]
K <- dim(Y)[2]

##Kennett
X <- as.vector(Kennett[which(Kennett$TShift <= sample_date_range[2] & Kennett$TShift >= sample_date_range[1]),3])

##sub sampling for memory/computation
Nsub <- seq(1,N,10)

nbData <- list(Y=Y[Nsub,],
                X=X[Nsub])

nbConsts <- list(N=length(Nsub),
                    K=K)
nbInits <- list(B=0,
                B0=0,
                b=rep(0,K),
                b0=rep(0,K),
                sigB=0.0001,
                sigB0=0.0001)

nbModel <- nimbleModel(code=nbCode,
                        data=nbData,
                        inits=nbInits,
                        constants=nbConsts)

#compile nimble model to C++ code—much faster runtime
C_nbModel <- compileNimble(nbModel, showCompilerOutput = FALSE)

#configure the MCMC
nbModel_conf <- configureMCMC(nbModel)

nbModel_conf$monitors <- c("B","B0","sigB","sigB0")
nbModel_conf$addMonitors2(c("b","b0"))

#samplers
nbModel_conf$removeSamplers(c("B","B0","b","b0","sigB","sigB0"))
nbModel_conf$addSampler(target=c("B","B0","sigB","sigB0"),type="AF_slice")
for(k in 1:K){
   nbModel_conf$addSampler(target=c(paste("b[",k,"]",sep=""),paste("b0[",k,"]",sep="")),type="AF_slice")
}

#nbModel_conf$printSamplers()

#thinning to conserve memory when the samples are saved below
nbModel_conf$setThin(1)
nbModel_conf$setThin2(1)

#build MCMC
nbModelMCMC <- buildMCMC(nbModel_conf)

#compile MCMC to C++—much faster
C_nbModelMCMC <- compileNimble(nbModelMCMC,project=nbModel)

#number of MCMC iterations
niter=500000

#set seed for replicability
set.seed(1)

#save the MCMC chain (monitored variables) as a matrix
samples <- runMCMC(C_nbModelMCMC, niter=niter)

#save samples
if(B > 0){
   fileout <- paste("../Results/MCMC/Kennett/mcmc_samples_kennett_","pos_","hier.RData",sep="")
}else{
   fileout <- paste("../Results/MCMC/Kennett/mcmc_samples_kennett_","neg_","hier.RData",sep="")
}

save(samples,file=fileout)
