nbCode <- nimbleCode({
   ###top-level regression
   B ~ dunif(-100,100)
   B0 ~ dunif(-100,100)
   sigB ~ dexp(0.01)
   sigB0 ~ dexp(0.01)
   for (k in 1:K) {
      ###low-level regression
      b[k] ~ dnorm(mean=B,sd=sigB)
      b0[k] ~ dnorm(mean=B0,sd=sigB0)
      for (n in 1:N){
        alpha[n,k] ~ dunif(1e-10,1-1e-10)
        lambda[n,k] <- exp(b0[k] + X[n] * b[k])
        Y[n,k] ~ dnegbin(size=lambda[n,k],prob=alpha[n,k])
      }
   }
})

#DATA
##No Chrono Uncertainty
#Y <- hist(simdates,breaks=seq(start,end))$counts
#X <- (Ndates - 1):0
#N <- length(Y)

##RECTS
Y <- rects_sample[,2:101]#dim(rects_sample)[2]]
Y[which(is.na(Y))] <- 0
N <- dim(Y)[1]
K <- dim(Y)[2]

##shifting the time scale
shifted_range <- -(sample_date_range - start)
X <- shifted_range[2]:shifted_range[1]

##Kennett
#X <- as.vector(Kennett[which(Kennett$TShift <= sample_date_range[2] & Kennett$TShift >= sample_date_range[1]),3])

nbData <- list(Y=Y,
                X=X)

nbConsts <- list(N=N,
                    K=K)
nbInits <- list(B=0,
                B0=0,
                b=rep(0,K),
                b0=rep(0,K),
                sigB=1,
                sigB0=1)

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
nbModel_conf$removeSamplers(c("B","B0","b","b0"))
nbModel_conf$addSampler(target=c("B","B0"),type="AF_slice")
for(k in 1:K){
   nbModel_conf$addSampler(target=c(paste("b[",k,"]",sep=""),paste("b0[",k,"]",sep="")),type="AF_slice")
   #nbModel_conf$addSampler(target=paste("alpha[1:",N,",",k,"]",sep="") ,type="RW_block")
}

#nbModel_conf$printSamplers()

#build MCMC
nbModelMCMC <- buildMCMC(nbModel_conf)

#compile MCMC to C++—much faster
C_nbModelMCMC <- compileNimble(nbModelMCMC,project=nbModel)

#number of MCMC iterations
niter=200000

#set seed for replicability
set.seed(1)

#save the MCMC chain (monitored variables) as a matrix
samples <- runMCMC(C_nbModelMCMC, niter=niter)

#save samples
save(samples,file="../Results/MCMC/Exp/mcmc_samples_exp_neg_hier.RData")
