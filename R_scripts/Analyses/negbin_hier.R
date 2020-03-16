poisCode <- nimbleCode({
   ###top-level regression
   B ~ dunif(-1000,1000)
   B0 ~ dunif(-1000,1000)
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
Y <- rects_sample[,sample(1:dim(rects_sample[,-1])[2],size=100,replace=FALSE)]
Y[which(is.na(Y))] <- 0
N <- dim(Y)[1]
K <- dim(Y)[2]

##shifting the time scale
#shifted_range <- -(sample_date_range - start)
#X <- shifted_range[2]:shifted_range[1]

##Kennett
X <- as.vector(Kennett[which(Kennett$TShift <= sample_date_range[2] & Kennett$TShift >= sample_date_range[1] ),3])

poisData <- list(Y=Y,
                X=X)

poisConsts <- list(N=N,
                    K=K)
poisInits <- list(B=0,
                B0=0,
                b=rep(0,K),
                b0=rep(0,K),
                sigB=1,
                sigB0=1)

poisModel <- nimbleModel(code=poisCode,
                        data=poisData,
                        inits=poisInits,
                        constants=poisConsts)

#compile nimble model to C++ code—much faster runtime
C_poisModel <- compileNimble(poisModel, showCompilerOutput = FALSE)

#configure the MCMC
poisModel_conf <- configureMCMC(poisModel)

poisModel_conf$monitors <- c("B","B0","sigB","sigB0")
poisModel_conf$addMonitors2(c("b","b0"))

#build MCMC
poisModelMCMC <- buildMCMC(poisModel_conf)

#compile MCMC to C++—much faster
C_poisModelMCMC <- compileNimble(poisModelMCMC,project=poisModel)

#number of MCMC iterations
niter=100000

#set seed for replicability
set.seed(1)

#save the MCMC chain (monitored variables) as a matrix
samples <- runMCMC(C_poisModelMCMC, niter=niter)

#save samples
save(samples,file="../Results/MCMC/Exp/mcmc_samples_kennett_neg_hier.RData")
