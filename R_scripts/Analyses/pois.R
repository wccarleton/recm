poisCode <- nimbleCode({
   B0 ~ dnorm(mean=0,sd=1000)
   B ~ dnorm(mean=0,sd=1000)
   for (n in 1:N) {
      lambda[n] <- exp(B0 + X[n] * B)
      Y[n] ~ dpois(lambda[n])
      y[n] ~ dpois(lambda[n])
   }
})

#DATA
Y <- rects_sample[,10]
Y[which(is.na(Y))] <- 0
N <- length(Y)
#X <- seq(0,(N-1)*10,10)
X <- Kennett[which(Kennett$TShift %in% Dates),c(3,5)]

poisData <- list(Y=Y,
                X=as.vector(X[,1]))

poisConsts <- list(N=N)

poisInits <- list(B=0,
                B0=0)

poisModel <- nimbleModel(code=poisCode,
                        data=poisData,
                        inits=poisInits,
                        constants=poisConsts)

#compile nimble model to C++ code—much faster runtime
C_poisModel <- compileNimble(poisModel, showCompilerOutput = FALSE)

#configure the MCMC
poisModel_conf <- configureMCMC(poisModel)

poisModel_conf$addMonitors2(c("y"))

#build MCMC
poisModelMCMC <- buildMCMC(poisModel_conf,thin=1,enableWAIC = TRUE)

#compile MCMC to C++—much faster
C_poisModelMCMC <- compileNimble(poisModelMCMC,project=poisModel)

#number of MCMC iterations
niter=100000

#set seed for replicability
set.seed(1)

#save the MCMC chain (monitored variables) as a matrix
samples <- runMCMC(C_poisModelMCMC, niter=niter)

#save samples
#save(samples,file="../Results/mcmc_samples_spdf_pois.RData")
#save(samples,file="../Results/mcmc_samples_kde_pois.RData")
