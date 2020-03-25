poisCode <- nimbleCode({
   B0 ~ dnorm(mean=0,sd=100)
   B ~ dnorm(mean=0,sd=100)
   for (n in 1:N) {
      lambda[n] <- exp(B0 + X[n] * B)
      alpha[n] ~ dunif(1e-10,1-1e-10)
      Y[n] ~ dnegbin(size=lambda[n],prob=alpha[n])
      y[n] ~ dnegbin(size=lambda[n],prob=alpha[n])
   }
})

#DATA
##No Chrono Uncertainty
#Y <- rev(hist(simdates,breaks=seq(start,end))$counts)
#X <- 0:(Ndates - 1)
#N <- length(Y)
#X <- as.vector(X_sim[,1])

##RECTS
#Y <- rects_sample[,sample(1:dim(rects_sample[,-1])[2],size=1)]
#Y[which(is.na(Y))] <- 0
#N <- length(Y)

##random null
#X <- rnorm(n=N)

##shifting the time scale
#shifted_range <- -(sample_date_range - start)
#X <- shifted_range[2]:shifted_range[1]

##Kennett
#X <- as.vector(Kennett[which(Kennett$TShift <= sample_date_range[2] & Kennett$TShift >= sample_date_range[1] ),3])


poisData <- list(Y=Y,
                X=X)

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
##no chrono
#YDF <- cbind(seq(start,end + 1),X,Y)
##rects
#YDF <- cbind(Dates,X,Y)
#save(YDF,file="../Data/SimData/kennett_neg.RData")
#save(samples,file="../Results/MCMC/Exp/mcmc_samples_exp_neg_nochrono.RData")
