nbCode <- nimbleCode({
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
Y <- rev(hist(simdates,breaks=seq(start,end))$counts)
X <- 0:(span - 1)
N <- length(Y)

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


nbData <- list(Y=Y,
                X=X)

nbConsts <- list(N=N)

nbInits <- list(B=0,
                B0=0)

nbModel <- nimbleModel(code=nbCode,
                        data=nbData,
                        inits=nbInits,
                        constants=nbConsts)

#compile nimble model to C++ code—much faster runtime
C_nbModel <- compileNimble(nbModel, showCompilerOutput = FALSE)

#configure the MCMC
nbModel_conf <- configureMCMC(nbModel)
nbModel_conf$addMonitors2(c("y"))

#samplers
nbModel_conf$removeSamplers(c("B","B0"))
nbModel_conf$addSampler(target=c("B","B0"),type="AF_slice")

#build MCMC
nbModelMCMC <- buildMCMC(nbModel_conf,thin=1,enableWAIC = TRUE)

#compile MCMC to C++—much faster
C_nbModelMCMC <- compileNimble(nbModelMCMC,project=nbModel)

#number of MCMC iterations
niter=100000

#set seed for replicability
set.seed(1)

#save the MCMC chain (monitored variables) as a matrix
samples <- runMCMC(C_nbModelMCMC, niter=niter)

#save samples
##no chrono
#YDF <- cbind(seq(start,end + 1),X,Y)
##rects
#YDF <- cbind(Dates,X,Y)
#save(YDF,file="../Data/SimData/kennett_neg.RData")
#save(samples,file="../Results/MCMC/Exp/mcmc_samples_exp_neg_nochrono.RData")
