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
        #alpha[n,k] ~ dunif(1e-10,1)
        lambda[n,k] <- exp(b0[k] + X[n] * b[k])
        Y[n,k] ~ dpois(lambda[n,k])
      }
   }
})

#DATA
##No Chrono Uncertainty
#Y <- rev(hist(simdates,breaks=seq(start,end))$counts)
#X <- 0:(span - 1)
#N <- length(Y)

##RECTS
Y <- rects_sample[,sample(2:dim(rects_sample)[2],size=100,replace=F)]
Y[which(is.na(Y))] <- 0
N <- dim(Y)[1]
K <- dim(Y)[2]

##random null
#X <- rnorm(n=N)

##shifting the time scale
#shifted_range <- -(sample_date_range - start)
X <- shifted_range[2]:shifted_range[1]

##Kennett
#X <- as.vector(Kennett[which(Kennett$TShift <= sample_date_range[2] & Kennett$TShift >= sample_date_range[1] ),3])


nbData <- list(Y=Y,
                X=X)

nbConsts <- list(N=N,
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
#nbModel_conf$addMonitors2(c("y"))

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
