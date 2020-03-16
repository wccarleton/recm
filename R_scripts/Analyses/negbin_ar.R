poisARCode <- nimbleCode({
   ###regression params
   B ~ dnorm(mean=0,sd=1000)
   ###autocorrelation part
   sigma ~ dexp(5)
   rho ~ dnorm(mean=0,sd=10)
   #alpha0 ~ dnorm(mean=0,sd=1000)
   #alpha[1] ~ dnorm(rho * alpha0,sd=sigma)
   for (n in 2:N){
      alpha[n] ~ dunif(1e-10,1-1e-10)#dnorm(rho * alpha[n-1],sd=sigma)
   }
   ###linear model
   for (n in 1:N) {
      lambda[n] <- exp(X[n] * B)
      Y[n] ~ dnegbin(size=lambda[n],prob=alpha[n])
      #y[n] ~ dpois(lambda[n])
   }
})

#DATA
#Y <- c14ensemble[,10]
#Y[which(is.na(Y))] <- 0
#N <- dim(c14ensemble)[1]

#X <- c(0:(N-1))

Y <- rects_sample$counts[,11]
Y[which(is.na(Y))] <- 0
N <- length(Y)
X <- seq(0,(N-1))


poisARData <- list(Y=Y,
                  X=X)

poisARConsts <- list(N=N)

poisARInits <- list(B=0,
                    alpha0=0,
                    alpha=rep(0,N),
                    sigma=1,
                    rho=1)

poisARModel <- nimbleModel(code=poisARCode,
                        data=poisARData,
                        inits=poisARInits,
                        constants=poisARConsts)

#compile nimble model to C++ code—much faster runtime
C_poisARModel <- compileNimble(poisARModel, showCompilerOutput = FALSE)

#configure the MCMC
poisARModel_conf <- configureMCMC(poisARModel)

#change samplers
poisARModel_conf$removeSamplers(c("alpha","B","B0"))
poisARModel_conf$addSampler(target=c("alpha","B","B0"),type="AF_slice")

#select the variables that we want to monitor in the MCMC chain
poisARModel_conf$addMonitors(c("alpha0","alpha"))
poisARModel_conf$addMonitors2(c("y"))

#build MCMC
poisARModelMCMC <- buildMCMC(poisARModel_conf,enableWAIC = FALSE)

#compile MCMC to C++—much faster
C_poisARModelMCMC <- compileNimble(poisARModelMCMC,project=poisARModel)

#number of MCMC iterations
niter=20000

#set seed for replicability
set.seed(1)

#save the MCMC chain (monitored variables) as a matrix
samples <- runMCMC(C_poisARModelMCMC, niter=niter)

#save samples
#save(samples,file="../Results/mcmc_test_1.RData")
