poisARHierCode <- nimbleCode({
   ###top-level regression
   B ~ dunif(-1000,1000)
   sigB ~ dunif(1e-7,10)
   for (k in 1:K) {
      ###low-level regression
      b[k] ~ dnorm(mean=B,sd=sigB)
      ###autocorrelation part
      sigma[k] ~ dexp(0.1)
      rho[k] ~ dnorm(mean=0,sd=10)
      alpha0[k] ~ dnorm(mean=0,sd=1000)
      alpha[1,k] ~ dnorm(rho[k] * alpha0[k],sd=sigma[k])
      for (n in 2:N){
         alpha[n,k] ~ dnorm(rho[k] * alpha[n-1,k],sd=sigma[k])
      }
      for (n in 1:N){
         lambda[n,k] <- exp(alpha[n,k] + X[n] * b[k])
         Y[n,k] ~ dpois(lambda[n,k])
         y[n,k] ~ dpois(lambda[n,k])
      }
   }
})

Y <- rects_sample$counts[,11:21]
Y[which(is.na(Y))] <- 0
N <- dim(Y)[1]
K <- dim(Y)[2]
#resolution <- 10
X <- seq(0,(N-1))

poisARHierData <- list(Y=Y,
                  X=X)

poisARHierConsts <- list(N=N,
                     K=K)

poisARHierInits <- list(B=0,
                        sigB=1,
                        b=rep(0,K),
                        sigma=rep(1,K),
                        alpha0=rep(1,K),
                        rho=rep(1,K),
                        alpha=matrix(1,ncol=K,nrow=N))

poisARHierModel <- nimbleModel(code=poisARHierCode,
                        data=poisARHierData,
                        inits=poisARHierInits,
                        constants=poisARHierConsts)

#compile nimble model to C++ code—much faster runtime
C_poisARHierModel <- compileNimble(poisARHierModel, showCompilerOutput = FALSE)

#configure the MCMC
poisARHierModel_conf <- configureMCMC(poisARHierModel)

#select the variables that we want to monitor in the MCMC chain
poisARHierModel_conf$addMonitors(c("b","alpha0","alpha"))
poisARHierModel_conf$addMonitors2(c("y"))

#samplers
poisARHierModel_conf$removeSamplers(c("alpha","b"))
for(k in 1:K){
    poisARHierModel_conf$addSampler(target=c(paste("alpha[1:",N,",",k,"]",sep=""),paste("b[",k,"]",sep="")),type="AF_slice")
}

#build MCMC
poisARHierModelMCMC <- buildMCMC(poisARHierModel_conf)

#compile MCMC to C++—much faster
C_poisARHierModelMCMC <- compileNimble(poisARHierModelMCMC,project=poisARHierModel)

#number of MCMC iterations
niter=100000

#set seed for replicability
set.seed(1)

#call the C++ compiled MCMC model
samples <- runMCMC(C_poisARHierModelMCMC, niter=niter)

#save samples
save(samples,file="../Results/MCMC/Linear/linear_pos_2_neg.RData")
