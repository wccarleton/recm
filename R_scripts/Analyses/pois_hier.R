poisHierCode <- nimbleCode({
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
         lambda[n,k] <- exp(b0[k] + X[n] * b[k])
         Y[n,k] ~ dpois(lambda[n,k])
      }
   }
})

Y <- rects_sample$counts[,11:21]
Y[which(is.na(Y))] <- 0

K <- dim(Y)[2]

#first_nonzero_row <- 1 + (min(which(t(Y)!=0)) %/% K)
#last_nonszero_row <- max(which(t(Y)!=0)) %/% K
#Y <- Y[first_nonzero_row:last_nonszero_row,]

N <- dim(Y)[1]

X <- seq(0,(N-1))

poisHierData <- list(Y=Y,
                  X=X)

poisHierConsts <- list(N=N,
                     K=K)

poisHierInits <- list(B=0,
                        B0=0,
                        b=rep(0,K),
                        b0=rep(0,K),
                        sigB=1,
                        sigB0=1)

poisHierModel <- nimbleModel(code=poisHierCode,
                        data=poisHierData,
                        inits=poisHierInits,
                        constants=poisHierConsts)

#compile nimble model to C++ code—much faster runtime
C_poisHierModel <- compileNimble(poisHierModel, showCompilerOutput = FALSE)

#configure the MCMC
poisHierModel_conf <- configureMCMC(poisHierModel)

#select the variables that we want to monitor in the MCMC chain
poisHierModel_conf$addMonitors(c("b","b0"))

#samplers
poisHierModel_conf$removeSamplers(c("B","B0","b","b0"))
poisHierModel_conf$addSampler(target=c("B","B0"),type="AF_slice")
for(k in 1:K){
    poisHierModel_conf$addSampler(target=c(paste("b[",k,"]",sep=""),paste("b0[",k,"]",sep="")),type="AF_slice")
}

#build MCMC
poisHierModelMCMC <- buildMCMC(poisHierModel_conf,thin=9,enableWAIC = TRUE)

#compile MCMC to C++—much faster
C_poisHierModelMCMC <- compileNimble(poisHierModelMCMC,project=poisHierModel)

#number of MCMC iterations
niter=100000

#set seed for replicability
set.seed(1)

#call the C++ compiled MCMC model
samples <- runMCMC(C_poisHierModelMCMC, niter=niter)

#save samples
save(samples,file="../Results/MCMC/Linear/linear_pos_2_noar.RData")
