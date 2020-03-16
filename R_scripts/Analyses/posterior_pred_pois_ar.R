posteriorPoisAR <- function(s,Xs,alphas){
    cols <- colnames(s)
    N <- length(alphas)
    lambda <- exp(alphas + s[1] * Xs)
    predY <- rpois(n=N,lambda=lambda)
    return(predY)
}

#full_posterior_pred <- apply(samples[1000:niter,],1,posteriorPoisAR,Xs=X,alphas=grep("alpha\\[",colnames(samples)))
