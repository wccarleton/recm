calSample <- function(x){
   date_list <-  lapply(x,function(x){
                                    d <- sample(x = x[,1],
                                    size = 1,
                                    replace = T,
                                    prob = x[,2])
                                    return(d)
                                    })
   dates <- unlist(date_list)
   return(dates)
}

calSampleApprox <- function(x,t1,t2,r){
    n <- length(x)
    funs <- lapply(x,approxfun)
    y_list <- lapply(1:n,function(j)funs[[j]](seq(t1,t2,r)))
    y_mat <- do.call(cbind,y_list)
    y_mat[which(is.na(y_mat))] <- 0
    return(y_mat)
}
