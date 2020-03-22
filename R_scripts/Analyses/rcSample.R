calSampleApprox <- function(x,t1,t2,r){
    n <- length(x)
    funs <- lapply(x,approxfun)
    y_list <- lapply(1:n,function(j)funs[[j]](seq(t1,t2,r)))
    y_mat <- do.call(cbind,y_list)
    y_mat[which(is.na(y_mat))] <- 0
    return(y_mat)
}

resolution <- 1

ndates <- Ndates
nsamps <- 1000

sample_date_range <- range(unlist(lapply(c14post[1:ndates],function(x)range(x[,1]))))

c14_matrix <- calSampleApprox(c14post[1:ndates],sample_date_range[1]+1,sample_date_range[2],resolution)

Dates <- seq(sample_date_range[1],sample_date_range[2],resolution)

rects_sample <- data.frame(Date=Dates)
for(a in 1:nsamps){
    count_sample <- apply(c14_matrix,2,function(x)sample(Dates,size=1,prob=x))
    count_df <- as.data.frame(table(count_sample))
    names(count_df) <- c("Date","Count")
    rects_sample <- merge(rects_sample,count_df,by="Date",all=T)
}

rects_sample <- as.matrix(rects_sample[,-1])
rects_sample[which(is.na(rects_sample))] <- 0
colnames(rects_sample) <- 1:nsamps
rects_sample <- as.data.frame(cbind(Dates,rects_sample))
rects_sample <- rects_sample[with(rects_sample,order(-Dates)),]

###
#tempx <- Kennett[which(Kennett$TShift <= sample_date_range[2] & Kennett$TShift >= sample_date_range[1] ),c(3,5)]
