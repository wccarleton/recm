rects <- function(dates,
                  nsamples,
                  start_date=NULL,
                  end_date=NULL,
                  resolution){
    if(is.null(start_date) | is.null(end_date)){
        date_range <- range(unlist(lapply(dates,function(x)range(x[,1]))))
        start_date <- date_range[1]
        end_date <- date_range[2]
    }
   drange <- seq(start_date,end_date,resolution)
   print(drange)
   count_matrix <- matrix(NA,
                           nrow = length(drange),
                           ncol = nsamples)
   for(j in 1:nsamples){
      sample_df <- as.data.frame(table(calSample(dates)))
      matched_dates <- which(sample_df[,1] %in% drange)
      count_matrix[which(drange %in% sample_df[,1]),j] <- sample_df[matched_dates,2]
   }
   return(list(dates = drange,
               counts = count_matrix))
}
