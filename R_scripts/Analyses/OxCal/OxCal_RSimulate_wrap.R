#generate cal dates from a uniform distribution
#wrap oxcal R_Simulate()
oxcalWrapRSimulate <- function(name,caldate,uncertainty){
   oxstring <- paste('R_Simulate(',
                              '"',
                              name,
                              '",',
                              caldate,
                              ',',
                              uncertainty,
                              ');',
                              sep='')
   return(oxstring)
}

oxcalRSimulate <- function(df){
   oxcalsource <- c()
   for(j in 1:nrow(df)){
      oxstring <- oxcalWrapRSimulate(df[j,1],df[j,2],df[j,3])
      oxcalsource <- c(oxcalsource,oxstring)
   }
   return(oxcalsource)
}
