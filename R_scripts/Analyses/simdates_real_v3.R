Kennett <- read.csv("../Data/Climate/kennett_Yok_Balum.csv",as.is=T)
Kennett <- Kennett[-which(apply(Kennett,1,function(x)any(is.na(x)))),]
Kennett[,4] <- scale(Kennett[,4],center=T,scale=F)
Kennett[,3] <- scale(Kennett[,3],center=T,scale=T)
Kennett$TShift <- seq(7500,7500-dim(Kennett)[1] + 1)
Ndates = 1000
start = 6000
end = 5000
B = -1
X_sim <- Kennett[which(Kennett$TShift <= start & Kennett$TShift >= end ),c(3,5)]

prob_fun <- exp(5 + X_sim[,1] * B)

simdates <- sample(x=start:(end + 1),size=Ndates,replace=T,prob=prob_fun)

sim_c14 <- t(sapply(simdates,calBP.14C))
simc14sample <- sim_c14[sample(1:nrow(sim_c14),size=Ndates),]
c14post <- lapply(1:Ndates,function(x)calibrate(simc14sample[x,1],simc14sample[x,2],graph=F)$calib)

##
source("../R_scripts/Analyses/rcSample.R")
