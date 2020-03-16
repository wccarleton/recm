Kennett <- read.csv("../Data/Climate/kennett_Yok_Balum.csv",as.is=T)
Kennett <- Kennett[-which(apply(Kennett,1,function(x)any(is.na(x)))),]
Kennett[,4] <- scale(Kennett[,4],center=T,scale=F)
Kennett[,3] <- scale(Kennett[,3],center=T,scale=T)
Kennett$TShift <- seq(7500,7500-dim(Kennett)[1] + 1)
Ndates = 10000
start = 6000
end = 4000
B = -0.5
X_sim <- Kennett[which(Kennett$TShift <= start & Kennett$TShift > end ),c(3)]
events <- rpois(n=start-end,lambda=exp(2.5 + X_sim * B))
events_df <- data.frame(Date=start:(end+1),Count=events)
simdates <- as.vector(unlist(apply(events_df,1,function(x)rep(x[1],x[2]))))
sim_c14 <- t(sapply(simdates,calBP.14C))
simc14sample <- sim_c14[sample(1:nrow(sim_c14),size=Ndates),]
c14post <- lapply(1:Ndates,function(x)calibrate(simc14sample[x,1],simc14sample[x,2],graph=F)$calib)
