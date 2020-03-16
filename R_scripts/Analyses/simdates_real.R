Kennett <- read.csv("../Data/Climate/kennett_Yok_Balum.csv",as.is=T)
Kennett <- Kennett[-which(apply(Kennett,1,function(x)any(is.na(x)))),]
Kennett <- rbind(Kennett,Kennett)
Kennett[,4] <- scale(Kennett[,4],center=T,scale=F)
Kennett[,3] <- scale(Kennett[,3],center=T,scale=T)
Kennett$TShift <- seq(10000,10000-dim(Kennett)[1] + 1)
Ndates = 1000
start = 5000
end = 4000
B = -0.5
X_sim <- Kennett[which(Kennett$TShift <= start & Kennett$TShift > end ),c(3)] #as.vector(arima.sim(model=list(ar=0.95),n=2000,sd=0.01))#
prob_fun <- exp(X_sim * B)
prob_fun <- prob_fun/max(prob_fun)
simdates <- data.frame(Name=1:Ndates,
                        Date=sample(x=start:(end + 1),size=Ndates,replace=T,prob=prob_fun),
                        Uncertainty=rep(20,Ndates))
#oxcal_rdates <- oxcalRSimulate(simdates)
#write.table(oxcal_rdates,file="../Data/OxCal/simdates_Kennett_neg.txt",row.names=F,quote=F,col.names=F)
simdates_hist <- hist(simdates$Date,breaks=seq(start,end))
simdates_df <- data.frame(Count=simdates_hist$counts,Date=simdates_hist$mids+0.5)
simdates_df <- simdates_df[with(simdates_df,order(-Date)),]
sim_c14 <- t(sapply(simdates[,2],calBP.14C))
c14post <- lapply(1:Ndates,function(x)calibrate(sim_c14[x,1],sim_c14[x,2],graph=F)$calib)
