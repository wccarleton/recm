Borneo <- read.csv("../Data/Climate/Borneo.csv",as.is=T)
Borneo$d18OScaled <- scale(Borneo[,3],center=T,scale=T)
Borneo_sampler <- approxfun(x=Borneo$age_calBP,y=Borneo$d18OScaled)
X_sim <- Borneo_sampler(4001:5000)

B=1

prob_fun <- exp(5 + X_sim * B)

Ndates = 1000
simdates <- sample(x=4001:5000,size=Ndates,replace=T,prob=prob_fun)
simcounts <- as.data.frame(table(simdates))


sim_c14 <- t(sapply(simdates,calBP.14C))
simc14sample <- sim_c14[sample(1:nrow(sim_c14),size=Ndates),]
c14post <- lapply(1:Ndates,function(x)calibrate(simc14sample[x,1],simc14sample[x,2],graph=F)$calib)

##save
#YDF <- data.frame(Dates=start:(end+1),X=X_sim,Y=rev(hist(simdates,breaks=seq(start,end))$counts))
#save(YDF,file="../Data/SimData/Borneo_neg_nochrono.RData")
##
source("../R_scripts/Analyses/rcSample.R")
