Ndates = 1000
span = 1000
start = 6000
end = 5000
#B = -0.004

prob_fun <- exp(0:(span-1) * B)

simdates <- sample(x=start:(end + 1),size=Ndates,replace=T,prob=prob_fun)

sim_c14 <- t(sapply(simdates,calBP.14C))
simc14sample <- sim_c14[sample(1:nrow(sim_c14),size=Ndates),]
c14post <- lapply(1:Ndates,function(x)calibrate(simc14sample[x,1],simc14sample[x,2],graph=F)$calib)

##
source("../R_scripts/Analyses/rcSample.R")
