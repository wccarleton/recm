Ndates = 1000
B = 0.004
start = -3000
prob_fun <- exp(0:(Ndates-1) * B)
prob_fun <- prob_fun/max(prob_fun)
simdates <- data.frame(Name=1:Ndates,
                        Date=start + sample(x=0:(Ndates-1),size=Ndates,replace=T,prob=prob_fun),
                        Uncertainty=rep(20,Ndates))
oxcal_rdates <- oxcalRSimulate(simdates)
write.table(oxcal_rdates,file="../Data/OxCal/simdates_exp_pos.txt",row.names=F,quote=F,col.names=F)
