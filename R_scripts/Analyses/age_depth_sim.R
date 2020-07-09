Kennett_age <- read.csv("../Data/Climate/kennett_Yok_Balum_age.csv",as.is=T)
Kennett_age_clam <- data.frame(lab_ID = Kennett_age[,1],
                                C14_age = NA,
                                cal_age = Kennett_age[,17],
                                error = Kennett_age[,18],
                                reservoir = NA,
                                depth = Kennett_age[,2])
write.csv(Kennett_age_clam,file="../Data/Climate/Clam/Kennett_age_clam.csv",row.names=F)

Borneo_age <- read.csv("../Data/Climate/borneo2016ba03_age.csv",as.is=T)
Borneo_age_clam <- data.frame(lab_ID = 1:dim(Borneo_age)[1],
                                C14_age = NA,
                                cal_age = Borneo_age[,3],
                                error = Borneo_age[,4],
                                reservoir = NA,
                                depth = Borneo_age[,2])
write.csv(Borneo_age_clam,file="../Data/Climate/Clam/Borneo_age_clam.csv",row.names=F)

Browns_age <- read.csv("../Data/Climate/browns2017iso_age",as.is=T,skip=1,head=F)
Browns_age_clam <- data.frame(lab_ID = 1:dim(Browns_age)[1],
                                C14_age = NA,
                                cal_age = Browns_age[,14],
                                error = Browns_age[,15],
                                reservoir = NA,
                                depth = Browns_age[,1])
write.csv(Browns_age_clam,file="../Data/Climate/Clam/Browns_age_clam.csv",row.names=F)

Kennett <- read.csv("../Data/Climate/kennett_Yok_Balum.csv",as.is=T)
Kennett <- Kennett[-which(apply(Kennett,1,function(x)any(is.na(x)))),]
Kennett[,4] <- scale(Kennett[,4],center=T,scale=F)
Kennett[,3] <- scale(Kennett[,3],center=T,scale=T)
