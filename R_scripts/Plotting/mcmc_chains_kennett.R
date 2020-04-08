#positive
##no chrono
load("../Results/MCMC/Kennett/mcmc_samples_kennett_pos_nochrono.RData")
burnin <- 5000
niter <- dim(samples$samples)[1]
noms <- colnames(samples$samples)
cols <- grep("B",noms)
ncols <- length(cols)
chains <- samples$samples[burnin:niter,cols]
nsamps <- dim(chains)[1]
samps <- 1:nsamps
namen <- c("beta","beta[0]")
p <- list()
for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j]] <- ggplot() +
                geom_path(data=DF,
                            mapping=aes(y=B,x=Iteration))+
                labs(y=parse(text=namen[j]),keep.source=F) +
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

##RECE member
load("../Results/MCMC/Kennett/mcmc_samples_kennett_pos.RData")
burnin <- 5000
niter <- dim(samples$samples)[1]
noms <- colnames(samples$samples)
cols <- grep("B",noms)
ncols <- length(cols)
chains <- samples$samples[burnin:niter,cols]
nsamps <- dim(chains)[1]
samps <- 1:nsamps
namen <- c("beta","beta[0]")
plen <- length(p)
for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j + plen]] <- ggplot() +
                geom_path(data=DF,
                            mapping=aes(y=B,x=Iteration))+
                labs(y=parse(text=namen[j]),keep.source=F) +
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

##HiREC
#load("../Results/MCMC/Kennett/mcmc_samples_kennett_pos_hier.RData")
samples <- read.csv("../Results/MCMC/Kennett/mcmc_samples_kennett_pos_hier.csv",as.is=T)
burnin <- 5000
niter <- dim(samples)[1]
noms <- colnames(samples)
cols <- grep("B",noms)[c(1,2)]
ncols <- length(cols)
chains <- samples[burnin:niter,cols]
nsamps <- dim(chains)[1]
samps <- 1:nsamps
namen <- c("beta","beta[0]","sigma[beta]","sigma[beta[0]]")

plen <- length(p)

for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j + plen]] <- ggplot() +
                geom_path(data=DF,
                            mapping=aes(y=B,x=Iteration))+
                labs(y=parse(text=namen[j]),keep.source=F) +
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

#Negative
##no chrono
load("../Results/MCMC/Kennett/mcmc_samples_kennett_neg_nochrono.RData")
burnin <- 5000
niter <- dim(samples$samples)[1]
noms <- colnames(samples$samples)
cols <- grep("B",noms)
ncols <- length(cols)
chains <- samples$samples[burnin:niter,cols]
nsamps <- dim(chains)[1]
samps <- 1:nsamps
namen <- c("beta","beta[0]")
plen <- length(p)
for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j + plen]] <- ggplot() +
                geom_path(data=DF,
                            mapping=aes(y=B,x=Iteration))+
                labs(y=parse(text=namen[j]),keep.source=F) +
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

##RECE member
load("../Results/MCMC/Kennett/mcmc_samples_kennett_neg.RData")
burnin <- 5000
niter <- dim(samples$samples)[1]
noms <- colnames(samples$samples)
cols <- grep("B",noms)
ncols <- length(cols)
chains <- samples$samples[burnin:niter,cols]
nsamps <- dim(chains)[1]
samps <- 1:nsamps
namen <- c("beta","beta[0]")
plen <- length(p)
for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j + plen]] <- ggplot() +
                geom_path(data=DF,
                            mapping=aes(y=B,x=Iteration))+
                labs(y=parse(text=namen[j]),keep.source=F) +
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

##HiREC
#load("../Results/MCMC/Kennett/mcmc_samples_kennett_neg_hier.RData")
samples <- read.csv("../Results/MCMC/Kennett/mcmc_samples_kennett_neg_hier.csv",as.is=T)
burnin <- 5000
niter <- dim(samples)[1]
noms <- colnames(samples)
cols <- grep("B",noms)[c(1,2)]
ncols <- length(cols)
chains <- samples[burnin:niter,cols]
nsamps <- dim(chains)[1]
samps <- 1:nsamps
namen <- c("beta","beta[0]","sigma[beta]","sigma[beta[0]]")

plen <- length(p)

for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j + plen]] <- ggplot() +
                geom_path(data=DF,
                            mapping=aes(y=B,x=Iteration))+
                labs(y=parse(text=namen[j]),keep.source=F) +
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

##Arrange
fig <- ggarrange(plotlist=p,
                  ncol=2,
                  nrow=6,
                  align="v")

annotate_figure(fig,
               left = text_grob("Value",
                                 family="Times",
                                 rot=90),
               bottom = text_grob("Iteration",
                                 family="Times"),
               top = text_grob("MCMC Chains",
                                 family="Times",
                                 face="bold"),
               fig.lab.pos = "top")

##Save
ggsave(filename=paste("../Images/mcmcchains_kennett.png",sep=""),
      width=20,
      height=20,
      units="cm",
      scale=1,
      device = "png",
      dpi=600)
