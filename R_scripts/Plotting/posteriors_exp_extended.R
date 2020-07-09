#positive
##replication 1
load("../Results/MCMC/Extended/mcmc_samples_exp_pos_repn1_top.RData")
noms <- colnames(mcmc_top)[c(1,3)]
chains <- mcmc_top[,c(1,3)]
namen <- c("B","sigma[B]")
ncols <- length(noms)
samps <- 1:dim(mcmc_top)[1]
p <- list()
for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j]] <- ggplot() +
                geom_density(data=DF,
                            mapping=aes(x=B),
                            fill="grey",
                            colour=NA,
                            alpha=0.8)+
                labs(y=parse(text=namen[j]),keep.source=F) +
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

plen <- length(p)

##replication 2
load("../Results/MCMC/Extended/mcmc_samples_exp_pos_repn2_top.RData")
noms <- colnames(mcmc_top)[c(1,3)]
chains <- mcmc_top[,c(1,3)]
namen <- c("B","sigma[B]")
ncols <- length(noms)
samps <- 1:dim(mcmc_top)[1]
for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j + plen]] <- ggplot() +
                geom_density(data=DF,
                            mapping=aes(x=B),
                            fill="grey",
                            colour=NA,
                            alpha=0.8)+
                labs(y=parse(text=namen[j]),keep.source=F) +
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

plen <- length(p)

##replication 3
load("../Results/MCMC/Extended/mcmc_samples_exp_pos_repn3_top.RData")
noms <- colnames(mcmc_top)[c(1,3)]
chains <- mcmc_top[,c(1,3)]
namen <- c("B","sigma[B]")
ncols <- length(noms)
samps <- 1:dim(mcmc_top)[1]
for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j + plen]] <- ggplot() +
                geom_density(data=DF,
                            mapping=aes(x=B),
                            fill="grey",
                            colour=NA,
                            alpha=0.8)+
                labs(y=parse(text=namen[j]),keep.source=F) +
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

##Arrange
fig <- ggarrange(plotlist=p,
                  ncol=2,
                  nrow=3,
                  align="hv")

annotate_figure(fig,
               left = text_grob("Density",
                                 family="Times",
                                 rot=90),
               bottom = text_grob("Value",
                                 family="Times"),
               top = text_grob("Posterior Density Estimates",
                                 family="Times",
                                 face="bold"),
               fig.lab.pos = "top")

##Save
ggsave(filename=paste("../Images/posteriors_exp_extended.png",sep=""),
      width=20,
      height=20,
      units="cm",
      scale=1,
      device = "png",
      dpi=300)
