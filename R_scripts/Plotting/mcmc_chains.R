burnin <- 1000
niter <- dim(samples)[1]
noms <- colnames(samples)
cols <- grep("b\\[",noms)
ncols <- length(cols)
chains <- samples[burnin:niter,cols]
nsamps <- dim(chains)[1]
samps <- 1:nsamps
p <- list()
for(j in 1:ncols){
    DF <- data.frame(B=chains[,j],Iteration=samps)
    p[[j]] <- ggplot() +
                geom_path(data=DF,
                            mapping=aes(y=B,x=Iteration))+
                theme_minimal() +
                theme(text = element_text(family="Times", size=12),
                        plot.title = element_text(face="bold",hjust=0.5,size=15),
                        axis.title.x = element_blank())
}

fig <- ggarrange(plotlist=p,
                  ncol=1,
                  nrow=ncols,
                  align="v")
annotate_figure(fig,
               left = text_grob("Value",
                                 family="Times",
                                 rot=90),
               bottom = text_grob("Sample",
                                 family="Times"),
               top = text_grob("MCMC Chains",
                                 family="Times",
                                 face="bold"),
               fig.lab.pos = "top")

ggsave(filename=paste("../Images/mcmcchains.png",sep=""),
      width=10,
      height=50,
      units="cm",
      scale=2,
      device = "png",
      dpi=600)
