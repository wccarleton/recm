#load("../Results/mcmc_samples_spdf_ar.RData")
df <- as.data.frame(samples[2000:dim(samples)[1],grep("B|alpha0|rho|sigma",colnames(samples))])
df$index <- 1:dim(df)[1]
p1 <- ggplot(data=df,aes(y=B,x=index)) +
geom_line() +
labs(y=expression(beta)) +
#scale_x_continuous(breaks=seq(0,length(sampleindex),100000)) +
theme_minimal() +
theme(text = element_text(family="Times", size=12),
      plot.title = element_text(face="bold",hjust=0.5,size=15),
      axis.title.x = element_blank())

p2 <- ggplot(data=df,aes(y=alpha0,x=index)) +
geom_line() +
labs(y=expression(alpha[0])) +
#scale_x_continuous(breaks=seq(0,length(sampleindex),100000)) +
theme_minimal() +
theme(text = element_text(family="Times", size=12),
      plot.title = element_text(face="bold",hjust=0.5,size=15),
      axis.title.x = element_blank())

p4 <- ggplot(data=df,aes(y=rho,x=index)) +
geom_line() +
labs(y=expression(rho)) +
#scale_x_continuous(breaks=seq(0,length(sampleindex),100000)) +
theme_minimal() +
theme(text = element_text(family="Times", size=12),
      plot.title = element_text(face="bold",hjust=0.5,size=15),
      axis.title.x = element_blank())

p5 <- ggplot(data=df,aes(y=sigma,x=index)) +
geom_line() +
labs(y=expression(sigma)) +
#scale_x_continuous(breaks=seq(0,length(sampleindex),100000)) +
theme_minimal() +
theme(text = element_text(family="Times", size=12),
      plot.title = element_text(face="bold",hjust=0.5,size=15),
      axis.title.x = element_blank())

fig <- ggarrange(p1,p2,p4,p5,
                  ncol=1,
                  nrow=4,
                  align="v")
fig
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

ggsave(filename=paste("../Images/mcmcchains_spdf_exp_ar_sin.png",sep=""),
      width=10,
      height=10,
      units="cm",
      scale=2,
      device = "png",
      dpi=600)
