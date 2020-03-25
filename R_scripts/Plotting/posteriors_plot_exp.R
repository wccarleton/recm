burnin <- 2000
thin <- 9
#nochrono
##positive
load("../Results/MCMC/Exp/mcmc_samples_exp_pos_nochrono.RData")
niter <- dim(samples$samples)[1]
df <- data.frame(B = samples$samples[seq(burnin,niter,thin),1])
p1 <- ggplot(data=df) +
         geom_density(mapping=aes(x=B, y=..scaled..),fill="grey",alpha=0.75,colour=NA) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
               plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.y=element_blank())


##negative
load("../Results/MCMC/Exp/mcmc_samples_exp_neg_nochrono.RData")
niter <- dim(samples$samples)[1]
df <- data.frame(B = samples$samples[seq(burnin,niter,thin),1])
p2 <- ggplot(data=df) +
         geom_density(mapping=aes(x=B, y=..scaled..),fill="grey",alpha=0.75,colour=NA) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
         plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.y=element_blank())
#rects draw
##positive
load("../Results/MCMC/Exp/mcmc_samples_exp_pos.RData")
niter <- dim(samples$samples)[1]
df <- data.frame(B = samples$samples[seq(burnin,niter,thin),1])
p3 <- ggplot(data=df) +
         geom_density(mapping=aes(x=B, y=..scaled..),fill="grey",alpha=0.75,colour=NA) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
         plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.y=element_blank())


##negative
load("../Results/MCMC/Exp/mcmc_samples_exp_neg.RData")
niter <- dim(samples$samples)[1]
df <- data.frame(B = samples$samples[seq(burnin,niter,thin),1])
p4 <- ggplot(data=df) +
         geom_density(mapping=aes(x=B, y=..scaled..),fill="grey",alpha=0.75,colour=NA) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
         plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.y=element_blank())
#hirec
##positive
load("../Results/MCMC/Exp/mcmc_samples_exp_pos_hier.RData")
niter <- dim(samples$samples)[1]
df <- data.frame(B = samples$samples[seq(burnin,niter,thin),1])
p5 <- ggplot(data=df) +
         geom_density(mapping=aes(x=B, y=..scaled..),fill="grey",alpha=0.75,colour=NA) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
         plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.y=element_blank())


##negative
load("../Results/MCMC/Exp/mcmc_samples_exp_neg_hier.RData")
niter <- dim(samples$samples)[1]
df <- data.frame(B = samples$samples[seq(burnin,niter,thin),1])
p6 <- ggplot(data=df) +
         geom_density(mapping=aes(x=B, y=..scaled..),fill="grey",alpha=0.75,colour=NA) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
         plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.y=element_blank())

###
fig <- ggarrange(p1,p3,p5,p2,p4,p6,
         ncol=3,
         nrow=2)

annotate_figure(fig,
               top=text_grob("Stage 1 Posterior Densities",family="Times"),
               left=text_grob("Density (scaled to 1)",family="Times",rot=90),
               bottom=text_grob("Value",family="Times"))

ggsave(filename="../Images/posteriors_exp.png",
      device = "png",
      height = 10,
      width = 20,
      units = "cm",
      scale = 1.5,
      dpi = 2000)
