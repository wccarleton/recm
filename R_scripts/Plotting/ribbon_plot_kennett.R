burnin <- 2000

#nochrono
##positive
load("../Results/MCMC/Kennett/mcmc_samples_kennett_pos_nochrono.RData")
niter <- dim(samples$samples2)[1]
preds <- samples$samples2[burnin:niter,]
preds <- t(apply(preds,2,quantile,prob=c(0.05,0.95)))
colnames(preds) <- c("lower","upper")
load("../Data/SimData/kennett_pos_nochrono.RData")
df <- data.frame(times=rev(YDF[,1]),Y=YDF[,4],Ymin=preds[,1],Ymax=preds[,2])
p1 <- ggplot(data=df) +
         geom_ribbon(mapping=aes(x=times, ymin=Ymin, ymax=Ymax),fill="skyblue",alpha=0.9,colour=NA) +
         geom_point(mapping=aes(x=times,y=Y),shape=15,fill="black",size=0.25,alpha=0.5) +
         #scale_x_reverse() +
         xlim(c(6250,4800)) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
               plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.title.y=element_blank())


##negative
load("../Results/MCMC/Kennett/mcmc_samples_kennett_neg_nochrono.RData")
niter <- dim(samples$samples2)[1]
preds <- samples$samples2[burnin:niter,]
preds <- t(apply(preds,2,quantile,prob=c(0.05,0.95)))
colnames(preds) <- c("lower","upper")
load("../Data/SimData/kennett_neg_nochrono.RData")
df <- data.frame(times=rev(YDF[,1]),Y=YDF[,4],Ymin=preds[,1],Ymax=preds[,2])
p2 <- ggplot(data=df) +
         geom_ribbon(mapping=aes(x=times, ymin=Ymin, ymax=Ymax),fill="skyblue",alpha=0.9,colour=NA) +
         geom_point(mapping=aes(x=times,y=Y),shape=15,fill="black",size=0.25,alpha=0.5) +
         #scale_x_reverse() +
         xlim(c(6250,4800)) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
               plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.title.y=element_blank())
#rects draw
##positive
load("../Results/MCMC/Kennett/mcmc_samples_kennett_pos.RData")
niter <- dim(samples$samples2)[1]
preds <- samples$samples2[burnin:niter,]
preds <- t(apply(preds,2,quantile,prob=c(0.05,0.95)))
colnames(preds) <- c("lower","upper")
load("../Data/SimData/kennett_pos.RData")
df <- data.frame(times=rev(YDF[,1]),Y=YDF[,3],Ymin=preds[,1],Ymax=preds[,2])
p3 <- ggplot(data=df) +
         geom_ribbon(mapping=aes(x=times, ymin=Ymin, ymax=Ymax),fill="skyblue",alpha=0.9,colour=NA) +
         geom_point(mapping=aes(x=times,y=Y),shape=15,fill="black",size=0.25,alpha=0.5) +
         #scale_x_reverse() +
         xlim(c(6250,4800)) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
               plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.title.y=element_blank())


##negative
load("../Results/MCMC/Kennett/mcmc_samples_kennett_neg.RData")
niter <- dim(samples$samples2)[1]
preds <- samples$samples2[burnin:niter,]
preds <- t(apply(preds,2,quantile,prob=c(0.05,0.95)))
colnames(preds) <- c("lower","upper")
load("../Data/SimData/kennett_neg.RData")
df <- data.frame(times=rev(YDF[,1]),Y=YDF[,3],Ymin=preds[,1],Ymax=preds[,2])
p4 <- ggplot(data=df) +
         geom_ribbon(mapping=aes(x=times, ymin=Ymin, ymax=Ymax),fill="skyblue",alpha=0.9,colour=NA) +
         geom_point(mapping=aes(x=times,y=Y),shape=15,fill="black",size=0.25,alpha=0.5) +
         #scale_x_reverse() +
         xlim(c(6250,4800)) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
               plot.margin=unit(c(0,1,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank())

###
fig <- ggarrange(p1,p2,p3,p4,
         ncol=1,
         nrow=4,
         align="v")

annotate_figure(fig,
               top=text_grob("Stage 2 Posterior Predictive Interval",family="Times"),
               left=text_grob("Count",family="Times",rot=90),
               bottom=text_grob("Time (Cal. BP)",family="Times"))

ggsave(filename="../Images/ribbon_kennett.png",
      device = "png",
      height = 10,
      width = 20,
      units = "cm",
      scale = 1.5,
      dpi = 2000)
