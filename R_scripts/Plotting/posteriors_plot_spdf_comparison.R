burnin <- 5000
thin <- 9
#nochrono
##positive
#load("../Results/MCMC/Exp/mcmc_samples_exp_pos_nochrono.RData")
#niter <- dim(samples$samples)[1]
#df <- data.frame(B = samples$samples[seq(burnin,niter,thin),1])
#p1 <- ggplot(data=df) +
#         geom_density(mapping=aes(x=B, y=..scaled..),fill="grey",alpha=0.75,colour=NA) +
#         theme_minimal() +
#         theme(text = element_text(family="Times", size=12),
#               plot.margin=unit(c(0,1,0,0),"cm"),
#                 axis.title.x=element_blank(),
#                 axis.ticks.y=element_blank(),
#                 axis.text.y=element_blank(),
#                 axis.title.y=element_blank())

#rec
##positive
load("../Results/MCMC/Exp/mcmc_samples_exp_pos_hier.RData")
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

#glm of SPDF
oxcal_spd <- read.csv("../Data/OxCal/sum_exp_pos.csv",as.is=T)
##shift the year datum and scale in accordance with the SPDs resolution (5 years)
oxcal_spd$Years <- 1:dim(oxcal_spd)[1] * 5
glm_spdf <- glm(Density~Years,data=oxcal_spd)
glm_coefs <- coefficients(summary(glm_spdf))
xdomain <- seq(glm_coefs[2,1] - (3 * glm_coefs[2, 2]), glm_coefs[2,1] + (3 * glm_coefs[2, 2]), 1e-6)
glm_coef_dens <- data.frame(Density=rescale(dnorm(x=xdomain, mean=glm_coefs[2,1], sd=glm_coefs[2, 2])),
                            Value=xdomain)

p3 <- ggplot() +
         geom_area(data=glm_coef_dens,mapping=aes(y=Density,x=Value),fill="grey",alpha=0.75,colour=NA) +
         geom_density(data=df,mapping=aes(x=B, y=..scaled..),fill="darkgrey",alpha=0.75,colour=NA) +
         geom_vline(xintercept=0.004,colour="darkgrey",alpha=0.75,lty=2,size=1) +
         labs(x="Value",y="Density (scaled to 1)") +
         xlim(0.001,0.0045) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
                 axis.ticks.y=element_blank(),
                 axis.text.y=element_blank())
p3
###
#fig <- ggarrange(p2,p3,
#         ncol=2,
#         nrow=1)

#annotate_figure(fig,
#               top=text_grob("SPDF Comparison",family="Times"),
#               left=text_grob("Density (scaled to 1)",family="Times",rot=90),
#               bottom=text_grob("Value",family="Times"))

ggsave(filename="../Images/spdf_comparison.png",
      device = "png",
      height = 5,
      width = 20,
      units = "cm",
      scale = 1,
      dpi = 2000)
