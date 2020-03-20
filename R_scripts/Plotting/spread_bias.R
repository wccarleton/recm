#Load data
switch <- 1
if(switch == 0){
   source("../R_scripts/Analyses/simdates_exp_v2.R")
   simdates_exp_nc <- simdates
   rects_sample_exp <- rects_sample
   sample_date_range_exp <- sample_date_range

   source("../R_scripts/Analyses/simdates_real_v3.R")
   simdates_ken_nc <- simdates
   rects_sample_ken <- rects_sample
   sample_date_range_ken <- sample_date_range

   counts_exp_nc <- hist(simdates_exp_nc,breaks=rev(seq(end,start)))
   process_exp_nc <- data.frame(y=exp(-2.712684 + 0.004*1000:1),
                        times=1:1000)

   counts_ken_nc <- hist(simdates_ken_nc,breaks=rev(seq(end,start)))
   process_ken_nc <- data.frame(y=exp(-1.292848 + rev(X_sim[,1])),
                              X=rev(X_sim[,1]))

   load("../Results/MCMC/Exp/mcmc_samples_exp_pos.RData")
   samples_exp <- samples

   load("../Results/MCMC/Kennett/mcmc_samples_kennett_pos.RData")
   samples_kennett <- samples
}

#monotonic dfs
df_exp_nc <- data.frame(y=process_exp_nc$y,
                        counts=counts_exp_nc$counts,
                        times=rev(start:(end + 1)))


b_unc <- mean(samples_exp$samples[2000:dim(samples_exp$samples)[1],1])
b0_unc <- mean(samples_exp$samples[2000:dim(samples_exp$samples)[1],2])

shifted_range <- -(sample_date_range_exp - start)
X <- shifted_range[2]:shifted_range[1]

y_unc <- exp(b0_unc + b_unc*X)

df_exp <- data.frame(times=sample_date_range_exp[1]:sample_date_range_exp[2],
                     counts=rev(rects_sample_exp[,runif(n=1,2,1000)]),
                     y_unc=rev(y_unc))

#kennett dfs
df_ken_nc <- data.frame(y=process_ken_nc$y,
                        counts=counts_ken_nc$counts,
                        X=rev(X_sim[,1]))

b_unc <- mean(samples_kennett$samples[2000:dim(samples_kennett$samples)[1],1])
b0_unc <- mean(samples_kennett$samples[2000:dim(samples_kennett$samples)[1],2])

shifted_range <- -(sample_date_range_ken - start)
Xt <- shifted_range[2]:shifted_range[1]
X <- as.vector(Kennett[which(Kennett$TShift <= sample_date_range[2] & Kennett$TShift >= sample_date_range[1] ),3])

X_shifted_range <- range(X)

y_unc <- exp(b0_unc + b_unc*X)

df_ken <- data.frame(X=X,
                  counts=rects_sample_ken[,runif(n=1,2,1000)],
                  y_unc=y_unc)


#GG Objs
p1 <- ggplot(data=df_exp_nc) +
         geom_col(mapping=aes(y=counts,x=times),alpha=0.25) +
         geom_path(mapping=aes(y=y,x=times)) +
         ylim(c(0,8)) +
         xlim(c(sample_date_range_exp[2],sample_date_range_exp[1])) +
         labs(y="Count") +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
                 plot.margin=unit(c(0,0,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())

p2 <- ggplot(data=df_exp) +
         geom_col(mapping=aes(y=counts,x=times),alpha=0.25) +
         geom_path(mapping=aes(y=y_unc,x=times)) +
         ylim(c(0,8)) +
         xlim(c(sample_date_range_exp[2],sample_date_range_exp[1])) +
         labs(x="Time (Cal. BP)",y="Count") +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
                 plot.margin=unit(c(0,0,0,0),"cm"))

p3 <- ggplot(data=df_ken_nc) +
         geom_point(mapping=aes(y=counts,x=X),alpha=0.25,colour="darkgrey") +
         geom_path(df_ken_nc[order(df_ken_nc$X),],mapping=aes(y=y,x=X)) +
         ylim(c(0,40)) +
         xlim(c(X_shifted_range)) +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
                 plot.margin=unit(c(0,0,0,0),"cm"),
                 axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.title.y=element_blank())


p4 <- ggplot(data=df_ken) +
         geom_point(mapping=aes(y=counts,x=X),alpha=0.25,colour="darkgrey") +
         geom_path(data=df_ken[order(df_ken$X),],mapping=aes(y=y_unc,x=X)) +
         ylim(c(0,40)) +
         xlim(c(X_shifted_range)) +
         labs(x="X") +
         theme_minimal() +
         theme(text = element_text(family="Times", size=12),
                 plot.margin=unit(c(0,0,0,0),"cm"),
                 axis.title.y=element_blank())

###
fig <- ggarrange(p1,p3,p2,p4,
         ncol=2,
         nrow=2,
         align="v")
fig

ggsave(filename="../Images/temporal_spread_bias.png",
      device = "png",
      height = 10,
      width = 20,
      units = "cm",
      scale = 1.5,
      dpi = 2000)
