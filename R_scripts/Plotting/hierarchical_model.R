domain <- seq(-5,5,0.01)
B <- dnorm(domain,mean=0,sd=1)
B1 <- dnorm(domain,mean=-2,sd=1)
B2 <- dnorm(domain,mean=-1,sd=1)
B3 <- dnorm(domain,mean=1,sd=1)
B4 <- dnorm(domain,mean=2,sd=1)

dframe <- data.frame(ParamVal=domain,B=B,B2=B2,B3=B3,B4=B4)

p1 <- ggplot(data=dframe) +
        geom_area(aes(y=B,x=ParamVal),fill="steelblue",alpha=0.75) +
        labs(x="Value",y="Density") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())

p2 <- ggplot(data=dframe) +
        geom_area(aes(y=B1,x=ParamVal),fill="grey",alpha=0.75) +
        geom_area(aes(y=B2,x=ParamVal),fill="grey",alpha=0.75) +
        geom_area(aes(y=B3,x=ParamVal),fill="grey",alpha=0.75) +
        geom_area(aes(y=B4,x=ParamVal),fill="grey",alpha=0.75) +
        labs(x="Value",y="Density") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12))

fig <- ggarrange(p1,p2,
         ncol=1,
         nrow=2,
         align="v")
fig

ggsave(filename="../Images/hiermodel.png",
      device = "png",
      height = 10,
      width = 20,
      units = "cm",
      scale = 1.5,
      dpi = 2000)
