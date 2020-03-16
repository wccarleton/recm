start <- sample_date_range[2]#6000
end <- sample_date_range[1]#5000

calcurve <- copyCalibrationCurve()
names(calcurve) <- c("CalendarBP","C14","Uncertainty")
p1 <- ggplot(data=calcurve) +
    geom_path(mapping=aes(y=C14,x=CalendarBP)) +
    geom_ribbon(mapping=aes(x=CalendarBP,ymin=C14 - (2.96 * Uncertainty),ymax=C14 + (2.96 * Uncertainty)),fill="skyblue",alpha=0.75) +
    #coord_flip() +
    ylim(c(5000,5475)) +
    scale_x_reverse() +
    xlim(c(start,end)) +
    theme_minimal() +
    theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0,1,0,1),"cm"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

date_df <-  as.data.frame(cbind(Dates,c14_matrix))
names(date_df) <- c("Date","Density1","Density2")
date_df_long <- gather(date_df,key="Sample",value="Density",c("Density1","Density2"))

p2 <- ggplot(data=date_df_long) +
        geom_area(mapping=aes(y=Density,x=Date,fill=Sample),alpha=0.5,position="identity") +
        scale_x_reverse() +
        xlim(c(start,end)) +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0,1,0,1),"cm"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position="none")

rects_df_long <- gather(rects_sample,key="Sample",value="Count",paste(1:1000))

p3 <- ggplot(data=rects_df_long[which(rects_df_long$Sample==16),]) +
        geom_col(mapping=aes(y=Count,x=Dates),position="identity",alpha=0.75,colour=NA) +
        scale_x_reverse() +
        xlim(c(start,end)) +
        ylim(c(0,2)) +
        labs(x="Date (Calendar BP)",
            y="RECE Member\nCount") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0,1,0,1),"cm"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

p4 <- ggplot(data=rects_df_long[which(rects_df_long$Sample==15),]) +
        geom_col(mapping=aes(y=Count,x=Dates),position="identity",alpha=0.75,colour=NA) +
        scale_x_reverse() +
        xlim(c(start,end)) +
        ylim(c(0,2)) +
        labs(x="Date (Calendar BP)",
            y="RECE Member\nCount") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0,1,0,1),"cm"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

p5 <- ggplot(data=rects_df_long) +
        geom_col(mapping=aes(y=Count,x=Dates),position="identity",alpha=0.25,colour=NA) +
        scale_x_reverse() +
        xlim(c(start,end)) +
        labs(x="Date (Calendar BP)",
            y="RECE Count") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0,1,0,1),"cm"))

###
fig <- ggarrange(p1,p2,p3,p4,p5,
         ncol=1,
         nrow=5,
         align="v")
fig

ggsave(filename="../Images/simdata_ex.png",
      device = "png",
      height = 10,
      width = 15,
      units = "cm",
      scale = 1.5,
      dpi = 2000)
