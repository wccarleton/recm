start <- sample_date_range[2]#6000
end <- sample_date_range[1]#5000

calcurve <- copyCalibrationCurve()
names(calcurve) <- c("CalendarBP","C14","Uncertainty")
p1 <- ggplot(data=calcurve) +
    geom_path(mapping=aes(y=C14,x=CalendarBP)) +
    geom_ribbon(mapping=aes(x=CalendarBP,ymin=C14 - (2.96 * Uncertainty),ymax=C14 + (2.96 * Uncertainty)),fill="skyblue",alpha=0.75) +
    #coord_flip() +
    ylim(c(4250,5475)) +
    scale_x_reverse() +
    xlim(c(start,end)) +
    theme_minimal() +
    theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0.25,1,0,1),"cm"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

process_df <- data.frame(ProcessY=prob_fun,CalendarBP=seq(6000,5000 + 1))

p2 <- ggplot(data=process_df) +
        geom_path(mapping=aes(y=ProcessY,x=CalendarBP)) +
        scale_x_reverse() +
        xlim(c(start,end)) +
        labs(y = "Process") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0.1,1,0,1),"cm"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

p3 <- ggplot() +
        geom_histogram(mapping=aes(x=simdates),breaks=seq(start,end + 1)) +
        #ylim(c(4350,5375)) +
        scale_x_reverse() +
        xlim(c(start,end)) +
        labs(x="Date (Calendar BP)",
            y="Count") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0.1,1,0,1),"cm"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

rects_df_long <- gather(rects_sample,key="Sample",value="Count",paste(1:1000))

p4 <- ggplot(data=rects_df_long) +
        geom_col(mapping=aes(y=Count,x=Dates),position="identity",alpha=0.05,colour=NA) +
        scale_x_reverse() +
        xlim(c(start,end)) +
        labs(x="Date (Calendar BP)",
            y="RECE Count") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0.1,1,0,1),"cm"),)

###
fig <- ggarrange(p1,p2,p3,p4,
         ncol=1,
         nrow=4,
         align="v")
fig

ggsave(filename="../Images/simdata_exp_pos.png",
      device = "png",
      height = 10,
      width = 20,
      units = "cm",
      scale = 1,
      dpi = 600)
