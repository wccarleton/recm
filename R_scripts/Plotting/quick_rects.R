rects_sub <- rects_sample[,c(1,rects_indeces)]
rects_df_long <- gather(rects_sub,key="Sample",value="Count",paste(101:200))
ggplot(data=rects_df_long) +
    geom_col(mapping=aes(y=Count,x=Dates),position="identity",alpha=0.25,colour=NA) +
    scale_x_reverse() +
    #xlim(c(start,end)) +
    labs(x="Date (Calendar BP)",
        y="RECE Count") +
    theme_minimal() +
    theme(text = element_text(family="Times", size=12),
        plot.margin=unit(c(0,1,0,1),"cm"))
