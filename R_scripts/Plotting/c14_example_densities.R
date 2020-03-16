year <- 6000
c14date <- calBP.14C(year)
caldate <- calibrate(c14date[1],c14date[2],graph=F)$calib
dates <- seq(6200,4000)
events <- as.numeric(dates == year)
caldate <- as.data.frame(caldate)
names(caldate) <- c("year","density")

dframe <- data.frame(year=dates,events=events,c14=dnorm(dates,mean=c14date[1],sd=c14date[2]))
dframe <- merge(dframe,caldate,by=1,all=T)
dframe[is.na(dframe)] <- 0

ggplot(data=dframe) +
geom_vline(xintercept=6000,alpha=0.5) +
geom_area(aes(y=c14,x=year),fill="steelblue",alpha=0.75) +
geom_area(aes(y=density,x=year),fill="firebrick",alpha=0.75) +
scale_x_reverse() +
xlim(c(6200,5000)) +
ylim(c(0,0.03)) +
labs(x="Year BP",y="Density") +
theme_minimal() +
theme(text = element_text(family="Times", size=12))

ggsave(filename="../Images/example_dates.png",
      device = "png",
      height = 10,
      width = 20,
      units = "cm",
      scale = 1.5,
      dpi = 2000)
