library(ggplot2)
library(TSA)

# LOAD DATA
datafolder <- "S:/BigData/data/"
influenza_file <- paste(datafolder, "influenza/Influenza data 2010-2018.csv", sep="")

influenza_data <- read.csv(influenza_file, sep = ";")
influenza_data$weeknr <- strftime(influenza_data$prdate, format="%Y-%V")
influenza_data <- influenza_data[order(influenza_data$weeknr),]
influenza_data[is.na(influenza_data)] <- 0

# aggregate per week
influenza_data_weekly <- aggregate(influenza_data[c("a", "h1n1", "h3n2", "b", "c")], 
                                   by=list(weeknr=influenza_data$weeknr), FUN=sum)
influenza_data_weekly$weekdate <- strptime(paste(influenza_data_weekly$weeknr, "1"), "%Y-%W %u")

# plot the different influenza types
ggplot(data=influenza_data_weekly, aes(weekdate, y=value, color=variable)) + 
  geom_line(aes(y=a, col="a")) +
  geom_line(aes(y=b, col="b")) +
  geom_line(aes(y=c, col="c")) + 
  geom_line(aes(y=h1n1, col="h1n1")) +
  geom_line(aes(y=h3n2, col="h3n2"))

# make a periodogram (fourie transform), 1/frequency = number of weeks
p = periodogram(influenza_data_weekly$h3n2)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)
1/top2$freq
