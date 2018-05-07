library(ggplot2)
library(TSA)


cpr_to_birthday <- function(cpr){
  if (!is.na(as.numeric(cpr)) && nchar(cpr) == 10) {
    day = substr(cpr, 1, 2)
    month = substr(cpr, 3, 4)
    if (as.integer(day) > 31 | as.integer(month) > 12) {
       NULL
    } else {
      year = as.integer(substr(cpr, 5, 6))
      centry_id = as.integer(substr(cpr, 7, 7))
      if (centry_id <= 3) {
        centry = 1900
      } else if (centry_id == 4) {
        if (year <= 36) {
          centry = 2000
        } else {
          centry = 1900
        }
      } else if (centry_id >= 5 && centry_id <=8) {
        if (year <= 57) {
          centry = 2000
        } else {
          centry = 1800
        }
      } else if (centry_id == 9) {
        if (year <= 36) {
          centry = 2000
        } else {
          centry = 1900
        }
      }
      paste(year+centry, month, day, sep="-")
    }
  } else {
    NULL
  }
}

hours_in_year <- 8765.812536

# LOAD DATA
datafolder <- "S:/BigData/data/"
influenza_file <- paste(datafolder, "influenza/Influenza data 2010-2018.csv", sep="")
influenza_data <- read.csv(influenza_file, sep = ";")
influenza_data$birthday <- sapply(as.vector(influenza_data[,"cprnr"]), cpr_to_birthday)
valid_cpr = sapply(influenza_data$birthday, function(x) {!is.null(x)})
influenza_data <- influenza_data[valid_cpr,]

influenza_data$prdate <- as.Date(influenza_data$prdate, format="%Y-%m-%d")
influenza_data$birthday <- as.Date(influenza_data$birthday, format="%Y-%m-%d")

influenza_data$age_at_test <- round(difftime(influenza_data$prdate, influenza_data$birthday, units="hours")/hours_in_year)

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
