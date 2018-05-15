
cpr_to_birthday <- function(cpr){
  if (!is.na(suppressWarnings(as.numeric(cpr))) && nchar(cpr) == 10) {
    day = substr(cpr, 1, 2)
    month = substr(cpr, 3, 4)
    if (as.integer(day) > 31 | as.integer(month) > 12) {
      return("-1")
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
      return(paste(year+centry, month, day, sep="-"))
    }
  } else {
    return("-1")
  }
}

hours_in_year <- 8765.812536

# =============== LOAD DATA INFLUENZA DATA ========================
load_influenza_data <- function() {
  datafolder <- "S:/BigData/data/"
  influenza_file <- paste(datafolder, "influenza/Influenza data 2010-2018.csv", sep="")
  influenza_data <- read.csv(influenza_file, sep = ";", stringsAsFactors = FALSE)
  
  # TODO: figure out how to do this without unlist...
  influenza_data$birthday <- unlist(lapply(influenza_data$cprnr, cpr_to_birthday))
  
  valid_cpr = sapply(influenza_data$birthday, function(x) {x != "-1"})
  influenza_data <- influenza_data[valid_cpr,]
  
  influenza_data$prdate <- as.Date(influenza_data$prdate, format="%Y-%m-%d")
  influenza_data$birthday <- as.Date(influenza_data$birthday, format="%Y-%m-%d")
  
  influenza_data$alder <- as.numeric(round(difftime(influenza_data$prdate, influenza_data$birthday, units="hours")/hours_in_year))
  
  influenza_data$weeknr <- strftime(influenza_data$prdate, format="%Y-%V")
  influenza_data <- influenza_data[order(influenza_data$weeknr),]
  influenza_data[is.na(influenza_data)] <- 0
  
  # aggregate per week
  influenza_data_weekly <- aggregate(influenza_data[c("a", "h1n1", "h3n2", "b", "c")], 
                                     by=list(weeknr=influenza_data$weeknr), FUN=sum)
  influenza_data_weekly$weekdate <- strptime(paste(influenza_data_weekly$weeknr, "1"), "%Y-%W %u")

  list(person_data = influenza_data, weekly_data = influenza_data_weekly)
}



# =============== LOAD DATA PNEUMOKOK DATA ========================
load_pneumokok_data <- function() {
  datafolder <- "S:/BigData/data/"
  pneumokok_file <- paste(datafolder, "Pneumococcus/pneumokokker_kun_data.csv", sep="")
  pneumokok_data <- read.csv(pneumokok_file, sep=";")
  
  pneumokok_data$prdate <- as.Date(pneumokok_data$pr_vedato, format="%d-%b-%y") 
  
  pneumokok_data$weeknr <- strftime(pneumokok_data$prdate, format="%Y-%V")
  pneumokok_data <- pneumokok_data[order(pneumokok_data$weeknr),]
  pneumokok_data[is.na(pneumokok_data)] <- 0
  
  # aggregate per week
  pneumokok_data_weekly <- aggregate(pneumokok_data[c("weeknr")], 
                                     by=list(weeknr=pneumokok_data$weeknr), FUN=length)
  colnames(pneumokok_data_weekly) <- c("weeknr", "count")
  pneumokok_data_weekly$weekdate <- strptime(paste(pneumokok_data_weekly$weeknr, "1"), "%Y-%W %u")

  list(person_data=pneumokok_data, weekly_data=pneumokok_data_weekly)
  
}

# =============== LOAD DATA MYCOPLASMA DATA ========================
load_mycoplasma_data <- function() {
  datafolder <- "S:/BigData/data/"
  myco_files <- list.files(paste(datafolder, "mycoplasma/", sep=""), full.names = TRUE)
  myco_data <- lapply(myco_files, read.csv)
  myco_data <- lapply(myco_data, function(x) {x[ , order(names(x))]})
  
  myco_data_comb <- do.call("rbind", myco_data)
  
  myco_data_comb$weeknr <- paste(myco_data_comb$aar, myco_data_comb$uge, sep="-")
  myco_data_comb <- myco_data_comb[order(myco_data_comb$aar, myco_data_comb$uge),]
  
  
  # aggregate per week
  myco_data_weekly <- aggregate(mycopl ~ weeknr, myco_data_comb, FUN=sum)
  myco_data_weekly$weekdate <- strptime(paste(myco_data_weekly$weeknr, "1"), "%Y-%W %u")
  myco_data_weekly <- myco_data_weekly[order(myco_data_weekly$weekdate), ]

  list(person_data=myco_data_comb, weekly_data=myco_data_weekly)  
}
