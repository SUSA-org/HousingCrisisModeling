library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(tseries)
library(forecast)
library(grid)
library(zoo)
buildingpermitsfornames <- readRDS("RDS/BuildingPermits.RDS")
countynames <- buildingpermitsfornames$County

medinc <- readRDS("RDS/MedianIncome.RDS")
vacrate <- medinc[,7]
vacrate <- rev(vacrate)

years <- seq(as.Date("1996-01-01"), as.Date("2015-01-01"), by="months")

vacmonth <- data.frame(matrix(ncol = 59, nrow = 229))
vacmonth[,1] <- years
for(county_ind in 1:58) {
  countyname = countynames[county_ind]
  inds = which(grepl(countyname, medinc$County,ignore.case=TRUE))
  pop <- as.data.frame(vacrate[inds,])
  for (i in 1:19) {
    sequence <- seq(pop[i,], pop[i+1,], length.out = 13)
    vacmonth[((i-1)*12+1):((i-1)*12+13),county_ind+1] <- sequence
  }
}

bigvacmonth <- rbind(vacmonth[rep(1, 72), ], vacmonth, vacmonth[rep(229, 35),])
colnames(bigvacmonth) <- c("Date", countynames)
View(bigvacmonth)
saveRDS(bigvacmonth,"Monthly_RDS/Population.RDS")
