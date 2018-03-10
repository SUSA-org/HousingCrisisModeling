library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(tseries)
library(forecast)
library(grid)
library(zoo)
hai <- readRDS("RDS/haiyear.rds")
buildingpermitsfornames <- readRDS("RDS/BuildingPermits.RDS")
countynames <- buildingpermitsfornames$County

vacrate <- readRDS("RDS/electricity.rds")
vacrate[,3:29] <- vacrate[,29:3]

View(vacrate)
years <- seq(as.Date("1990-01-01"), as.Date("2016-01-01"), by="months")

vacmonth <- data.frame(matrix(ncol = 59, nrow = 313))
vacmonth[,1] <- years
f <- function(x) as.numeric(gsub("[,\\$%]","",x))
clean.numeric <- function(x) sapply(x,f)
vacrate[,3:29] <- clean.numeric(vacrate[,3:29])
for(j in 1:58) {
  countyname = tolower(vacrate[j,1])
  ind = which(grepl(countyname, countynames, ignore.case=TRUE))
  for (i in 3:28) {
    sequence <- seq(vacrate[j, i], vacrate[j, i+1], length.out = 13)
    vacmonth[((i-3)*12+1):((i-3)*12+13),ind+1] <- sequence
  }
}
View(vacmonth)
#saveRDS(vacmonth,"Monthly_RDS/Electricity.RDS")
