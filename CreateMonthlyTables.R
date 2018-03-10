library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(ggthemr)
library(grid)
housingUnits <-  readRDS("RDS/housingUnits.rds")
medinc <- readRDS("RDS/MedianIncome.RDS")
medhome <- readRDS("RDS/MedianHomePrice.RDS")
haiyear <- readRDS("RDS/haiyear.rds")
buildingpermitsfornames <- readRDS("RDS/BuildingPermits.RDS")
buildingpermits <- read.csv("Monthly_CSV/housing.csv", header=FALSE)
popdens <- readRDS("RDS/pop_dens.rds")
fares <- readRDS("RDS/Passenger_Fares.rds")
miles <- readRDS("RDS/Vehicle_Miles.rds")
miles <- miles[-49,]
inventory <- readRDS("RDS/Vehicle_Inventory.rds")
inventory <- inventory[-50,]
hours <- readRDS("RDS/Vehicle_Hours.rds")
hours <- hours[-46,]
hai <- readRDS("RDS/haiyear.rds")
unemployment <- unique(readRDS("RDS/unemployment.rds"))
rentalprice <- readRDS("RDS/RentalPrice.RDS") %>% subset(State == "CA")
acs <- readRDS("data-collection/cleaned-data/Tidy ACS Data (2011-2016)")
valuepersquarefoot <- read.csv("Monthly_CSV/ValuePerSquareFoot.csv")
valuepersquarefoot <- valuepersquarefoot[order(valuepersquarefoot$Time),]
foreclosures <- read.csv("Monthly_CSV/Foreclosures.csv")
vacancy <- readRDS("Monthly_RDS/VacancyRates.RDS")
gasconsumption <- readRDS("Monthly_RDS/GasConsumption.RDS")
countynames <- buildingpermitsfornames$County

f <- function(x) as.numeric(gsub("[\\.,\\$%]","",x))
clean.numeric <- function(x) sapply(x,f)
medhomecsv <- read.csv("Old Excel Files/201712 MedianPricesofExistingDetachedHomesHistoricalData.csv")
colnames(medhomecsv) %<>% (function(x) gsub(".", " ",x, fixed=TRUE))
medhomecsv <- medhomecsv[1:336,]

if (TRUE) {
tables <- list()
num_cols = 11
start <- proc.time()
for (i in 1:58) {
  data <- data.frame(matrix(ncol = num_cols, nrow = 336))
  countyname = countynames[i]
  colnames(data)[1] <- "Date"
  data$Date <- seq(1990,2017+11/12,1/12)
  
  colnames(data)[2] <- "Median Home Price"
  ind = which(colnames(medhomecsv) == countyname)
  if (length(ind) != 0) {
    data[,2] <- clean.numeric(medhomecsv[,ind])
  }
  
  colnames(data)[3] <- c("Vacancy Rates")
  data[1:325, 3] <- clean.numeric(vacancy[, i+1])
  
  # colnames(data)[3] <- c("Rental Price")
  # ind <- which(rentalprice$RegionName == countyname)
  # if (length(ind) != 0) {
  #   data[241:334, 3] <- c(clean.numeric(rentalprice[ind,7:100]))
  # }
  
  colnames(data)[4] <- c("Building permits")
  data[133:336, 4] <- c(clean.numeric(buildingpermits[i,-seq(13,221,13)]))
  
  colnames(data)[5] <- c("Unemployment")
  countyunemployment <- subset(unemployment, grepl(countyname,`Area Name`) & `Seasonally Adjusted (Y/N)`=="N")
  data[,5] <- countyunemployment[,10]
  
  colnames(data)[6] <- c("ValuePerSquareFoot")
  ind <- which(grepl(countyname, colnames(valuepersquarefoot)))
  if (length(ind) != 0) {
    data[76:336, 6] <- valuepersquarefoot[1:261,ind]
  }
  
  colnames(data)[7] <- c("Foreclosures")
  data[97:332, 7] <- clean.numeric(foreclosures[,i+1])
  
  
  colnames(data)[8] <- c("Gas Consumption")
  data[1:313, 8] <- clean.numeric(gasconsumption[, i+1])
  
  colnames(data)[9] <- c("Foreclosures")
  colnames(data)[10] <- c("Foreclosures")
  colnames(data)[11] <- c("Foreclosures")
  tables[[i]] <- data
}
print(proc.time() - start)
}
View(tables[1])
