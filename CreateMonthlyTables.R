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
popdens <- readRDS("Monthly_RDS/PopulationDensity.rds")
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
electricity <- readRDS("Monthly_RDS/Electricity.RDS")
income <- readRDS("Monthly_RDS/MedianIncomeByMonth.rds")
popdens <- readRDS("Monthly_RDS/PopulationDensity.rds")
crime <- readRDS("Monthly_RDS/CrimeCountByMonth.rds")
crime[1:433,] <- crime[433:1,]
fares <- readRDS("Monthly_RDS/PassengerFareByMonth.rds")
colnames(fares)[50] <- "Yuba"
hours <- readRDS("Monthly_RDS/VehicleHoursByMonth.rds")
colnames(hours)[50] <- "Yuba"
miles <- readRDS("Monthly_RDS/VehicleMilesByMonth.rds")
colnames(miles)[50] <- "Yuba"
popdens[1:313,] <- popdens[313:1,]
countynames <- buildingpermitsfornames$County

f <- function(x) as.numeric(gsub("[,\\$%]","",x))
clean.numeric <- function(x) sapply(x,f)
medhomecsv <- read.csv("Old Excel Files/201712 MedianPricesofExistingDetachedHomesHistoricalData.csv")
colnames(medhomecsv) %<>% (function(x) gsub(".", " ",x, fixed=TRUE))
medhomecsv <- medhomecsv[1:336,]

if (TRUE) {
tables <- list()
num_cols = 15
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
  data[,5] <- clean.numeric(countyunemployment[,11])
  
  colnames(data)[6] <- c("ValuePerSquareFoot")
  ind <- which(grepl(countyname, colnames(valuepersquarefoot)))
  if (length(ind) != 0) {
    data[76:336, 6] <- valuepersquarefoot[1:261,ind]
  }
  
  colnames(data)[7] <- c("Foreclosures")
  data[97:332, 7] <- rev(clean.numeric(foreclosures[,i+1]))
  
  
  colnames(data)[8] <- c("Gas Consumption")
  data[1:313, 8] <- clean.numeric(gasconsumption[, i+1])
  
  colnames(data)[9] <- c("Electricity")
  data[1:313, 9] <- clean.numeric(electricity[, i+1])
  
  colnames(data)[10] <- c("Median Income")
  ind <- which(countyname == colnames(income))
  data[73:301, 10] <- clean.numeric(income[,ind])
  
  colnames(data)[11] <- c("Population Density")
  data[1:313, 11] <- clean.numeric(popdens[,i+1])

  colnames(data)[12] <- c("Crime Occurrences")
  data[1:313,12] <- crime[121:433,i+1]

  colnames(data)[13] <- c("Passenger Fares")
  ind <- which(countyname == colnames(fares))
  data[157:301,13] <- fares[,ind]

  colnames(data)[14] <- c("Vehicle Hours Driven")
  ind <- which(countyname == colnames(hours))
  data[157:301,14] <- hours[,ind]
  
  colnames(data)[15] <- c("Vehicle Miles Driven")
  ind <- which(countyname == colnames(miles))
  data[157:301,15] <- miles[,ind]
  
  tables[[i]] <- data
}
print(proc.time() - start)
}
# Convert counts into rates
rate_inds <- c(8,9,12,13,14,15)
for (i in 1:58) {
  for (rate_ind in rate_inds) {
    tables[[i]][,rate_ind] = tables[[i]][,rate_ind] / population[,i+1]
  }
}


View(tables[[1]])
saveRDS(tables, "Monthly_RDS/CountyMonthlyData.rds")
write.csv(tables, "Monthly_CSV/CountyMonthlyData.csv")
counts <- cbind(tables[[1]])
counts[,-1] <- 0
for (i in 1:length(tables)) {
  counts[,-1] <- counts[,-1] + 1 - matrix(as.numeric(is.na.data.frame(tables[[i]][,-1])), nrow=336, byrow=FALSE)
}
View(counts)
