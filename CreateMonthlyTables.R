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
buildingpermits <- readRDS("RDS/BuildingPermits.RDS")
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

countynames <- buildingpermits$County

f <- function(x) as.numeric(gsub("[,\\$%]","",x))
clean.numeric <- function(x) sapply(x,f)
medhomecsv <- read.csv("Old Excel Files/201712 MedianPricesofExistingDetachedHomesHistoricalData.csv")
colnames(medhomecsv) %<>% (function(x) gsub(".", " ",x, fixed=TRUE))
medhomecsv <- medhomecsv[1:336,]

if (TRUE) {
tables <- list()
num_cols = 3
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
  
  colnames(data)[3] <- c("Rental Price")
  ind <- which(rentalprice$RegionName == countyname)
  if (length(ind) != 0) {
    
    data[241:334, 3] <- c(clean.numeric(rentalprice[ind,7:100]))
  }
  
  #colnames(data)[4] <- c("Unemployment")
  #countyunemployment <- subset(unemployment, grepl(countyname,`Area Name`) & `Seasonally Adjusted (Y/N)`=="Y")
  #data[,4] <- countyunemployment[,10]
  
  tables[[i]] <- data
}
print(proc.time() - start)
}
View(tables[1])
