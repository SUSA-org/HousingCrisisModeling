library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(ggthemr)
library(grid)
housingUnits <-  data.frame(County=character(),
                            "Total Housing Units"=integer(), 
                            Year=character(), 
                            stringsAsFactors=FALSE) 
for (year in 2010:2017) {
  curHousingUnits <- read.csv(paste("E5", year, ".csv", sep=""), skip=3, nrows=58)
  colnames(curHousingUnits)[1] <- "County"
  colnames(curHousingUnits)[5] <- "Total Housing Units"
  curHousingUnits <- curHousingUnits[,1:13]
  curHousingUnits$Year = year
  curHousingUnits$`Total Housing Units` <- as.numeric(gsub(",","",curHousingUnits$`Total Housing Units`))
  housingUnits %<>% rbind(curHousingUnits)
}
medinc <- readRDS("MedianIncome.RDS")
medhome <- readRDS("MedianHomePrice.RDS")
hai <- read.csv("HistoricalHAI.csv", skip=3)
haiyear <- read.csv("HAIYearly.csv")
View(haiyear)
for (year in 1991:2017) {
  if (year <= 2005) {
    data <- hai[seq((year - 1991)*12+1, (year-1991)*12+12,1), -1]
    haiyear[year-1990,-1] <- colMeans(data, na.rm=TRUE)
  } else {
    data <- hai[seq(182+ (year - 2006) * 4, 186 + (year-2006)*4), -1]
    haiyear[year-1990, -1] <- colMeans(data, na.rm=TRUE)
  }
}
colnames(haiyear) <- gsub(".", " ", colnames(haiyear), fixed=TRUE)
buildingpermits <- readRDS("BuildingPermits.RDS")
View(buildingpermits)
rentalprice <- readRDS("RentalPrice.RDS") %>% subset(State == "CA")
View(rentalprice)
correlations <- data.frame(matrix(ncol = 14, nrow = 58))
colnames(correlations)[1:2] <- c("County","Building Permits")
colnames(correlations)[3:14] <- colnames(housingUnits)[2:13]
for (i in 1:43) {
  hai <- haiyear[,2+i]
  countyname <- colnames(haiyear)[2+i]
  ind = which(correlations$County == countyname)
  print(ind)
  permits <- c(rep(NA, 10), as.numeric(gsub(",","",buildingpermits[i,-1])))
  correlations[ind,2] <- cor(permits,hai, use="complete.obs")
  
  for (j in 1:12) {
    feature <- c(rep(NA,19), as.numeric(gsub("%", "", gsub(",", "", subset(housingUnits, County == countyname)[,1+j]), fixed=TRUE)))
    correlations[ind, j+2] <- cor(feature, hai, use="complete.obs")
  }
}
data <- data.frame(matrix(ncol = 14, nrow = 58))
colnames(data)[1] <- "County"
data$County <- buildingpermits$County
colMeans(correlations[,-1], na.rm=TRUE)
View(correlations)