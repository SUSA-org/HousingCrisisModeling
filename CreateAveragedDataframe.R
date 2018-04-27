library(pls)
library(caret)
library(AppliedPredictiveModeling)
library(lars)
library(MASS)
library(ggplot2)
hai <- readRDS("TraditionalHAIByMonth.rds")
data <- readRDS("Monthly_RDS/CountyMonthlyData.rds")
buildingpermitsfornames <- readRDS("RDS/BuildingPermits.RDS")
countynames <- buildingpermitsfornames$County
population <- readRDS("Monthly_RDS/Population.RDS")

inds = c()
for (i in 1:58) {
  countyname = countynames[i]
  ind <- which(grepl(countyname, colnames(valuepersquarefoot)))
  if (length(ind) != 0) {
    inds <- c(inds, i)
  }
}
newinds = c()
for (i in inds) {
  countyname = countynames[i]
  ind <- which(grepl(countyname, colnames(hai)))
  if (length(ind) != 0 & !all(is.na(hai[,ind]))) {
    newinds <- c(newinds, i)
  }
}

population_weights <- cbind(population)
for (i in 1:336) {
  rowsum = sum(population[i,-1],na.rm=T)
  population_weights[i,-1] <- population[i,-1] / rowsum
}
county_lm <- as.data.frame(scale(county[,-c(1,3,5,7,8,14,16)]))
all_counties <- as.data.frame(matrix(0, ncol = 10, nrow = 336))
colnames(all_counties) <- colnames(county_lm)
for (i in 1:length(newinds)) {
  # Extract only county
  county <- data[[newinds[i]]]
  countyname <- countynames[newinds[i]]
  
  hai_ind <- which(grepl(countyname, colnames(hai)))
  county_hai <- c(rep(NA,12), hai[,hai_ind])
  county$hai <- county_hai
  
  #Modify HAI
  county$`Modified HAI` <- county$hai - 0.01*county$Foreclosures - 0.1*county$Unemployment
  county$`Modified HAI` <- (county$`Modified HAI` - mean(county$`Modified HAI`, na.rm=T)) / sd(county$`Modified HAI`, na.rm=T) * sd(county$hai, na.rm=T) + mean(na.omit(county$hai))
  county_lm <- as.data.frame(county[,-c(1,3,5,7,8,14,16)])
  for (j in 1:336) {
    all_counties[j,] <- all_counties[j,] + county_lm[j,] * population_weights[j,newinds[i]+1]
  }
}
all_counties[all_counties == 0] <- NA
saveRDS(all_counties, "Monthly_RDS/AllCounties.RDS")
