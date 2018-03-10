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
unemployment <- readRDS("RDS/unemployment.rds")
rentalprice <- readRDS("RDS/RentalPrice.RDS") %>% subset(State == "CA")
acs <- readRDS("data-collection/cleaned-data/Tidy ACS Data (2011-2016)")

countynames <- buildingpermits$County


f <- function(x) as.numeric(gsub("[,\\$%]","",x))
clean.numeric <- function(x) sapply(x,f)
housingUnits[,2:14] %<>% clean.numeric

tables <- list()
num_cols = 117
for (i in 1:58) {
  data <- data.frame(matrix(ncol = num_cols, nrow = 28))
  countyname = countynames[i]
  print(paste("Currently on",countyname))
  colnames(data)[1] <- "Year"
  data$Year <- 1990:2017
  
  colnames(data)[2] <- "Building Permits"
  data[12:28,2] <- clean.numeric(buildingpermits[i,2:18])
  
  colnames(data)[3] <- "Median Income"
  data[7:26, 3] <- clean.numeric(subset(medinc, County == countyname)[,5])
  
  colnames(data)[4] <- "Median Home Price"
  ind <- which(colnames(medhome) == countyname)
  if (length(ind) != 0) {
    v <- medhome[,ind]
    v[is.nan(v)] <- NA
    data[1:28,4] <- v
  }
  
  colnames(data)[5:7] <- c("Total Housing Units", "Vacancy Rate", "Persons per Household")
  data[21:28,5:7] <- subset(housingUnits,County==countyname)[,c(5,12,13)]
  
  colnames(data)[8] <- c("Population Density")
  data[21:28,8] <- clean.numeric(popdens[i,2:9])
  
  colnames(data)[9] <- c("Passenger Fares")
  ind <- which(fares$County == countyname)
  data[14:26,9] <- clean.numeric(fares[ind,2:14])
  
  colnames(data)[10] <- c("Vehicle Inventory")
  ind <- which(inventory$County == countyname)
  if (length(ind) != 0) {
    data[14:26,10] <- clean.numeric(inventory[ind,2:14])
  }
  
  colnames(data)[11] <- c("Vehicle Miles")
  ind <- which(miles$`Transit Organization` == countyname)
  if (length(ind) != 0) {
    data[14:26,11] <- clean.numeric(miles[ind,2:14])
  }
  
  colnames(data)[12] <- c("Vehicle Hours")
  ind <- which(hours$County == countyname)
  if (length(ind) != 0) {
    data[14:26,12] <- clean.numeric(hours[ind,2:14])
  }
  
  colnames(data)[13] <- c("HAI")
  ind <- which(colnames(hai)==countyname)
  if (length(ind) != 0) {
    v <- hai[,ind]
    v[is.nan(v)] <- NA
    data[2:28,13] <- v
  }
  
  colnames(data)[14] <- c("Unemployment")
  newunemployment <- c()
  countyunemployment <- subset(unemployment, grepl(countyname,`Area Name`))
  f <- function(x) as.numeric(gsub("[,\\$%]","",x))
  for (j in 1:(336/12)) {
    newunemployment <- c(newunemployment, mean(sapply(countyunemployment[(12*(j-1)+1):(12*j),11],f), na.rm=TRUE))
  }
  data[,14] <- newunemployment
  
  colnames(data)[15] <- c("Rental Price")
  ind <- which(rentalprice$RegionName == countyname)
  if (length(ind) != 0) {
    newrents <- c()
    for (j in 1:8) {
      newrents <- c(newrents, mean(clean.numeric(rentalprice[ind, ((j-1)*12+7):(min(100,(j*12+7)))]), na.rm=TRUE))
      newrents[is.nan(newrents)] <- NA
    }
    data[21:28, 15] <- newrents
  }
  
  cols <- unique(acs$`Statistic Description`)
  colnames(data)[16:117] <- cols
  for (k in 1:length(cols)) {
    col = cols[k]
    smallTable <- subset(acs, `Statistic Description` == col & grepl(paste(countyname, "County"), `Geography`) &Statistic == "Estimate")
    if (dim(smallTable)[1] != 0) {
    for (j in 1:length(smallTable$Year)) {
      year <- smallTable$Year[j]
      data[year-1989, k+15] = smallTable$Value[j]
    }
    }
  }
  
  tables[[i]] <- data
}
View(tables[1])


counts <- cbind(tables[[1]])
counts[TRUE] <- 0
counts[,1] <- 1990:2017
for (i in 1:length(tables)) {
  for (j in 1:length(tables[[1]][,1])) {
    for (k in 2:length(tables[[1]][1,])) {
      if (!is.na(tables[[i]][j,k])) {
        counts[j,k] <- counts[j,k] + 1
      }
    }
  }
}
View(counts)

for (i in 1:length(tables)) {
  
}

linearRegression <- function(countyname) {
  i = which(countynames == countyname) 
  data <- tables[[i]]
  fit0315 <- lm(data[14:24,13] ~ data[14:24,2] + data[14:24,3] + data[14:24,4] + data[14:24,9] + data[14:24,10] + data[14:24,11] + data[14:24,12])
  coeff <- coefficients(fit0315)
  mat <- cbind.data.frame(rep(1,13), data[14:26,2], data[14:26,3], data[14:26,4], data[14:26,9], data[14:26,10], data[14:26,11], data[14:26,12])
  hai_pred <- as.vector(as.matrix(mat) %*% as.matrix(coeff))
  data$pred <- c(rep(NA, 13),hai_pred, rep(NA,2))
  #data[14:26,] %>% ggplot(aes(Year)) + geom_point(aes(Year, HAI), color='red') + geom_point(aes(Year, pred), color='blue')
}
linearRegressionPlot <- function(countyname) {
  i = which(countynames == countyname) 
  data <- tables[[i]]
  fit0315 <- glm(data[14:24,13] ~ data[14:24,2] + data[14:24,3] + data[14:24,4] + data[14:24,9] + data[14:24,10] + data[14:24,11] + data[14:24,12])
  coeff <- coefficients(fit0315)
  mat <- cbind.data.frame(rep(1,13), data[14:26,2], data[14:26,3], data[14:26,4], data[14:26,9], data[14:26,10], data[14:26,11], data[14:26,12])
  hai_pred <- as.vector(as.matrix(mat) %*% as.matrix(coeff))
  data$pred <- c(rep(NA, 13),hai_pred, rep(NA,2))
  ggplot(data[14:26,], aes(Year)) + geom_point(aes(Year, HAI), color='red') + geom_point(aes(Year, pred), color='blue')
}
goodnames <- c()
for (countyname in countynames) {
  tryCatch({linearRegression(countyname)
    goodnames <- c(goodnames,countyname)},warning = function(war) {
    }, error = function(err) {
    }, finally = {
    })
}
plots <- list()
i = 1
for (countyname in goodnames) {
  plots[[i]] <- linearRegressionPlot(countyname)
  i = i + 1
}
source("multiplot.R")
multiplot(plotlist=plots, cols=9)
  godata <- tables[[1]]
newmedhome <- data.frame(matrix(ncol = 52, nrow=336/12))
medhome = medhome[1:336,]
f <- function(x) as.numeric(gsub("[,\\$]","",x))
for (i in 1:(336/12)) {
  newmedhome[i,1] <- i + 1989
  newmedhome[i,2:52] <- colMeans(sapply(medhome[(12*(i-1)+1):(12*i),3:53],f), na.rm=TRUE)
}
colnames(newmedhome) <- c("Year", colnames(medhome)[3:53])
saveRDS(newmedhome, "MedianHomePrice.rds")
saveRDS(tables, "dataByCounty.rds")
