library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(ggthemr)
library(grid)
singles <- readRDS("singlesize.rds") 
colnames(singles) <- c("Year", "Total", "(0,1400]","(1400,1799]","(1799,2399]","(2399,2999]","(2999,4000]")
singles %<>% gather("Range", "Number of Single Units", -Year, -Total)
View(singles)
singles %>% ggplot(aes(Year, `Number of Single Units`, col =Range)) + geom_line() + facet_wrap(~ Range, scale="fixed")
singles %>% ggplot(aes(Range, `Number of Single Units`, fill=Range)) + geom_col() + facet_wrap(~ Year, scale="fixed") + theme(legend.position = "bottom", axis.text.x= element_blank(), axis.ticks.x = element_blank())
if (TRUE) {
  housingUnits <-  data.frame(County=character(),
                              "Total Housing Units"=integer(), 
                              Year=character(), 
                              stringsAsFactors=FALSE) 
  for (year in 2010:2017) {
    curHousingUnits <- read.csv(paste("E5", year, ".csv", sep=""), skip=3, nrows=58)
    colnames(curHousingUnits)[1] <- "County"
    colnames(curHousingUnits)[5] <- "Total Housing Units"
    curHousingUnits <- curHousingUnits[,1:13] %>% subset(select=c("County", "Total Housing Units"))
    curHousingUnits$Year = year
    curHousingUnits$`Total Housing Units` <- as.numeric(gsub(",","",curHousingUnits$`Total Housing Units`))
    housingUnits %<>% rbind(curHousingUnits)
  }
  View(housingUnits)
  housingUnits <- housingUnits[order(housingUnits$County, housingUnits$Year),]
  nums <- housingUnits$`Total Housing Units`
  numsDisplaced <- c(nums[-1], 0)
  difs <- numsDisplaced - nums
  housingUnits$difs <- difs
  housingUnits %>% ggplot(aes(Year, `Total Housing Units`, col = County)) + geom_line() + facet_wrap(~County)
  housingUnits %>% subset(Year != 2017) %>% ggplot(aes(Year, difs, col = County)) + 
    geom_line(show.legend=F) + facet_wrap(~County) + ggtitle("Total housing units constructed") + ylab("Number of units") +
    theme(panel.spacing = unit(.7, "lines"))
  countyNames = as.character(unique(housingUnits$County))
  countyNames = countyNames[countyNames != "Lake"]
  smallNames <- vector()
  mediumNames <-vector()
  bigNames <- vector()
  for (countyName in countyNames) {
    if (max(subset(housingUnits, Year!=2017 & County==countyName)$difs) < 500) {
      smallNames <- c(smallNames, countyName)
    }
    else if (max(subset(housingUnits, Year!=2017 & County==countyName)$difs) < 2500){
      mediumNames <- c(mediumNames, countyName)
    }
    else {
      bigNames <- c(bigNames, countyName)
    }
  }
  
  housingUnits %>% subset(Year != 2017 & County %in% smallNames) %>% 
    ggplot(aes(Year, difs, col = County)) + geom_line(show.legend=F) + facet_wrap(~County) + ggtitle("Total housing units constructed (smaller counties)") + 
    ylab("Number of units") + theme(panel.spacing = unit(.7, "lines"))
  housingUnits %>% subset(Year != 2017 & (County %in% mediumNames | County == "Lake")) %>% 
    ggplot(aes(Year, difs, col = County)) + geom_line(show.legend=F) + facet_wrap(~County) + ggtitle("Total housing units constructed (medium counties)") + 
    ylab("Number of units") + theme(panel.spacing = unit(.7, "lines"))
  housingUnits %>% subset(Year != 2017 & (County %in% bigNames)) %>% 
    ggplot(aes(Year, difs, col = County)) + geom_line(show.legend=F) + facet_wrap(~County) + ggtitle("Total housing units constructed (larger counties)") + 
    ylab("Number of units") + theme(panel.spacing = unit(.7, "lines"))
}

gas <- readRDS("gas.rds")
View(gas)
gas %<>% gather("Type", "Price per Gallon", -Year)
gas %>% ggplot(aes(Year, `Price per Gallon`, col=Type)) + geom_line(size=1) +
  ggtitle("Gas prices") + ylab("Price per Gallon (Dollars)")

electricity <- readRDS("electricity.rds")[,-5]
View(electricity)
electricity %<>% gather("Type", "Price per kWh", -Year)
electricity %>% ggplot(aes(Year, `Price per kWh`, col=Type)) + geom_line(size=1.5)  +
  ggtitle("Electricity prices") + ylab("Price per kWh (Dollars)")
