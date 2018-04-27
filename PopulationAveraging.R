hai <- readRDS("TraditionalHAIByMonth.rds")
data <- readRDS("Monthly_RDS/CountyMonthlyData.rds")
buildingpermitsfornames <- readRDS("RDS/BuildingPermits.RDS")
countynames <- buildingpermitsfornames$County
pop <- readRDS("Monthly_RDS/Population.RDS")

coeffs <- as.data.frame(matrix(ncol = 10, nrow = 29))
colnames(coeffs) <- c("Intercept", colnames(county_lm[1:9]))
for (i in 1:length(newinds)) {
  # Extract only county
  county <- data[[newinds[i]]]
  
  #Modify HAI
  county_hai <- c(rep(NA,12), hai$Alameda)
  county$hai <- county_hai
  
  #Plot Modified HAI
  county$`Modified HAI` <- county$hai - 0.01*county$Foreclosures - 0.1*county$Unemployment
  county$`Modified HAI` <- (county$`Modified HAI` - mean(county$`Modified HAI`, na.rm=T)) / sd(county$`Modified HAI`, na.rm=T) * sd(county$hai, na.rm=T) + mean(na.omit(county$hai))
  ggplot(county) +
    geom_line(aes(x = Date, y = hai), color = 'red') +
    geom_line(aes(x = Date, y = `Modified HAI`), color = 'blue')
  
  county_lm <- as.data.frame(scale(county[,-c(1,3,5,7,8,14,16)]))
  model <- lm(`Modified HAI` ~ ., data = county_lm)
  coeffs[i,] <- coefficients(model)
}
for (i in 1:10) {
  if (i == 1) {
    title <- "Intercept"
  } else {
    title <- colnames(county_lm)[i-1]
  }
  print(ggplot(coeffs) + geom_line(aes(x=1:29, y=coeffs[,i])) + ggtitle(title))
}