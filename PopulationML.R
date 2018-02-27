library(ggplot2)
library(scatterplot3d)
library(dplyr)
path <- "/Users/elliotstahnke/programs/SUSA/PopulationTimeSeries.rds"
df <- readRDS(file = path)
df <- df[,c("Year", "Country", "Fertility", "Mortality", "LifeExp", "birthRate", "popRate")]
colnames(df) <- c("Year", "Country", "Fertility", "Mortality", "LifeExp", "BirthRate", "PopRate")
df <- df[rowSums(is.na(df))<1,]
pcadata <- prcomp(df[3:6],scale. = TRUE)
variance_proportions <- variances$sdev/sum(variances$sdev)
plot(1:4,variance_proportions)

evecs <- pcadata[2]
U <- evecs$rotation[,c("PC1", "PC2")]
only2015df <- df[df$Year==2015,]
ggplot(cbind.data.frame(only2015df$Country, as.matrix(only2015df[3:6]) %*% U), aes(PC1, PC2)) +
  geom_text(aes(label=only2015df$Country), size=2.5,check_overlap = TRUE)

U <- evecs$rotation[,c("PC1", "PC2","PC3")]
only2015df <- df[df$Year==2015,]
projecteddata <- cbind.data.frame(Country=only2015df$Country, as.matrix(only2015df[3:6]) %*% U)
ggplot(projecteddata, aes(PC1, PC2)) +
  geom_text(aes(label=Country), size=2)


# df <- filter(df, )
# fert <- df$Fertility
# mort <- df$Mortality
# life <- df$LifeExp
# birth <- df$BirthRate
# pop <- df$PopRate
# fit <- lm(pop ~ fert + mort + life + birth)
# coeff <- coefficients(fit)
# data <- cbind.data.frame(rep(1, nrow(df)), fert, mort, life, birth)
# pop_predict <- as.matrix(data) %*% as.matrix(coeff)
# df$PopLR <- as.vector(pop_predict)
# df$Error <- as.vector(pop_predict - pop)
#df %>% ggplot(aes(Year)) + geom_point(aes(Year,PopRate), color='red') + geom_point(aes(Year, PopLR), color='blue')
outliers <- c("Syrian Arab Republic", "Bahrain","Lebanon", "Djibouti","United Arab Emirates")
df <- subset(df, !Country %in% outliers)
linearRegression <- function(country){
  country_df <- filter(df, Country==country)
  train <- filter(country_df, Year <= 2010)
  test <- filter(country_df, Year > 2010)
  # individual arrays
  frametocol <- train
  fert <- frametocol$Fertility
  mort <- frametocol$Mortality
  life <- frametocol$LifeExp
  birth <- frametocol$BirthRate
  pop <- frametocol$PopRate
  
  #getting coefficients from linear regression
  fit <- lm(pop ~ fert + mort + life + birth)
  coeff <- coefficients(fit)
  
  frametocol <- test
  fert <- frametocol$Fertility
  mort <- frametocol$Mortality
  life <- frametocol$LifeExp
  birth <- frametocol$BirthRate
  pop <- frametocol$PopRate
  #predicting population rate
  indep <- cbind.data.frame(rep(1, nrow(test)), fert, mort, life, birth)
  pop_predict <- as.matrix(indep) %*% as.matrix(coeff)
  test$PopLR <- pop_predict %>% as.vector
  test$Error <- (pop_predict - pop)*(pop_predict - pop) %>% as.vector
  
  summary(fit)
  test$Error
}
linearRegressionPlot <- function(country){
  country_df <- filter(df, Country==country)
  train <- filter(country_df, Year <= 2010)
  test <- filter(country_df, Year > 2010)
  # individual arrays
  frametocol <- train
  fert <- frametocol$Fertility
  mort <- frametocol$Mortality
  life <- frametocol$LifeExp
  birth <- frametocol$BirthRate
  pop <- frametocol$PopRate
  
  #getting coefficients from linear regression
  fit <- lm(pop ~ fert + mort + life + birth)
  coeff <- coefficients(fit)
  
  frametocol <- country_df
  fert <- frametocol$Fertility
  mort <- frametocol$Mortality
  life <- frametocol$LifeExp
  birth <- frametocol$BirthRate
  pop <- frametocol$PopRate
  #predicting population rate
  indep <- cbind.data.frame(rep(1, nrow(country_df)), fert, mort, life, birth)
  pop_predict <- as.vector(as.matrix(indep) %*% as.matrix(coeff))
  country_df$PopLR <- pop_predict
  country_df %>% ggplot(aes(Year)) + geom_point(aes(Year,PopRate), color='red') + geom_point(aes(Year, PopLR), color='blue')
  
}
linearRegressionPredictions <- function(country){
  country_df <- filter(df, Country==country)
  train <- filter(country_df, Year <= 2010)
  test <- filter(country_df, Year > 2010)
  # individual arrays
  frametocol <- train
  fert <- frametocol$Fertility
  mort <- frametocol$Mortality
  life <- frametocol$LifeExp
  birth <- frametocol$BirthRate
  pop <- frametocol$PopRate
  
  #getting coefficients from linear regression
  fit <- lm(pop ~ fert + mort + life + birth)
  coeff <- coefficients(fit)
  
  frametocol <- country_df
  fert <- frametocol$Fertility
  mort <- frametocol$Mortality
  life <- frametocol$LifeExp
  birth <- frametocol$BirthRate
  pop <- frametocol$PopRate
  #predicting population rate
  indep <- cbind.data.frame(rep(1, nrow(country_df)), fert, mort, life, birth)
  pop_predict <- as.matrix(indep) %*% as.matrix(coeff)
  as.vector(pop_predict)
}
badPredict <- function(country) {
  country_df <- filter(df, Country==country)
  train <- filter(country_df, Year <= 2010)
  test <- filter(country_df, Year > 2010)
  pop_predict <- rep(country_df$PopRate[country_df$Year==2010],5)
  pop <- test$PopRate
  error <-(pop_predict - pop)*(pop_predict - pop) %>% as.vector
  error
}

#CalculateMLE for linear regression
errors <-  data.frame(matrix(ncol = 2, nrow = length(unique(df$Year))))
colnames(errors) <- c("Country", "MeanError")
i <- 1
preds <- c()
for(country in unique(df$Country)) {
  cur_error <- linearRegression(country)
  preds <- c(preds, linearRegressionPredictions(country))
  errors[i,] <- c(country, mean(cur_error))
  i <- i + 1
}
df$LRPopulation <- preds
errors$`MeanError` <- as.numeric(errors$`MeanError`)
#errors[order(errors$MeanError),]
print(paste("MSE for linear regression:", mean(errors$MeanError)))


errors2 <-  data.frame(matrix(ncol = 2, nrow = length(unique(df$Year))))
colnames(errors2) <- c("Country", "MeanError")
i <- 1
for(country in unique(df$Country)) {
  errors2[i,] <- c(country, mean(badPredict(country)))
  i <- i + 1
}
errors2$`MeanError` <- as.numeric(errors2$`MeanError`)
#errors[order(errors$MeanError),]
print(paste("MSE for using 2010 population rate:", mean(errors2$MeanError)))


