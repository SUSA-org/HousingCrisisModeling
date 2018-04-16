library(pls)
library(caret)
library(AppliedPredictiveModeling)
library(lars)
library(MASS)
library(ggplot2)

# Normalizing HAI for response variable
hai <- readRDS("TraditionalHAIByMonth.rds")
normalized_hai <- data.frame(matrix(ncol = 45, nrow = 324))
normalized_hai[, 1] <- hai[, 1]
colnames(normalized_hai) <- colnames(hai)
CA_hai <- (hai[, 2] - mean(hai[, 2])) / sd(hai[, 2])
normalized_hai[, 2] <- CA_hai
normalized_hai[, 3] <- (hai[, 3] - mean(hai[, 3])) / sd(hai[, 3])
for (i in c(6, 8, 13, 15, 18, 21, 23, 24, 26:28, 30:34, 38, 43)) {
  normalized_hai[, i] <- (hai[, i] - mean(hai[, i])) / sd(hai[, i])
}


ggplot(data = normalized_hai, aes(x = Month, y = CA)) + geom_line()

# Read county monthly data
data <- readRDS("Monthly_RDS/CountyMonthlyData.rds")

# Extract only Alameda
Alameda <- data[[1]]


# Add normalized HAI column to Alameda dataframe to create Alameda_new
years <-
  seq(as.Date("1990-01-01"), as.Date("2017-12-01"), by = "months")
Alameda[, 1] <- years
temprow <- matrix(c(rep.int(NA, length(12 * 45))), nrow = 12, ncol = 45)
newrow <- data.frame(temprow)
colnames(newrow) <- colnames(hai)
normalized_hai_new <- rbind(newrow, normalized_hai)
normalized_hai_new[, 1] <-
  seq(as.Date("1990-01-01"), as.Date("2017-12-01"), by = "months")
Alameda_new <- cbind(Alameda, normalized_hai_new[, 3])
colnames(Alameda_new) <- c(colnames(Alameda), "HAI")

# Remove dates column
dates <- Alameda_new[,1]
Alameda_new <- Alameda_new[, -1]

# Remove vacancy rates column
Alameda_new <- Alameda_new[, -2]

# Remove passenger fares column
#Alameda_new <- Alameda_new[, -11]

Alameda_new <- as.data.frame(scale(Alameda_new))

#Add dates back in 
Alameda_new$Dates <- dates

#Modify HAI
Alameda_hai <- c(rep(NA,12), hai$Alameda)
Alameda$hai <- Alameda_hai
Alameda_small <- Alameda[c(97:332),]

#check same trend for Merced
merced <- data[[24]]
merced_hai <- c(rep(NA,12), hai$Merced)
merced$hai <- merced_hai
merced$`Modified HAI` <- merced$hai - 0.01*merced$Foreclosures - 0.1*merced$Unemployment
ggplot(merced) +
  geom_line(aes(x = Date, y = hai), color = 'red') +
  geom_line(aes(x = Date, y = `Modified HAI`), color = 'blue')



#Plot Modified HAI
Alameda$`Modified HAI` <- Alameda$hai - 0.01*Alameda$Foreclosures - 0.1*Alameda$Unemployment
Alameda$`Modified HAI` <- (Alameda$`Modified HAI` - mean(Alameda$`Modified HAI`, na.rm=T)) / sd(Alameda$`Modified HAI`, na.rm=T) * sd(Alameda$hai, na.rm=T) + mean(na.omit(Alameda$hai))
ggplot(Alameda) +
  geom_line(aes(x = Date, y = hai), color = 'red') +
  geom_line(aes(x = Date, y = `Modified HAI`), color = 'blue')

#Scaling modified HAI to match traditional HAI

#Pick important features and fit linear model
Alameda_lm <- Alameda[,-c(1,3,5,7,8,14,16)]
model <- lm(`Modified HAI` ~ ., data = Alameda_lm)
summary(model)

for (i in 1:(dim(Alameda_lm)[2]-1)) {
  name <- colnames(Alameda_lm)[i]
   plot <- model %>% augment %>%
    transmute(Alameda_lm[c(157:301),i], Error = .resid) %>%
    ggplot(aes(Alameda_lm[c(157:301),i], Error)) + 
    geom_point() + geom_hline(yintercept = 0, col = "red", linetype = "dashed") + ggtitle(name)
   print(plot)
}

#Check normality
model %>% augment %>%
  transmute(Error = .resid) %>%
  ggplot(aes(Error)) + geom_density(fill = "grey")

#Check homoscedasticitiy
error <- model$residuals
y.fitted <- model$fitted.values
plot(x = y.fitted, y = error) 




#Plotting fitted vs residuals
model %>% augment %>% ggplot(aes(Modified.HAI,.fitted))+geom_point()+geom_abline(intercept=0,slope=1)

model %>% augment %>% ggplot(aes(Modified.HAI,.resid))+geom_point()


