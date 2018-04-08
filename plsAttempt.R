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
Alameda_new <- Alameda_new[, -1]

# Remove vacancy rates column
Alameda_new <- Alameda_new[, -2]

# Remove passenger fares column
Alameda_new <- Alameda_new[, -11]

# Add in Passenger Fare
# pas <- readRDS("Monthly_RDS/PassengerFareByMonth.rds")
# PassengerFare <- c(rep_len(NA, length.out = length(Alameda_new[1])))
# Alameda_new <- cbind(PassengerFare, Alameda_new)
# Alameda_new[c(157:301), 1] <- pas[, 3]

# Add in Crime
# crimedf <- readRDS("Monthly_RDS/CrimeCountByMonth.rds")
# Crime <- c(rep_len(NA, length.out = length(Alameda_new[2])))
# Alameda_new <- cbind(Crime, Alameda_new)
# Alameda_new[c(1:313), 1] <- rev(crimedf[c(1:313), 2])

Alameda_new <- as.data.frame(scale(Alameda_new))


# Take out test set
Alameda_new_test <- Alameda_new[241:nrow(Alameda_new), ]


### Only train on some of the Alameda data
Alameda_new_train <- Alameda_new[1:240, ]

# PLS on Alameda_new
set.seed(2167)
plsFit = plsr(
  HAI ~ .,
  ncomp = 8,
  data = Alameda_new_train,
  validation = "CV",
  na.action = na.omit
)
print(plsFit$validation$PRESS)
print(coef(plsFit))
# Extract coefficients from plsFit
coefs <- coef(plsFit)
coefs <- array(coefs, c(12, 1))
coefs <- matrix(coefs)
# Removing HAI
data_mat <- data.matrix(Alameda_new_train[, -13])
# Predict HAI on the training set
pred_hai <- data_mat %*% coefs

## put pred_hai and real_hai in same dataframe
library(dplyr)
Alameda_new_train <-
  Alameda_new_train %>% mutate("Predicted_HAI" = pred_hai)

# Tack on years
Alameda_new_train$Years <- years[1:240]
only_non_na_rows <- Alameda_new_train[157:length(pred_hai), ]


ggplot(only_non_na_rows) + geom_line(aes(x = Years, y = HAI), color = 'red') + geom_line(aes(x =
                                                                                               Years, y = Predicted_HAI), color = 'blue')

# plot(pred_hai)

correct_hai <- hai[229:nrow(hai), 3]
test_data_mat <- data.matrix(Alameda_new_test[,-13])
test_predictions <- test_data_mat %*% coefs

test_df <-
  data.frame(correct = correct_hai[1:61],
             predictions = test_predictions[1:61],
             years = years[241:301])
ggplot(test_df) + geom_line(aes(x = years, y = correct), color = 'red')

ggplot(test_df) + geom_line(aes(x = years, y = predictions), color = 'blue')