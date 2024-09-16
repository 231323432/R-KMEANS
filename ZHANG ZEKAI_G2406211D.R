library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(MASS)
library(caret)
library(readxl)

data1 <- read.csv("C:/Users/94000/Desktop/analyticn software/2004/electricity_usage.csv")
str(data1)

head(data1)
#Quantize the electricity_cost into categories:Create a new categorical feature cost_category that segments the electricity_cost into three categories: Low, Medium, and High based on the 33rd and 66th percentiles
data1$cost_category <- cut(data1$electricity_cost, 
                           breaks = quantile(data1$electricity_cost, c(0, 1/3, 2/3, 1)),
                           labels = c("Low", "Medium", "High"))
#Normalize the solar_generation values:Normalize the solar_generation 
#column to scale the values between 0 and 1 and create a column called solar_generation_normalized
data1$solar_generation_normalized <- scale(data1$solar_generation, center = FALSE, scale = max(data1$solar_generation, na.rm = TRUE))

head(data1)
#Create a new feature UsagePerPerson:Calculate the per-person electricity usage UsagePerPerson by dividing the TotalUsage by the household_size
data1$UsagePerPerson <- data1$TotalUsage / data1$household_size

#Create a time-of-day feature:Create a categorical feature time_of_day that divides the hour into four segments: Night [0-6), Morning [6-12), Afternoon [12-18), and Evening [18-24).
data1$time_of_day <- cut(data1$hour, breaks = c(0, 6, 12, 18, 24), labels = c("Night", "Morning", "Afternoon", "Evening"), right = FALSE)
data1$time_of_day

# Calculate the split index
splitIndex <- floor(nrow(data1) * 0.75)

# Split the data
trainData <- data1[1:splitIndex, ]
testData <- data1[(splitIndex + 1):nrow(data1), ]

# Check dimensions of the split datasets
dim(trainData)
dim(testData)

str(data1)
#Build a Decision Tree model:Build a decision tree model to predict TotalUsage based on the features: temperature, humidity, appliance_usage, solar_generation_normalized, UsagePerPerson, and time_of_day
library(rpart)
library(party)
install.packages("party")
# Build the decision tree model
decisionTreeModel <- ctree(TotalUsage ~ temperature + humidity + appliance_usage + solar_generation_normalized + UsagePerPerson + time_of_day, data = trainData)

# Print the decision tree summary
print(decisionTreeModel)

# Plot the decision tree
plot(decisionTreeModel)


# Make predictions on the test data
testData$predictedTotalUsage <- predict(decisionTreeModel, newdata = testData)



# Calculate RMSE
rmse <- sqrt(mean((testData$TotalUsage - testData$predictedTotalUsage)^2))
rmse
mae <- mean(abs(testData$TotalUsage - testData$predictedTotalUsage))

# Print the results
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")

#These results suggest that the model's predictions deviate from the actual TotalUsage values by approximately 1.01 units on average when considering squared errors (RMSE) and by 0.89 units when considering absolute errors (MAE).




#****************************************************************************#

data2 <- read.csv("C:/Users/94000/Desktop/analyticn software/2004/scooter.csv")
str(data2)

# Check for missing values
sum(is.na(data2))
#use KNN TO Impute missing values
library(VIM)


# Function to replace outliers with NA (using IQR method)
replace_outliers_with_NA <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}

# Apply the function to numeric columns
data2_cleaned <- data2_imputed %>%
  mutate(across(c(temperature, feels.like.temperature, relative.humidity, windspeed, psi, guest.users, registered.users), 
                replace_outliers_with_NA))

# Check for missing values (now including replaced outliers)
sum(is.na(data2_cleaned))

# Impute the newly created missing values (if needed)
data2_cleaned <- kNN(data2_cleaned, k = 5)

# Check if there are still missing values after imputation
sum(is.na(data2_cleaned)) 

# Remove the "_imp" columns
data2_cleaned <- data2_cleaned[, 1:11]


str(data2_cleaned)

library(zoo)
data2_cleaned <- data2_cleaned %>%
  mutate(across(c(temperature, feels.like.temperature, relative.humidity, windspeed, psi, guest.users, registered.users),
                ~ rollmean(., k = 3, fill = NA, align = "right")))

str(data2_cleaned)
sum(is.na(data2_cleaned))
data2_cleaned <- kNN(data2_cleaned, k = 5)
sum(is.na(data2_cleaned)) 
data2_cleaned <- data2_cleaned[, 1:11]
str(data2_cleaned)

data2_cleaned <- data2_cleaned %>%
  mutate(TotalUser = guest.users + registered.users)

# Select 'temperature' as the feature to explore
selected_feature <- "temperature"

#  Scatterplot with Smoothing Line
ggplot(data2_cleaned, aes(x = .data[[selected_feature]], y = TotalUser)) +
  geom_point(alpha = 0.5) +  # Add transparency to points
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add smoothing line
  labs(title = paste("Relationship between", selected_feature, "and Total Users"),
       x = selected_feature, 
       y = "Total Users")

#  Boxplot for Different Temperature Ranges
data2_cleaned$temperature_range <- cut(data2_cleaned$temperature, 
                                       breaks = c(0, 50, 60, 70, 80, 90, Inf),
                                       labels = c("<50", "50-60", "60-70", "70-80", "80-90", "90+"))

ggplot(data2_cleaned, aes(x = temperature_range, y = TotalUser)) +
  geom_boxplot() +
  labs(title = paste("Total Users vs.", selected_feature, "Ranges"),
       x = paste(selected_feature, "Range"),
       y = "Total Users") 
#Split the dataset into training and testing sets, 
splitIndex <- createDataPartition(data2_cleaned$TotalUser, p = 0.7, list = FALSE)

# Split the data into training and testing sets
trainData2 <- data2_cleaned[splitIndex, ]
testData2 <- data2_cleaned[-splitIndex, ]

# Check dimensions of the split datasets
dim(trainData2)
dim(testData2)

#Create a Random Forest model using the training data to predict the TotalUser
library(randomForest)
# Build the random forest model
randomForestModel <- randomForest(TotalUser ~ temperature + feels.like.temperature + relative.humidity + windspeed + psi +guest.users+registered.users, data = trainData2)
# Print the random forest model summary
print(randomForestModel)
# Make predictions on the test data
testData2$predictedTotalUser <- predict(randomForestModel, newdata = testData2)

# Calculate Mean Squared Error (MSE)
mse <- mean((testData2$TotalUser - testData2$predictedTotalUser)^2)
mse
# Calculate R-squared
rsquared <- 1 - mse / var(testData2$TotalUser)
rsquared


# Create a grid of hyperparameters to search over
hyperparameters <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7, 8, 9, 10))
# Perform cross-validation to find the best hyperparameters
tunedModel <- tuneRF(x = trainData2[, c("temperature", "feels.like.temperature", "relative.humidity", "windspeed", "psi", "guest.users", "registered.users")],
                     y = trainData2$TotalUser,
                     ntree = 500,
                     stepFactor = 1.5,
                     improve = 0.01,
                     trace = TRUE,
                     plot = TRUE,
                     tuneGrid = hyperparameters)
# Print the tuned model
print(tunedModel)
importance(tunedModel)
best_mtry <- tunedModel[which.min(tunedModel[, 2]), 1]
best_mtry
final_rf_model <- randomForest(TotalUser ~ temperature + feels.like.temperature + relative.humidity + windspeed + psi +guest.users+registered.users, 
                               data = trainData2, 
                               ntree = 500, 
                               mtry = best_mtry)

# Make predictions on the test data using the final model
testData2$tunedPredictedTotalUser <- predict(final_rf_model, newdata = testData2)
# Calculate Mean Squared Error (MSE) for the tuned model
tuned_mse <- mean((testData2$TotalUser - testData2$tunedPredictedTotalUser)^2)
tuned_mse
mse
# Calculate R-squared for the tuned model
tuned_rsquared <- 1 - tuned_mse / var(testData2$TotalUser)
tuned_rsquared
rsquared
#The tuned random forest model has a lower Mean Squared Error (MSE) and a higher R-squared value compared to the initial random forest model, indicating improved performance and accuracy.


