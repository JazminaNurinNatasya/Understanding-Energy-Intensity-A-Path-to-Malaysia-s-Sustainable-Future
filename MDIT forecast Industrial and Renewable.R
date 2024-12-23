#--------------Objective 2-------------#

# Load necessary libraries
library(forecast)  # For ARIMA, ETS models
library(fpp3)
library(Metrics)  # For MAE, MAPE
library(dplyr)

data <- read.csv(file.choose(), header=T)

# Split data into two periods: 1990-2021 and 2004-2021
data_1990_2021 <- data %>% filter(Year >= 1990 & Year <= 2021)
data_2004_2021 <- data %>% filter(Year >= 2004 & Year <= 2021)

# Fit multiple linear regression models
model_1990_2021 <- lm(Energy.intensity ~ Renewable.energy + Industrial, data = data_1990_2021)
model_2004_2021 <- lm(Energy.intensity ~ Renewable.energy + Industrial, data = data_2004_2021)

# Summary of models (R-squared and Adjusted R-squared)
summary(model_1990_2021)
summary(model_2004_2021)

# Predictions
predictions_1990_2021 <- predict(model_1990_2021, newdata = data_1990_2021)
predictions_2004_2021 <- predict(model_2004_2021, newdata = data_2004_2021)


# Calculate Performance Metrics for 1990-2021 model
rmse_1990_2021 <- rmse(data_1990_2021$Energy.intensity, predictions_1990_2021)
mae_1990_2021 <- mae(data_1990_2021$Energy.intensity, predictions_1990_2021)
mape_1990_2021 <- mape(data_1990_2021$Energy.intensity, predictions_1990_2021)
mad_1990_2021 <- mad(data_1990_2021$Energy.intensity - predictions_1990_2021)

# Calculate Performance Metrics for 2004-2021 model
rmse_2004_2021 <- rmse(data_2004_2021$Energy.intensity, predictions_2004_2021)
mae_2004_2021 <- mae(data_2004_2021$Energy.intensity, predictions_2004_2021)
mape_2004_2021 <- mape(data_2004_2021$Energy.intensity, predictions_2004_2021)
mad_2004_2021 <- mad(data_2004_2021$Energy.intensity - predictions_2004_2021)

# Print performance metrics
cat("Performance Metrics for 1990-2021 Model:\n")
cat("RMSE:", rmse_1990_2021, "\n")
cat("MAE:", mae_1990_2021, "\n")
cat("MAPE:", mape_1990_2021, "\n")
cat("MAD:", mad_1990_2021, "\n")

cat("\nPerformance Metrics for 2004-2021 Model:\n")
cat("RMSE:", rmse_2004_2021, "\n")
cat("MAE:", mae_2004_2021, "\n")
cat("MAPE:", mape_2004_2021, "\n")
cat("MAD:", mad_2004_2021, "\n")


#######Objective 3


#ARIMA Industrial
data_2004_2021 <- data %>% filter(Year >= 2004 & Year <= 2021)

# Step 1: Create time series objects for Industrial
ei_ts_2004_2021_ind <- ts(data_2004_2021$Industrial, start = 2004, end = 2021, frequency = 1)

# Step 2: Fit ARIMA models to Industrial for both periods
arima_model_2004_2021_ind <- auto.arima(ei_ts_2004_2021_ind)

# Step 3: Model summaries for R-squared and adjusted R-squared approximation
summary(arima_model_2004_2021_ind)

# Step 4: Calculate performance metrics for the model

# Predict in-sample values
predictions_2004_2021_ind <- fitted(arima_model_2004_2021_ind)

# Calculate RMSE, MAE, MAPE, and MAD for the 2004-2021 model
rmse_2004_2021_ind <- rmse(data_2004_2021$Industrial, predictions_2004_2021_ind)
mae_2004_2021_ind <- mae(data_2004_2021$Industrial, predictions_2004_2021_ind)
mape_2004_2021_ind <- mape(data_2004_2021$Industrial, predictions_2004_2021_ind)
mad_2004_2021_ind <- mad(data_2004_2021$Industrial - predictions_2004_2021_ind)

# Print performance metrics
cat("\nPerformance Metrics for 2004-2021 Model:\n")
cat("RMSE:", rmse_2004_2021_ind, "\n")
cat("MAE:", mae_2004_2021_ind, "\n")
cat("MAPE:", mape_2004_2021_ind, "\n")
cat("MAD:", mad_2004_2021_ind, "\n")

# Step 5: Forecast industrial for 5 years ahead
forecast_2004_2021_ind <- forecast(arima_model_2004_2021_ind, h = 5)

# Plot the forecasts
plot(forecast_2004_2021_ind, main="Forecast for Industrial (2004-2021)")

# Step 6: Forecasting industrial for next 5 years
cat("\nForecasted Industrial for the next 5 years (2004-2021):\n")
print(forecast_2004_2021_ind$mean)

#ETS industrial
library(forecast)  # For ETS models
library(Metrics)   # For performance metrics (MAE, MAPE)
library(dplyr)


data_2004_2021 <- data %>% filter(Year >= 2004 & Year <= 2021)

# Step 1: Create time series objects for Industrial
ei_ts_2004_2021_ind <- ts(data_2004_2021$Industrial, start = 2004, end = 2021, frequency = 1)

# Step 2: Fit ETS models to Industrial for both periods
ets_model_2004_2021_ind <- ets(ei_ts_2004_2021_ind)

# Step 3: Model summaries for R-squared and adjusted R-squared approximation
summary(ets_model_2004_2021_ind)

# Step 4: Calculate performance metrics for both models

# Predict in-sample values
predictions_2004_2021_ind <- fitted(ets_model_2004_2021_ind)

# Calculate MAE, MAPE, and MAD for the 2004-2021 model
rmse_2004_2021_ind <- rmse(data_2004_2021$Industrial, predictions_2004_2021_ind)
mae_2004_2021_ind <- mae(data_2004_2021$Industrial, predictions_2004_2021_ind)
mape_2004_2021_ind <- mape(data_2004_2021$Industrial, predictions_2004_2021_ind)
mad_2004_2021_ind <- mad(data_2004_2021$Industrial - predictions_2004_2021_ind)

# Print performance metrics

cat("\nPerformance Metrics for 2004-2021 Model:\n")
cat("RMSE:", rmse_2004_2021_ind, "\n")
cat("MAE:", mae_2004_2021_ind, "\n")
cat("MAPE:", mape_2004_2021_ind, "\n")
cat("MAD:", mad_2004_2021_ind, "\n")

# Step 5: Forecast industrial for 5 years ahead
forecast_2004_2021_ind <- forecast(ets_model_2004_2021_ind, h = 5)

# Plot the forecasts
plot(forecast_2004_2021_ind, main="Forecast for Industrial Value Added (2004-2021) using Exponential Smoothing (M, Ad, N)")

# Step 6: Forecasting Industrial for next 5 years
cat("\nForecasted Industrial for the next 5 years (2004-2021):\n")
print(forecast_2004_2021_ind$mean)


#####ARIMA Renewable energy

data_2004_2021 <- data %>% filter(Year >= 2004 & Year <= 2021)

# Step 1: Create time series objects for Industrial
ei_ts_2004_2021_RE <- ts(data_2004_2021$Renewable.energy, start = 2004, end = 2021, frequency = 1)

# Step 2: Fit ARIMA models to Industrial for both periods
arima_model_2004_2021_RE <- auto.arima(ei_ts_2004_2021_RE)

# Step 3: Model summaries for R-squared and adjusted R-squared approximation
summary(arima_model_2004_2021_RE)

# Step 4: Calculate performance metrics for the model

# Predict in-sample values
predictions_2004_2021_RE <- fitted(arima_model_2004_2021_RE)

# Calculate RMSE, MAE, MAPE, and MAD for the 2004-2021 model
rmse_2004_2021_RE <- rmse(data_2004_2021$Renewable.energy, predictions_2004_2021_RE)
mae_2004_2021_RE <- mae(data_2004_2021$Renewable.energy, predictions_2004_2021_RE)
mape_2004_2021_RE <- mape(data_2004_2021$Renewable.energy, predictions_2004_2021_RE)
mad_2004_2021_RE <- mad(data_2004_2021$Renewable.energy - predictions_2004_2021_RE)

# Print performance metrics
cat("\nPerformance Metrics for 2004-2021 Model:\n")
cat("RMSE:", rmse_2004_2021_RE, "\n")
cat("MAE:", mae_2004_2021_RE, "\n")
cat("MAPE:", mape_2004_2021_RE, "\n")
cat("MAD:", mad_2004_2021_RE, "\n")

# Step 5: Forecast industrial for 5 years ahead
forecast_2004_2021_RE <- forecast(arima_model_2004_2021_RE, h = 5)

# Plot the forecasts
plot(forecast_2004_2021_RE, main="Forecast for Renewable Energy (2004-2021) using ARIMA (0,1,0) with drift")

# Step 6: Forecasting industrial for next 5 years
cat("\nForecasted Industrial for the next 5 years (2004-2021):\n")
print(forecast_2004_2021_RE$mean)

#ETS Renewable energy
library(forecast)  # For ETS models
library(Metrics)   # For performance metrics (MAE, MAPE)
library(dplyr)

data_2004_2021 <- data %>% filter(Year >= 2004 & Year <= 2021)

# Step 1: Create time series objects for Industrial
ei_ts_2004_2021_RE <- ts(data_2004_2021$Renewable.energy, start = 2004, end = 2021, frequency = 1)

# Step 2: Fit ETS models to Industrial for both periods
ets_model_2004_2021_RE <- ets(ei_ts_2004_2021_RE)

# Step 3: Model summaries for R-squared and adjusted R-squared approximation
summary(ets_model_2004_2021_RE)

# Step 4: Calculate performance metrics for both models

# Predict in-sample values
predictions_2004_2021_RE <- fitted(ets_model_2004_2021_RE)

# Calculate MAE, MAPE, and MAD for the 2004-2021 model
rmse_2004_2021_RE <- rmse(data_2004_2021$Renewable.energy, predictions_2004_2021_RE)
mae_2004_2021_RE <- mae(data_2004_2021$Renewable.energy, predictions_2004_2021_RE)
mape_2004_2021_RE <- mape(data_2004_2021$Renewable.energy, predictions_2004_2021_RE)
mad_2004_2021_RE <- mad(data_2004_2021$Renewable.energy - predictions_2004_2021_RE)

# Print performance metrics

cat("\nPerformance Metrics for 2004-2021 Model:\n")
cat("RMSE:", rmse_2004_2021_RE, "\n")
cat("MAE:", mae_2004_2021_RE, "\n")
cat("MAPE:", mape_2004_2021_RE, "\n")
cat("MAD:", mad_2004_2021_RE, "\n")

# Step 5: Forecast industrial for 5 years ahead
forecast_2004_2021_RE <- forecast(ets_model_2004_2021_RE, h = 5)

# Plot the forecasts
plot(forecast_2004_2021_RE, main="Forecast for Renewable Energy (2004-2021)")

# Step 6: Forecasting Renewable Energy for next 5 years
cat("\nForecasted Renewable Energy for the next 5 years (2004-2021):\n")
print(forecast_2004_2021_RE$mean)


##### predict energy intensity

# Create a new data frame for the next 5 years with estimates or assumptions for Renewable.energy and Industrial
new_data <- data.frame(
  Year = 2022:2026,
  Renewable.energy = c(8.238829, 8.567672, 8.896516, 9.225359, 9.554203),
  Industrial = c(36.83507, 36.69218, 36.56740, 36.45842, 36.36324)  
)

# Predict energy intensity for the next 5 years
Predict_5years <- predict(model_2004_2021, newdata = new_data)

# Combine the predicted values with the year
Predict_df <- data.frame(Year = new_data$Year, Predict_Energy_Intensity = Predict_5years)

# Print the predicted values
cat("\nPredicted Energy Intensity for the next 5 years (2022-2026):\n")
print(Predict_df)

fitted_values <- fitted(model_2004_2021)

# Combine fitted values with actual data for 2004-2021
fitted_df <- data.frame(
  Year = data_2004_2021$Year, 
  Fitted_Energy_Intensity = fitted_values
)

# Plot actual, fitted, and predicted values
library(ggplot2)

# Create a new dataset to combine actual, fitted, and predicted values
combined_data <- rbind(
  data.frame(Year = data_2004_2021$Year, Energy.Intensity = data_2004_2021$Energy.intensity, Type = "Actual"),
  data.frame(Year = fitted_df$Year, Energy.Intensity = fitted_df$Fitted_Energy_Intensity, Type = "Fitted"),
  data.frame(Year = Predict_df$Year, Energy.Intensity = Predict_df$Predict_Energy_Intensity, Type = "Predicted")
)

# Plot the combined data
ggplot(combined_data, aes(x = Year, y = Energy.Intensity, color = Type, linetype = Type)) +
  geom_line(size = 1) +
  ggtitle("Actual, Fitted, and Predicted Energy Intensity (2004-2026)") +
  xlab("Year") +
  ylab("Energy Intensity (kWh/$)") +
  scale_color_manual(values = c("Actual" = "blue", "Fitted" = "#006400", "Predicted" = "red")) + # Dark green: #006400
  scale_linetype_manual(values = c("Actual" = "solid", "Fitted" = "dashed", "Predicted" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom")
