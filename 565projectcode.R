library(ggplot2)
library(readr)
library(forecast) # for ADF test
library(tseries) # also for ADF test

data <- read_csv("~/Downloads/fredgraph.csv")

# Convert '.' to NA in JCXFE_PC1_UNRATE_NROU and convert it to numeric
data$JCXFE_PC1_UNRATE_NROU <- as.numeric(replace(data$JCXFE_PC1_UNRATE_NROU, data$JCXFE_PC1_UNRATE_NROU == ".", NA))

# Convert DATE to Date type
data$DATE <- as.Date(data$DATE)

# Plotting
ggplot(data, aes(x = DATE)) +
  geom_line(aes(y = JCXFE_PC1_UNRATE_NROU, colour = "JCXFE_PC1_UNRATE_NROU")) +
  geom_line(aes(y = FEDFUNDS, colour = "FEDFUNDS")) +
  labs(y = "Rate", colour = "Indicator") +
  theme_minimal()

###Differencing:

# Handle missing values and convert types
data$JCXFE_PC1_UNRATE_NROU <- as.numeric(replace(data$JCXFE_PC1_UNRATE_NROU, data$JCXFE_PC1_UNRATE_NROU == ".", NA))
data$DATE <- as.Date(data$DATE)

# Differencing to remove trend - prepend NA to match the original data frame length
data$Differenced_FEDFUNDS <- c(NA, diff(data$FEDFUNDS, differences = 1))
data$Differenced_JCXFE_PC1_UNRATE_NROU <- c(NA, diff(data$JCXFE_PC1_UNRATE_NROU, differences = 1))

# ADF test for stationarity
adf.test(data$Differenced_FEDFUNDS[-1], alternative = "stationary")
adf.test(data$Differenced_JCXFE_PC1_UNRATE_NROU[-c(1:5)], alternative = "stationary")

# Transformations if variance is non-constant (optional step)
data$Transformed_FEDFUNDS <- log(data$FEDFUNDS)

# Plotting the differenced series
ggplot(data, aes(x = DATE)) +
  geom_line(aes(y = Differenced_FEDFUNDS, colour = "Differenced FEDFUNDS")) +
  geom_line(aes(y = Differenced_JCXFE_PC1_UNRATE_NROU, colour = "Differenced JCXFE_PC1_UNRATE_NROU")) +
  labs(y = "Differenced Rate", colour = "Indicator") +
  theme_minimal()

##ACF Plot:

Acf(data$Differenced_FEDFUNDS[-1], main="ACF for Differenced FEDFUNDS")
Pacf(data$Differenced_FEDFUNDS[-1], main="PACF for Differenced FEDFUNDS")
eacf(data$Differenced_FEDFUNDS[-1])

#part e with 1st model: ARMA(2,2)
series <-data$Differenced_FEDFUNDS[-1]
model <- arima(series, order = c(2, 0, 2))
model
residuals <- model$residuals
plot(residuals, type = 'l', main = "Residuals of the ARMA(2,2) Model", ylab = "Residuals", xlab = "Time")
abline(h = 0, col = "red")
acf(residuals, main="ACF of Residuals")
pacf(residuals, main="PACF of Residuals")
Box.test(residuals, lag = round(log(length(series))), type = "Ljung-Box")
model$aic
rolling.forecast_ase = function(series, start, order=c(2,0,2)) {
  T = length(series)
  errors = numeric(30) # Initialize a vector to store the squared errors
  for (t in (T-30):(T-1)) {
    model = arima(series[1:t], order=order)
    forecast = predict(model, n.ahead=1)
    predicted_value = forecast$pred
    actual_value = series[t+1]
    errors[t-(T-31)] = (predicted_value - actual_value)^2
  }
  
  ASE = mean(errors)
  return(ASE)
}
ASE = rolling.forecast_ase(series, start=T-30, order=c(2,0,2))
print(ASE)

#part e with 1st model: ARMA(3,7)
model <- arima(series, order = c(3, 0, 7))
model
residuals <- model$residuals
plot(residuals, type = 'l', main = "Residuals of the ARMA(2,2) Model", ylab = "Residuals", xlab = "Time")
abline(h = 0, col = "red")
acf(residuals, main="ACF of Residuals")
pacf(residuals, main="PACF of Residuals")
Box.test(residuals, lag = round(log(length(series))), type = "Ljung-Box")
model$aic
rolling.forecast_ase = function(series, start, order=c(2,0,2)) {
  T = length(series)
  errors = numeric(30) # Initialize a vector to store the squared errors
  for (t in (T-30):(T-1)) {
    model = arima(series[1:t], order=order)
    forecast = predict(model, n.ahead=1)
    predicted_value = forecast$pred
    actual_value = series[t+1]
    errors[t-(T-31)] = (predicted_value - actual_value)^2
  }
  
  ASE = mean(errors)
  return(ASE)
}
ASE = rolling.forecast_ase(series, start=T-30, order=c(3,0,7))
print(ASE)

