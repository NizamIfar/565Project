library(ggplot2)
library(readr)
library(forecast) # for ADF test
library(tseries) # also for ADF test

data <- read.csv("C:\\Users\\mmr257\\Downloads\\fredgraph.csv", header=TRUE)

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
# data$Transformed_FEDFUNDS <- log(data$FEDFUNDS)

# Plotting the differenced series
ggplot(data, aes(x = DATE)) +
  geom_line(aes(y = Differenced_FEDFUNDS, colour = "Differenced FEDFUNDS")) +
  geom_line(aes(y = Differenced_JCXFE_PC1_UNRATE_NROU, colour = "Differenced JCXFE_PC1_UNRATE_NROU")) +
  labs(y = "Differenced Rate", colour = "Indicator") +
  theme_minimal()
