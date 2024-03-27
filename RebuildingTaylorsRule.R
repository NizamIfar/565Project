library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

# Load datasets
pce_inflation <- read_csv("~/Downloads/DPCCRV1Q225SBEA.csv")
unrate <- read_csv("~/Downloads/UNRATE.csv")
nrou <- read_csv("~/Downloads/NROU.csv")

# Merging the datasets on the "DATE" column
combined_data <- reduce(list(pce_inflation, unrate, nrou), full_join, by = "DATE")

# Calculating the proposed interest rate using the provided formula
combined_data <- combined_data %>%
  mutate(Proposed_Interest_Rate = 2 + DPCCRV1Q225SBEA + 
           0.5 * (DPCCRV1Q225SBEA - 2) - 
           (UNRATE - NROU))

# Saving the combined data with the proposed interest rate to a CSV file
write_csv(combined_data, "~/Downloads/proposed_interest_rates.csv")

# Load the actual interest rates data
actual_interest_rates <- read_csv("~/Downloads/fredgraph.csv")

# Convert '.' to NA in actual_interest_rates and convert it to numeric (assuming this is necessary for your dataset)
actual_interest_rates$JCXFE_PC1_UNRATE_NROU <- as.numeric(replace(actual_interest_rates$JCXFE_PC1_UNRATE_NROU, actual_interest_rates$JCXFE_PC1_UNRATE_NROU == ".", NA))

# Load the proposed interest rates data (we just saved this, so it's a bit of a round trip in this script)
proposed_interest_rates <- read_csv("~/Downloads/proposed_interest_rates.csv")

# Ensure the DATE columns are in the same format for merging
actual_interest_rates$DATE <- as.Date(actual_interest_rates$DATE)
proposed_interest_rates$DATE <- as.Date(proposed_interest_rates$DATE)

# Merge the actual and proposed interest rates data on the DATE column
comparison_data <- merge(actual_interest_rates, proposed_interest_rates, by = "DATE")

# Plotting for visual comparison
ggplot(comparison_data, aes(x = DATE)) +
  geom_line(aes(y = JCXFE_PC1_UNRATE_NROU, color = "Actual")) + # Replace 'Actual_Interest_Rate' with your actual column name
  geom_line(aes(y = Proposed_Interest_Rate, color = "Proposed")) +
  labs(title = "Interest Rate: Actual vs Proposed",
       y = "Interest Rate",
       x = "Date") +
  theme_minimal() +
  scale_color_manual("", values = c("Actual" = "blue", "Proposed" = "red"))

comparison_data$SMA_Proposed_Interest_Rate <- zoo::rollapply(comparison_data$Proposed_Interest_Rate, width = 5, FUN = mean, na.rm = TRUE, fill = NA)

# Plotting for visual comparison with smoothing
ggplot(comparison_data, aes(x = DATE)) +
  geom_line(aes(y = JCXFE_PC1_UNRATE_NROU, color = "Actual")) +
  geom_line(aes(y = SMA_Proposed_Interest_Rate, color = "Smoothed Proposed"), linetype = "dashed") +
  labs(title = "Interest Rate: Actual vs Proposed (with Smoothing)",
       y = "Interest Rate",
       x = "Date") +
  theme_minimal() +
  scale_color_manual("", values = c("Actual" = "blue", "Smoothed Proposed" = "darkgreen")) +
  theme(legend.title = element_blank())

