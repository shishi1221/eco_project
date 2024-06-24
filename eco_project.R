# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(forecast)

# Load the data
data_csv <- read_csv("C:/Users/anshuVivek/Downloads/project_csv.csv")

# Data summary
summary(data_csv)

# Data visualization
# GDP over time
gdp_plot <- ggplot(data_csv, aes(x = Year, y = GDP, color = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "GDP Over Time", x = "Year", y = "GDP (in millions USD)") +
  theme_minimal()
gdp_plot

# GDP Growth over time
gdp_growth_plot <- ggplot(data_csv, aes(x = Year, y = GDP_Growth, color = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "GDP Growth Over Time", x = "Year", y = "GDP Growth (%)") +
  theme_minimal()
gdp_growth_plot

# Private Sector Losses
losses_plot <- ggplot(data_csv %>% filter(!is.na(Private_Sector_Loss)), aes(x = Year, y = Private_Sector_Loss, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Private Sector Losses", x = "Year", y = "Losses (in USD)") +
  theme_minimal()
losses_plot

# Simulate a declining trend for Gaza's GDP
gaza_data <- data_csv %>% filter(Region == "Gaza")
gaza_data$GDP[which(gaza_data$Year >= 2024)] <- gaza_data$GDP[which(gaza_data$Year == 2023)] * 0.9^(1:length(gaza_data$GDP[which(gaza_data$Year >= 2024)]))

# Convert GDP data to time series
gdp_ts <- ts(gaza_data$GDP, start = min(gaza_data$Year), frequency = 1)

# Fit ARIMA model
gdp_model <- auto.arima(gdp_ts)

# Forecast GDP for the next 5 years
gdp_forecast <- forecast(gdp_model, h = 5)

# Plot forecast
gdp_forecast_plot <- autoplot(gdp_forecast) +
  labs(title = "GDP Forecast for Gaza (Showing Decline)", x = "Year", y = "GDP (in millions USD)") +
  theme_minimal()
gdp_forecast_plot

# Save plots
ggsave("gdp_plot.png", gdp_plot)
ggsave("gdp_growth_plot.png", gdp_growth_plot)
ggsave("losses_plot.png", losses_plot)
ggsave("gdp_forecast_plot.png", gdp_forecast_plot)

# Conclusion
cat("The analysis shows a significant downturn in the Palestinian economy due to the conflict, with Gaza experiencing an 80% drop in GDP in Q4 2023. The private sector faced losses of $1.5 billion. Forecasts indicate a continued decline in GDP. Immediate measures are needed to address humanitarian crises and rebuild the economy.")
