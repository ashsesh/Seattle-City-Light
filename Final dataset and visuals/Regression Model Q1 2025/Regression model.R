library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(writexl)

# Load and clean the data
file_path <- "../Projects/Copy of Dataset 20240228.csv"
data <- read_csv(file_path, skip = 1)

data <- data %>%
  mutate(
    Year = as.numeric(gsub(",", "", Yr)),
    Month = as.numeric(gsub(",", "", Mo)),
    Day = as.numeric(gsub(",", "", Dy)),
    Hour = as.numeric(gsub(",", "", Hr)),
    Datetime = as.POSIXct(paste(Year, Month, Day, Hour, sep = "-"), format = "%Y-%m-%d-%H"),
    Forward_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `Forward`))))
  ) %>%
  filter(!is.na(Datetime), Month %in% c(1, 2, 3), Year != 2020)  # Exclude year 2020

# Build a linear regression model using historical data
model <- lm(Forward_Price ~ factor(Month) + factor(Hour) + lag(Forward_Price) + lag(Forward_Price, 2), 
            data = data)

# Prepare the future data for prediction (2025)
forward_prices_2024 <- data %>%
  filter(Year == 2024) %>%
  select(Month, Hour, Forward_Price) %>%
  mutate(Year = 2025)

# Predict forward prices for 2025 using the model
predicted_prices_2025 <- predict(model, newdata = forward_prices_2024)

# Combine predictions with the original data for plotting or further analysis
future_data <- forward_prices_2024 %>%
  mutate(Predicted_Forward_2025 = predicted_prices_2025)

# Export the predictions to Excel
write_xlsx(future_data, "predicted_forward_prices_2025 final.xlsx")

# Create and save the plot for regression model
combined_data_for_plot <- bind_rows(
  data %>% filter(Year %in% 2021:2024) %>% select(Year, Month, Hour, Forward_Price),
  future_data %>% mutate(Year = 2025, Forward_Price = Predicted_Forward_2025)
)

create_plot <- function(combined_data_for_plot, file_name) {
  ggplot(combined_data_for_plot, aes(x = factor(Hour), y = Forward_Price, color = factor(Year), group = interaction(Year, Month))) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE) +
    facet_wrap(~Month, scales = "free_y") +
    labs(title = "Forward Prices with Regression Predictions (2021-2025)", x = "Hour", y = "Forward Price ($)", color = "Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Save the plot
  ggsave(file_name, width = 12, height = 6)
}

create_plot(combined_data_for_plot, "Final Forward Regression Model.png")
