library(dplyr)
library(readr)
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

# Calculate historical year-over-year percent changes
percent_change_history <- data %>%
  group_by(Year, Month, Hour) %>%
  arrange(Year) %>%
  mutate(
    Forward_Price_Change = (Forward_Price - lag(Forward_Price)) / lag(Forward_Price) * 100
  ) %>%
  filter(!is.na(Forward_Price_Change))

# Check the percent change calculations
summary(percent_change_history$Forward_Price_Change)

# Calculate the average percent change for each month-hour combination
average_percent_change <- percent_change_history %>%
  group_by(Month, Hour) %>%
  summarise(Average_Forward_Change = mean(Forward_Price_Change, na.rm = TRUE))

# Check the average percent change calculations
summary(average_percent_change$Average_Forward_Change)

# Use 2024 forward prices to estimate 2025 forward prices using the percent change approach
forward_prices_2024 <- data %>%
  filter(Year == 2024) %>%
  select(Month, Hour, Forward_Price)

estimated_forward_prices_2025 <- forward_prices_2024 %>%
  left_join(average_percent_change, by = c("Month", "Hour")) %>%
  mutate(
    Estimated_Forward_2025 = ifelse(is.na(Average_Forward_Change), Forward_Price, Forward_Price * (1 + Average_Forward_Change / 100))
  )

# Export the combined estimates to Excel
write_xlsx(estimated_forward_prices_2025, "estimated_forward_prices_2025 final.xlsx")

# Create and save the plot for percent change method
combined_data_for_plot <- bind_rows(
  data %>% filter(Year %in% 2021:2024) %>% select(Year, Month, Hour, Forward_Price),
  estimated_forward_prices_2025 %>% mutate(Year = 2025, Forward_Price = Estimated_Forward_2025) %>% select(Year, Month, Hour, Forward_Price)
)

create_plot <- function(combined_data_for_plot, file_name) {
  ggplot(combined_data_for_plot, aes(x = factor(Hour), y = Forward_Price, color = factor(Year), group = interaction(Year, Month))) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE) +
    facet_wrap(~Month, scales = "free_y") +
    labs(title = "Forward Prices (2021-2025)", x = "Hour", y = "Forward Price ($)", color = "Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Save the plot
  ggsave(file_name, width = 12, height = 6)
}

create_plot(combined_data_for_plot, "Final Pct Change Model.png")
