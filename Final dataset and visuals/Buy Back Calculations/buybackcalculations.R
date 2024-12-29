library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)

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
    Forward_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `Forward`)))),
    EIM_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `EIM`)))),
    ICE_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `ICE`)))),
    PDX_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `PDX`))))
  ) %>%
  filter(!is.na(Datetime), Month %in% c(1, 2, 3), Year != 2020)  # Exclude year 2020

# Calculate average hourly prices for each day of January, February, and March
hourly_summary <- data %>%
  group_by(Year, Month, Hour) %>%
  summarise(
    Forward_Price = mean(Forward_Price, na.rm = TRUE),
    EIM_Price = mean(EIM_Price, na.rm = TRUE),
    ICE_Price = mean(ICE_Price, na.rm = TRUE),
    PDX_Price = mean(PDX_Price, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(Forward_Price) & !is.na(EIM_Price) & !is.na(ICE_Price) & !is.na(PDX_Price))  # Filter out any rows with NA values

# Create Month-Hour labels, ensuring the correct order
hourly_summary <- hourly_summary %>%
  mutate(
    Month_Hour = factor(
      paste0(case_when(
        Month == 1 ~ "Jan",
        Month == 2 ~ "Feb",
        Month == 3 ~ "Mar"
      ), "-", Hour),
      levels = unlist(lapply(c("Jan", "Feb", "Mar"), function(m) paste0(m, "-", 1:24)))
    )
  )
# Assuming 'data' is your data frame with hourly price data
# Define the percentage of the forward price index to set the buyback price
buyback_percentage <- 0.70  # Example: 70% of the forward price

# Calculate the average forward price for Heavy Load (HE07-22)
forward_price_he07_22 <- data %>%
  filter(Hour %in% 7:22) %>%
  summarise(avg_forward_price = mean(Forward_Price, na.rm = TRUE)) %>%
  pull(avg_forward_price)

# Calculate the buyback price as a percentage of the forward price
buyback_price <- forward_price_he07_22 * buyback_percentage

# Calculate the average price for HE19-22 (Hours 19 to 22)
avg_price_he19_22 <- data %>%
  filter(Hour %in% 19:22) %>%
  summarise(avg_price = mean(PDX_Price, na.rm = TRUE)) %>%
  pull(avg_price)

# Calculate total revenue from selling during HE19-22
total_revenue <- avg_price_he19_22 * 160  # 160 MWh sold during HE19-22

# Calculate total cost of buying back during HE07-22
total_cost <- buyback_price * 160  # 160 MWh bought during HE07-22

# Determine profit or loss
profit <- total_revenue - total_cost

# Print the results
cat("Average Forward Price during HE07-22:", forward_price_he07_22, "\n")
cat("Buyback Price (70% of Forward Price):", buyback_price, "\n")
cat("Average Price during HE19-22:", avg_price_he19_22, "\n")
cat("Total Revenue from selling (HE19-22): $", total_revenue, "\n")
cat("Total Cost from buying back (HE07-22): $", total_cost, "\n")
cat("Profit (Revenue - Cost): $", profit, "\n")
