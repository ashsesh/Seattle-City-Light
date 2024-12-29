library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)

# Load and clean the data
file_path <- "../Projects/Copy of Dataset 20240228.csv"
data <- read_csv(file_path, skip = 7)

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
  filter(!is.na(Datetime), Month %in% c(1, 2, 3), Year != 2020)  # Exclude year 2020 and select months Jan-Mar

# Calculate average hourly prices for each price type
hourly_summary <- data %>%
  group_by(Year, Month, Day, Hour) %>%
  summarise(
    Forward_Price = mean(Forward_Price, na.rm = TRUE),
    EIM_Price = mean(EIM_Price, na.rm = TRUE),
    ICE_Price = mean(ICE_Price, na.rm = TRUE),
    PDX_Price = mean(PDX_Price, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate percentage change from the previous hour
hourly_summary <- hourly_summary %>%
  group_by(Year, Month, Day) %>%
  arrange(Year, Month, Day, Hour) %>%
  mutate(
    Forward_Price_Pct_Change = (Forward_Price - lag(Forward_Price)) / lag(Forward_Price) * 100,
    EIM_Price_Pct_Change = (EIM_Price - lag(EIM_Price)) / lag(EIM_Price) * 100,
    ICE_Price_Pct_Change = (ICE_Price - lag(ICE_Price)) / lag(ICE_Price) * 100,
    PDX_Price_Pct_Change = (PDX_Price - lag(PDX_Price)) / lag(PDX_Price) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(Forward_Price_Pct_Change) & !is.na(EIM_Price_Pct_Change) & !is.na(ICE_Price_Pct_Change) & !is.na(PDX_Price_Pct_Change))  # Filter out rows with NA values

# Combine percentage changes into a single dataset
combined_data <- hourly_summary %>%
  pivot_longer(cols = c("PDX_Price_Pct_Change", "Forward_Price_Pct_Change", "ICE_Price_Pct_Change"),
               names_to = "Price_Type",
               values_to = "Pct_Change") %>%
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

# Plot the percentage changes
create_plot <- function(combined_data, title, file_name) {
  ggplot(combined_data, aes(x = Month_Hour, y = Pct_Change, color = factor(Year), linetype = Price_Type, group = interaction(Year, Price_Type))) +
    geom_line() +
    labs(title = title, x = "Datetime", y = "Percentage Change (%)", color = "Year", linetype = "Price Type") +
    theme_minimal(base_family = "sans") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      text = element_text(size = 8),
      legend.position = "bottom"
    )
  
  # Save the plot
  ggsave(file_name, width = 12, height = 6)
}

# Example usage
create_plot(combined_data, "Hourly Percentage Change in PDX and Forward Prices (Jan-Mar)", "PDXwithForwardPctChangeHourly.png")
