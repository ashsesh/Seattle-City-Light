# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)

# Load the data
file_path <- "../Projects/Copy of Dataset 20240228.csv"
data <- read_csv(file_path, skip = 7)

# Clean and filter data for October
data <- data %>%
  mutate(
    Year = as.numeric(gsub(",", "", Yr)),
    Month = as.numeric(gsub(",", "", Mo)),
    Day = as.numeric(gsub(",", "", Dy)),
    Hour = as.numeric(gsub(",", "", Hr)),
    Datetime = as.POSIXct(paste(Year, Month, Day, Hour, sep = "-"), format = "%Y-%m-%d-%H"),
    `Net Market` = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `Net Market`)))),
    Load = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", Load)))),
    Generation = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", Generation)))),
    Forward_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `Forward`)))),
    EIM_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `EIM`)))),
    ICE_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `ICE`)))),
    PDX_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `PDX`))))
  ) %>%
  filter(!is.na(Datetime), Month == 3)  # Adjust to October

# Calculate average hourly prices for each hour across the month for each year
hourly_summary <- data %>%
  group_by(Year, Hour) %>%
  summarise(
    Forward_Price = mean(Forward_Price, na.rm = TRUE),
    EIM_Price = mean(EIM_Price, na.rm = TRUE),
    ICE_Price = mean(ICE_Price, na.rm = TRUE),
    PDX_Price = mean(PDX_Price, na.rm = TRUE)
  ) %>%
  ungroup()

# Function to create and save plot
create_plot <- function(data, y_var, title, file_name) {
  ggplot(data, aes(x = Hour, y = !!sym(y_var), color = factor(Year), group = Year)) +
    geom_line(size = 0.75) +
    labs(title = title, x = "Hour", y = y_var, color = "Year") +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      axis.text.x = element_text(angle = 45, hjust = 1) # Angle the x-axis labels
    ) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_y_continuous() +
    ggsave(file_name, width = 10, height = 6)
}

# Create and save plots
create_plot(hourly_summary, "Forward_Price", "Average Forward Price in March (Hourly)", "Forward March Hourly.png")
create_plot(hourly_summary, "EIM_Price", "Average EIM Price in March (Hourly)", "EIM March Hourly.png")
create_plot(hourly_summary, "ICE_Price", "Average ICE Price in March (Hourly)", "ICE March Hourly.png")
create_plot(hourly_summary, "PDX_Price", "Average PDX Price in March (Hourly)", "PDX March Hourly.png")

