library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)

# Assuming data is already loaded and cleaned
file_path <- "../Projects/Copy of Dataset 20240228.csv"
data <- read_csv(file_path, skip = 7)

# Clean and filter data for September
data <- data %>%
  mutate(
    Year = gsub(",", "", Yr),
    Month = gsub(",", "", Mo),
    Day = gsub(",", "", Dy),
    Hour = gsub(",", "", Hr),
    Year = gsub(" ", "", Yr),
    Datetime = as.POSIXct(paste(Year, Month, Day, Hour, sep = "-"), format = "%Y-%m-%d-%H"),
    `Net Market` = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `Net Market`)))),
    Load = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", Load)))),
    Generation = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", Generation)))),
    Forward_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `Forward`)))),
    EIM_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `EIM`)))),
    ICE_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `ICE`)))),
    PDX_Price = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `PDX`))))
  ) %>%
  filter(!is.na(Datetime), Month == 12)

# Calculate average hourly prices for each day of September for each year
hourly_summary <- data %>%
  group_by(Year, Day = day(Datetime), Hour) %>%
  summarise(
    Forward_Price = mean(Forward_Price, na.rm = TRUE),
    EIM_Price = mean(EIM_Price, na.rm = TRUE),
    ICE_Price = mean(ICE_Price, na.rm = TRUE),
    PDX_Price = mean(PDX_Price, na.rm = TRUE)
  ) %>%
  ungroup()

# Function to create and save plot
create_plot <- function(data, y_var, title, file_name) {
  ggplot(data, aes(x = as.POSIXct(paste0(Day, "-", Hour), format = "%d-%H"), y = !!sym(y_var), color = factor(Year), group = Year)) +
    geom_line() +
    labs(title = title, x = "Date-Hour", y = y_var, color = "Year") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      text = element_text(size = 12),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1) # Angle the x-axis labels
    ) +
    scale_x_datetime(date_labels = "%d-%H", , date_breaks = "1 day") +
    ggsave(file_name, width = 10, height = 6)
}

long_data <- data %>%
  select(Day, Hour, Year, Load, Generation) %>%
  pivot_longer(cols = c("Load", "Generation"), names_to = "Variable", values_to = "Value")

# Plot Load and Generation on the same0 graph
ggplot(long_data, aes(x = as.POSIXct(paste(Day, Hour, sep = "-"), format = "%d-%H"), y = Value, color = factor(Year), linetype = Variable)) +
  geom_line(size = 0.5) + # Thinner lines
  labs(title = "Load and Generation in December (Hourly)", x = "Day-Hour", y = "Value", color = "Year", linetype = "Variable") +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    text = element_text(size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1) # Angle the x-axis labels
  ) +
  scale_x_datetime(date_labels = "%d-%H", date_breaks = "1 day") +
  scale_linetype_manual(values = c("Load" = "solid", "Generation" = "dashed")) +
  ggsave("Load & Generation December.png", width = 10, height = 6)

# Create and save plots
create_plot(hourly_summary, "Forward_Price", "Average Forward Price in December (Hourly)", "Forward December Hourly.png")
create_plot(hourly_summary, "EIM_Price", "Average EIM Price in December (Hourly)", "EIM December Hourly.png")
create_plot(hourly_summary, "ICE_Price", "Average ICE Price in December (Hourly)", "ICE December Hourly.png")
create_plot(hourly_summary, "PDX_Price", "Average PDX Price in December (Hourly)", "PDX December Hourly.png")

