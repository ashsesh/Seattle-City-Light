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

# Create a function to generate plots for each price type
create_plot <- function(y_var, title, file_name) {
  ggplot(hourly_summary, aes(x = Month_Hour, y = !!sym(y_var), color = factor(Year), group = interaction(Year, Month))) +
    geom_line() +
    labs(title = title, x = "Month-Hour", y = y_var, color = "Year") +
    theme_minimal(base_family = "sans") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      text = element_text(size = 12),
      legend.position = "bottom"
    ) +
    ggsave(file_name, width = 12, height = 6)
}

# Generate plots for January, February, and March
create_plot("Forward_Price", "Forward Prices (Jan-Mar)", "Forward_Prices_Jan_Mar.png")
create_plot("EIM_Price", "EIM Prices (Jan-Mar)", "EIM_Prices_Jan_Mar.png")
create_plot("ICE_Price", "ICE Prices (Jan-Mar)", "ICE_Prices_Jan_Mar.png")
create_plot("PDX_Price", "PDX Prices (Jan-Mar)", "PDX_Prices_Jan_Mar.png")

