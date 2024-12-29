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


hourly_summary <- data %>%
  group_by(Year, Month, Hour) %>%
  summarise(
    Forward_Price = mean(Forward_Price, na.rm = TRUE),
    EIM_Price = mean(EIM_Price, na.rm = TRUE),
    ICE_Price = mean(ICE_Price, na.rm = TRUE),
    PDX_Price = mean(PDX_Price, na.rm = TRUE)
  ) %>%
  ungroup()

# Combine PDX_Price and Forward_Price into a single dataset
combined_data <- hourly_summary %>%
  pivot_longer(cols = c("PDX_Price", "Forward_Price", "ICE_Price"),
               names_to = "Price_Type",
               values_to = "Price") %>%
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

# Plot the combined data


create_plot <- function(combined_data, title, file_name) {
  ggplot(combined_data, aes(x = Month_Hour, y = Price, color = factor(Year), linetype = Price_Type, group = interaction(Year, Price_Type))) +
    geom_line() +
    labs(title = title, x = "Month-Hour", y = "Price", color = "Year", linetype = "Price Type") +
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
create_plot(combined_data, "PDX and Forward and Day Ahead (Jan-Mar)", "PDXwithForwardwithDayAhead Final.png")


