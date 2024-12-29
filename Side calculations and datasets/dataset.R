library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(openxlsx)
library(broom)

file_path <- "../Projects/Copy of Dataset 20240228.csv"
data <- read_csv(file_path, skip = 7)

data <- data %>%
  mutate(
    Year = gsub(",", "", Yr),
    Month = gsub(",", "", Mo),
    Day = gsub(",", "", Dy),
    Hour = gsub(",", "", Hr),
    Year = gsub(" ", "", Yr),
    Datetime = as.POSIXct(paste(Year, Month, Day, Hour, sep = "-"), format = "%Y-%m-%d-%H")
  ) %>%
  filter(!is.na(Datetime))

data <- data %>%
  mutate(
    Year = as.numeric(gsub(",", "", Year)),
    Month = as.numeric(gsub(",", "", Month)),
    Day = as.numeric(gsub(",", "", Day)),
    Hour = as.numeric(gsub(",", "", Hour)),
    `Net Market` = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", `Net Market`)))),
    Load = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", Load)))),
    Generation = as.numeric(gsub("[^0-9.-]", "", gsub("\\(", "-", gsub("\\)", "", Generation)))),
    Datetime = as.POSIXct(paste(Year, Month, Day, Hour, sep = "-"), format = "%Y-%m-%d-%H")
  ) %>%
  filter(!is.na(Datetime))

monthly_summary <- data %>%
  group_by(Month) %>%
  summarise(
    Net_Market = sum(`Net Market`, na.rm = TRUE),
    Load = sum(Load, na.rm = TRUE),
    Generation = sum(Generation, na.rm = TRUE)
  ) %>% 
  arrange(Month)

hourly_summary <- data %>%
  group_by(Hour) %>%
  summarise(
    Net_Market = sum(`Net Market`, na.rm = TRUE),
    Load = sum(Load, na.rm = TRUE),
    Generation = sum(Generation, na.rm = TRUE)
  ) %>%
  arrange(Hour)

monthly_insights <- list(
  "Month with the highest net market" = monthly_summary$Month[which.max(monthly_summary$Net_Market)],
  "Month with the lowest net market" = monthly_summary$Month[which.min(monthly_summary$Net_Market)],
  "Average load per month" = mean(monthly_summary$Load, na.rm = TRUE),
  "Average generation per month" = mean(monthly_summary$Generation, na.rm = TRUE)
)

hourly_insights <- list(
  "Hour with the highest net market" = hourly_summary$Hour[which.max(hourly_summary$Net_Market)],
  "Hour with the lowest net market" = hourly_summary$Hour[which.min(hourly_summary$Net_Market)],
  "Average load per hour" = mean(hourly_summary$Load, na.rm = TRUE),
  "Average generation per hour" = mean(hourly_summary$Generation, na.rm = TRUE)
)

# Summarize loads and prices for September for the last few years
september_data <- data %>%
  filter(Month == 9)

september_summary <- september_data %>%
  group_by(Year) %>%
  summarise(
    Total_Load = sum(Load, na.rm = TRUE),
    Total_Generation = sum(Generation, na.rm = TRUE),
    Net_Market = sum(`Net Market`, na.rm = TRUE),
    Average_Price = mean(`Net Market`, na.rm = TRUE)
  )

# Correlation analysis
correlation_results <- cor(september_summary %>% select(Total_Load, Net_Market, Average_Price), use = "complete.obs")

# Writing to Excel file
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "Monthly Summary")
addWorksheet(wb, "Hourly Summary")
addWorksheet(wb, "Insights")
addWorksheet(wb, "September Summary")
addWorksheet(wb, "Correlation Results")

# Write data to worksheets
writeData(wb, "Monthly Summary", monthly_summary)
writeData(wb, "Hourly Summary", hourly_summary)

# Write insights to the "Insights" worksheet
writeData(wb, "Insights", t(data.frame(monthly_insights)), startRow = 1, startCol = 1)
writeData(wb, "Insights", t(data.frame(hourly_insights)), startRow = 6, startCol = 1)

# Write September summary and correlation results
writeData(wb, "September Summary", september_summary)
writeData(wb, "Correlation Results", correlation_results)

# Save workbook
saveWorkbook(wb, "data_sum.xlsx", overwrite = TRUE)

print("Data summaries and insights have been saved to data_summary.xlsx")

# Plotting the data for monthly
ggplot(monthly_summary, aes(x = as.factor(Month), y = Net_Market)) +
  geom_bar(stat = "identity") +
  ggtitle("Net Market by Month") +
  xlab("Month") +
  ylab("Net Market") +
  ggsave("monthly_summary_plot.png")

# Plotting the data for hourly
ggplot(hourly_summary, aes(x = as.factor(Hour), y = Net_Market)) +
  geom_bar(stat = "identity") +
  ggtitle("Net Market by Hour") +
  xlab("Hour") +
  ylab("Net Market") +
  ggsave("hourly_summary_plot.png")

# Plotting the data for September summary
ggplot(september_summary, aes(x = as.factor(Year), y = Total_Load)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Load in September by Year") +
  xlab("Year") +
  ylab("Total Load") +
  ggsave("september_total_load_plot.png")

ggplot(september_summary, aes(x = as.factor(Year), y = Average_Price)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Price in September by Year") +
  xlab("Year") +
  ylab("Average Price") +
  ggsave("september_average_price_plot.png")

# Linear Regression Models
model1 <- lm(Net_Market ~ Total_Load, data = september_summary)
model2 <- lm(Net_Market ~ Total_Load + Total_Generation, data = september_summary)
model3 <- lm(Average_Price ~ Total_Load, data = september_summary)
model4 <- lm(Average_Price ~ Total_Load + Total_Generation, data = september_summary)

# Summary of models
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Comparing models using AIC
AIC(model1, model2, model3, model4)

# Visualizations
# Scatter plot with regression line for Net Market vs Total Load
ggplot(september_summary, aes(x = Total_Load, y = Net_Market)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  ggtitle("Net Market vs Total Load") +
  xlab("Total Load") +
  ylab("Net Market") +
  ggsave("net_market_vs_total_load.png")

# Scatter plot with regression line for Net Market vs Total Load + Total Generation
ggplot(september_summary, aes(x = Total_Load + Total_Generation, y = Net_Market)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  ggtitle("Net Market vs Total Load + Total Generation") +
  xlab("Total Load + Total Generation") +
  ylab("Net Market") +
  ggsave("net_market_vs_load_generation.png")

# Scatter plot with regression line for Average Price vs Total Load
ggplot(september_summary, aes(x = Total_Load, y = Average_Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  ggtitle("Average Price vs Total Load") +
  xlab("Total Load") +
  ylab("Average Price") +
  ggsave("average_price_vs_total_load.png")

# Scatter plot with regression line for Average Price vs Total Load + Total Generation
ggplot(september_summary, aes(x = Total_Load + Total_Generation, y = Average_Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  ggtitle("Average Price vs Total Load + Total Generation") +
  xlab("Total Load + Total Generation") +
  ylab("Average Price") +
  ggsave("average_price_vs_load_generation.png")

ggplot(september_summary, aes(x = Total_Load, y = Average_Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  ggtitle("Average Price in September vs Average Load in September") +
  xlab("Total Load in September") +
  ylab("Average Price in September") +
  ggsave("average_price_vs_average_load_september.png")


# Write summaries to Excel file
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "Monthly Summary")
addWorksheet(wb, "Hourly Summary")
addWorksheet(wb, "Insights")
addWorksheet(wb, "September Summary")
addWorksheet(wb, "Model Summaries")
addWorksheet(wb, "Correlation Results")

# Write data to worksheets
writeData(wb, "Monthly Summary", monthly_summary)
writeData(wb, "Hourly Summary", hourly_summary)

# Write insights to the "Insights" worksheet
writeData(wb, "Insights", t(data.frame(monthly_insights)), startRow = 1, startCol = 1)
writeData(wb, "Insights", t(data.frame(hourly_insights)), startRow = 6, startCol = 1)

# Write September summary and correlation results
writeData(wb, "September Summary", september_summary)
writeData(wb, "Correlation Results", correlation_results)

# Write model summaries
writeData(wb, "Model Summaries", tidy(model1), startRow = 1, startCol = 1)
writeData(wb, "Model Summaries", tidy(model2), startRow = 10, startCol = 1)
writeData(wb, "Model Summaries", tidy(model3), startRow = 20, startCol = 1)
writeData(wb, "Model Summaries", tidy(model4), startRow = 30, startCol = 1)

# Save workbook
saveWorkbook(wb, "data_sum1.xlsx", overwrite = TRUE)

print("Data summaries, insights, and model summaries have been saved to data_summary.xlsx")

print("Plots saved and displayed.")


