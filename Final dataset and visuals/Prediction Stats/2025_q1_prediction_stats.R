library(dplyr)
library(writexl)

file_path <- "../Documents/predicted_forward_prices_2025 final.csv"
data <- read_csv(file_path)

# Summarize data by month and hour
summary_data <- data %>%
  group_by(Month, Hour) %>%
  summarize(
    Mean_Forward_Price = mean(Forward_Price, na.rm = TRUE),
    Median_Forward_Price = median(Forward_Price, na.rm = TRUE),
    Mean_Predicted_Forward_2025 = mean(Predicted_Forward_2025, na.rm = TRUE),
    Median_Predicted_Forward_2025 = median(Predicted_Forward_2025, na.rm = TRUE),
    .groups = 'drop'  # Ungroup after summarization
  )

# Print the summary
print(summary_data)

write_xlsx(summary_data, "predicted_stats_2025_q1 final.xlsx")
