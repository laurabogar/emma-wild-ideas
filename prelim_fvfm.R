#Preliminary analysis of Emma's Fv/Fm
# July 2025


library(tidyverse)

data = read_csv("Emma's DACF - Sheet1.csv")

df_summary <- data %>%
  group_by(Date, `Plant number`) %>%
  summarise(mean_FvFm = mean(`Fv/Fm`), .groups = "drop")

# Thanks to ChatGPT for the below code!

### Visualizing change overall: ###
ggplot(df_summary, aes(x = Date, y = mean_FvFm, group = `Plant number`, color = factor(`Plant number`))) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Mean Fv/Fm", color = "Plant") +
  theme_minimal()

### Calculating Fv/Fm change for last two measurements: ###
# Check there are at least 2 dates
if (length(sorted_dates) >= 2) {
  
  # Get the last two dates
  second_latest_date <- sorted_dates[length(sorted_dates) - 1]
  latest_date <- sorted_dates[length(sorted_dates)]
  
  # Filter for those two dates and reshape to wide format
  change_df <- df_summary %>%
    filter(Date %in% c(second_latest_date, latest_date)) %>%
    pivot_wider(
      names_from = Date,
      values_from = mean_FvFm,
      names_prefix = "FvFm_"
    )
  
  # Calculate change (latest - second latest)
  change_df <- change_df %>%
    mutate(
      change = .[[paste0("FvFm_", latest_date)]] - .[[paste0("FvFm_", second_latest_date)]]
    )
  
  print(change_df)
  
} else {
  message("Not enough timepoints: need at least 2 unique dates.")
}

formean = change_df[!is.na(change_df$change),]

# Here is where you can get the change in fv/fm from second-most-recent
# to most recent measurement
mean(formean$change)

sd(formean$change)
max(formean$change)
min(formean$change)


