# Load necessary libraries
library(dplyr)
library(readr)
library(stats)
library(tidyr)  # Ensure tidyr is loaded

# Load datasets
file_2025 <- "2025_weather_data_for_5_cities.csv"
file_history <- "3cities_doy_weather_1940_2024.csv"

df_2025 <- read_csv(file_2025)
df_history <- read_csv(file_history)

# Define climate variables for comparison
climate_variables <- c("TMAX", "TMIN", "TAVG", "PRCP", "WIND", "EVPR", "SUND",
                       "SUND5_cum", "GDD5_cum", "CDD7_cum",
                       "CODE0", "CODE1", "CODE2", "CODE3", "CODE51", 
                       "CODE53", "CODE55", "CODE61", "CODE63", "CODE65", "CODE71", 
                       "CODE73", "CODE75")

# Standardize climate variables (Z-score normalization)
normalize <- function(df, vars) {
  df[vars] <- scale(df[vars])
  return(df)
}

df_history <- normalize(df_history, climate_variables)
df_2025 <- normalize(df_2025, climate_variables)

# Find most similar historical years for each city
find_similar_years_3cities <- function(city_name, df_2025, df_history, n=3) {
  df_city_2025 <- df_2025 %>% filter(City == city_name)
  df_city_history <- df_history  %>% filter(City == city_name)
  
  # Compute Euclidean distance
  distances <- df_city_history %>%
    rowwise() %>%
    mutate(distance = sum((c_across(all_of(climate_variables)) - df_city_2025[1, climate_variables])^2, na.rm = TRUE)) %>%
    ungroup()
  
  # Select the 3 most similar years
  closest_years <- distances %>%
    arrange(distance) %>%
    slice(1:n) %>%
    select(Year, distance, City)
  
  return(closest_years)
}


find_similar_years_2other_cities <- function(city_name, df_2025, df_history, n=3) {
  df_city_2025 <- df_2025 %>% filter(City == city_name)
  df_city_history <- df_history
  
  # Compute Euclidean distance
  distances <- df_city_history %>%
    rowwise() %>%
    mutate(distance = sum((c_across(all_of(climate_variables)) - df_city_2025[1, climate_variables])^2, na.rm = TRUE)) %>%
    ungroup()
  
  # Select the 3 most similar years
  closest_years <- distances %>%
    arrange(distance) %>%
    slice(1:n) %>%
    select(Year, distance, City)
  
  return(closest_years)
}


# **Kyoto**
kyoto_similar <- find_similar_years_3cities("Kyoto", df_2025, df_history, n=3) %>%
  mutate(City_2025 = "Kyoto")
kyoto_similar
# **Liestal**
liestal_similar <- find_similar_years_3cities("Liestal", df_2025, df_history, n=3) %>%
  mutate(City_2025 = "Liestal")
liestal_similar
# **Washington**
washington_similar <- find_similar_years_3cities("Washington", df_2025, df_history, n=3) %>%
  mutate(City_2025 = "Washington")
washington_similar
# **Vancouver (Finding Similar Years from All Cities)**
vancouver_similar <- find_similar_years_2other_cities("Vancouver", df_2025, df_history, n=3) %>%
  mutate(City_2025 = "Vancouver")
vancouver_similar
# **New York City (Finding Similar Years from All Cities)**
nyc_similar <- find_similar_years_2other_cities("NewYorkCity", df_2025, df_history, n=3) %>%
  mutate(City_2025 = "NewYorkCity")
nyc_similar 
# **Combine All Results**
results <- bind_rows(kyoto_similar, liestal_similar, washington_similar, vancouver_similar, nyc_similar)
results 

# **Completion message**
# Select only the required columns for merging
df_history_selected <- df_history %>% select(Year, City, bloom_doy)

# Merge `results` with `df_history_selected` to add `bloom_doy`
results_updated <- results %>%
  left_join(df_history_selected, by = c("Year", "City"))

# Print updated results
print(results_updated)

# Compute average bloom_doy for each City_2025
results_final <- results_updated %>%
  group_by(City_2025) %>%
  summarise(Average_Bloom_DOY = round(mean(bloom_doy, na.rm = TRUE), 2)) %>% 
  ungroup()

# Print the final table
print(results_final)

# Save results as CSV file
output_file <- "5cities_Average_Bloom_DOY.csv"
write.csv(results_final, output_file, row.names = FALSE)

# Completion message
cat("Final results saved to:", output_file, "\n")
