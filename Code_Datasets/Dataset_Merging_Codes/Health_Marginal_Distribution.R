#load libraries
library(dplyr)
library(readr)
#read the data
health_data <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Health_Transformed.csv")

#grouping by LEA and year and sum over sexes
health_margins <- health_data %>%
  group_by(`CSO_LEA`, `Census Year`) %>%
  summarise(
    very_bad = sum(very_bad, na.rm = TRUE),
    bad = sum(bad, na.rm = TRUE),
    fair = sum(fair, na.rm = TRUE),
    good = sum(good, na.rm = TRUE),
    very_good = sum(very_good, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #adding new columns for marginal distributions (proportions)
  mutate(
    p_health_very_bad = very_bad / population,
    p_health_bad = bad / population,
    p_health_fair = fair / population,
    p_health_good = good / population,
    p_health_very_good = very_good / population
  )

print(health_margins)
#write_csv(health_margins, "Health_Marginal_Distribution.csv")
