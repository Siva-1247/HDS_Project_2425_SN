#load the libraries
library(dplyr)
library(readr)
library(tidyr)

#load the dataset
health_df <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/SAP2022T12T3LEA22.20250225T130227.csv")

#cleaning and transforming the data
health_df_transformed <- health_df %>%
  filter(Sex != "Both Sexes", `General Health` != "Not stated") %>%
  
  #creating a new variable called "health_cat" to categorize general health data
  mutate(
    health_cat = case_when(
      `General Health` == "Very Bad" ~ "very_bad",
      `General Health` == "Bad" ~ "bad",
      `General Health` == "Fair" ~ "fair",
      `General Health` == "Good" ~ "good",
      `General Health` == "Very Good" ~ "very_good",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(health_cat)) %>% #removing missing data
  group_by(`CSO Local Electoral Areas 2022`, `Census Year`, Sex, health_cat) %>%
  
  #summarizing the data by calculating the total proportions in each group
  summarise(popn = sum(VALUE), .groups = "drop") %>%
  
  #reshape the data for having the health categories as separate columns
  pivot_wider(
    names_from = health_cat,
    values_from = popn,
    values_fill = 0
  ) %>%
  
  #calculate the total population by summing across all health categories
  mutate(
    Population = rowSums(across(c(very_bad, bad, fair, good, very_good)))
  ) %>%
  select(`CSO Local Electoral Areas 2022`, `Census Year`, Sex,
         very_bad, bad, fair, good, very_good, Population)%>%
  rename(
    `CSO_LEA` = `CSO Local Electoral Areas 2022`,
    `Census Year` = `Census Year`,
    population = Population
  )
  

print(health_df_transformed)
#write_csv(health_df_transformed, "Health_Transformed.csv")
