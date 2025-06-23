#load the libraries
library(dplyr)
library(readr)
library(tidyr)
#load the dataset
age_df <- read.csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/SAP2022T1T1LEA22.20250225T130218.csv")

#cleaning and transforming the data
age_df_transformed <- age_df %>%
  filter(Sex != "Both Sexes") %>%
  #creating a new variable called "age_cat" to group ages into broader categories
  mutate(
    age_cat = case_when(
      Age %in% paste("Age", 12:17) ~ "A_12-17",
      Age %in% c(
        paste("Age", 18:24),
        "Age 25-29", "Age 30-34", "Age 35-39", 
        "Age 40-44", "Age 45-49", "Age 50-54"
      ) ~ "A_18-54",
      Age %in% c("Age 55-59", "Age 60-64") ~ "A_55-64",
      Age %in% c("Age 65-69", "Age 70-74") ~ "A_65-70",
      Age %in% c("Age 75-79", "Age 80-84", "Age 85 and over") ~ "A_>71",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_cat)) %>%
  group_by(`Local.Electoral.Area`, `Census.Year`, Sex, age_cat) %>%
  
  #summarize: sum the population for each group
  summarise(popn = sum(VALUE), .groups = "drop") %>%
  #reshape the data to make each age category a separate column
  pivot_wider(
    names_from = age_cat,
    values_from = popn,
    values_fill = 0
  ) %>%
  
  #calculate total population by summing across all age groups
  mutate(
    Population = `A_12-17` + `A_18-54` + `A_55-64` + `A_65-70` + `A_>71`
  ) %>%
  select(`Local.Electoral.Area`, `Census.Year`, Sex, `A_12-17`, `A_18-54`, `A_55-64`, `A_65-70`, `A_>71`, Population)%>%
  rename(
    CSO_LEA = `Local.Electoral.Area`,
    `Census Year` = `Census.Year`
  )

print(age_df_transformed)
#write.csv(age_df_transformed, "Age_Transformed.csv", row.names = FALSE)
