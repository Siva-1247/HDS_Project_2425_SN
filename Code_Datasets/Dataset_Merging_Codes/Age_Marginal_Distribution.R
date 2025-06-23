#load required libraries
library(dplyr)
library(readr)
#read the dataset
age_df <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Age_Transformed.csv")

#summarise by LEA and year, aggregating both sexes
age_df_margins <- age_df %>%
  group_by(`CSO_LEA`, `Census Year`) %>%
  summarise(
    p_age_12to17 = sum(`A_12-17`),
    p_age_18to54 = sum(`A_18-54`),
    p_age_55to64 = sum(`A_55-64`),
    p_age_65to70 = sum(`A_65-70`),
    p_age_71plus = sum(`A_>71`),
    Population = sum(Population),
    .groups = "drop"
  ) %>%
  #adding new columns for marginal distributions (proportions)
  mutate(
    p_age_12to17 = p_age_12to17 / Population,
    p_age_18to54 = p_age_18to54 / Population,
    p_age_55to64 = p_age_55to64 / Population,
    p_age_65to70 = p_age_65to70 / Population,
    p_age_71plus = p_age_71plus / Population
  )

print(age_df_margins)
#write.csv(age_df_margins, "Age_Marginal_Distribution.csv", row.names = FALSE)
