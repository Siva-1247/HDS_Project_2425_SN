library(readr)
library(dplyr)

age_df <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Age_Marginal_Distribution.csv")
edu_df <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Education_Marginal_Distribution.csv")
health_df <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Health_Marginal_Distribution.csv")

#rename HEALTH variables
health_df <- health_df %>%
  rename(
    p_health_VeryBad = p_health_very_bad,
    p_health_Bad = p_health_bad,
    p_health_Fair = p_health_fair,
    p_health_Good = p_health_good,
    p_health_VeryGood = p_health_very_good
  )

#merge datasets on LEAs
merged_dem_df <- age_df %>%
  inner_join(edu_df, by = c("CSO_LEA")) %>%
  inner_join(health_df, by = c("CSO_LEA"))
final_dem_df <- merged_dem_df %>%
  select(
    `CSO_LEA`,
    p_age_12to17, p_age_18to54, p_age_55to64, p_age_65to70, p_age_71plus,
    p_edu_NoFormal, p_edu_Primary, p_edu_UpperSecondary, p_edu_Apprenticeship,
    p_edu_HonoursBachelor, p_edu_Postgraduate,
    p_health_VeryBad, p_health_Bad, p_health_Fair, p_health_Good, p_health_VeryGood
  )

head(final_dem_df)
#write_csv(final_dem_df, "Final_Merged_Demographics.csv")
