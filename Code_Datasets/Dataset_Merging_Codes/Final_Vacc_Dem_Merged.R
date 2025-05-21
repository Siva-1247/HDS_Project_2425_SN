library(readr)
library(dplyr)

dem_df <- read_csv("D:/Documents/MSCHDS/RESEARCH_PROJECT/2Half/Final_Merged_Demographics.csv")
vacc_df <- read_csv("D:/Documents/MSCHDS/RESEARCH_PROJECT/VaccinationRate_Scrape_1711_24.csv")

vacc_df <- vacc_df %>%
  rename(`Local Electoral Areas` = `Local Electoral Area`)

#only the latest Month per LEA
vacc_latest <- vacc_df %>%
  filter(!is.na(Month)) %>%
  group_by(`Local Electoral Areas`) %>%
  filter(Month == max(Month)) %>%
  slice(1) %>%  # In case multiple rows have same max Month
  ungroup()

#drop the Month column
vacc_latest <- vacc_latest %>%
  select(-Month)
#drop the Census Year from dem data
dem_df <- dem_df %>%
  select(-`Census Year`)

#merging
merged_df <- vacc_latest %>%
  left_join(dem_df, by = "Local Electoral Areas")

#selecting the required columns
final_output <- merged_df %>%
  select(
    `Local Electoral Areas`,
    p_age_12to17, p_age_18to54, p_age_55to64, p_age_65to70, p_age_71plus,
    p_edu_NoFormal, p_edu_Primary, p_edu_UpperSecondary, p_edu_Apprenticeship,
    p_edu_HonoursBachelor, p_edu_Postgraduate,
    p_health_VeryBad, p_health_Bad, p_health_Fair, p_health_Good, p_health_VeryGood
  )

#check for any duplicates
any(duplicated(final_output$`Local Electoral Areas`))  # Should return FALSE

write_csv(final_output, "D:/Documents/MSCHDS/RESEARCH_PROJECT/2Half/vacc_dem_merge.csv")
