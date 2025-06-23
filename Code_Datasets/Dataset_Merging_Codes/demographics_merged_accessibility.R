library(dplyr)
library(readr)
library(sf)

#demographics dataset
demographics_df <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Final_Merged_Demographics.csv")

#accessibility datasets
combined_access <- st_read("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Accessibility_Data/combined_access_values_LEA.gpkg") %>%
  st_drop_geometry() %>%
  rename(
    CSO_LEA = CSO_LEA,
    Weighted_Access_Initial_Vacc_Center = Wt_accessibility_Initial_Vacc,
    Access_Pharmacies = accessibility_Pharmacy10 
  )

access_gp <- st_read("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Accessibility_Data/access_gp.gpkg") %>%
  st_drop_geometry() %>%
  rename(
    CSO_LEA = CSO_LEA,
    Access_GPs = accessibility_10
  )

#merge both accessibility datasets with demographics
final_df <- demographics_df %>%
  left_join(combined_access, by = "CSO_LEA") %>%
  left_join(access_gp, by = "CSO_LEA")
head(final_df)
#write_csv(final_df, "demographics_merged_accessibility.csv")
