library(dplyr)
library(readr)

#load merged vaccination and demographics data
vacc_dem_df <- read_csv("D:/Documents/MSCHDS/RESEARCH_PROJECT/2Half/HDS_Project_2425_SN/Final_Vacc_Dem_Merged.csv") %>%
  rename(LEA = `Local Electoral Areas`) %>%
  mutate(LEA = trimws(LEA))

gpkg_lea_df <- read_csv("D:/Documents/MSCHDS/RESEARCH_PROJECT/CSO_LEA_from_gpkg.csv") %>%
  mutate(CSO_LEA = trimws(CSO_LEA))

#create a candidate mapping table
mapping_df <- vacc_dem_df %>%
  distinct(LEA) %>%
  mutate(CSO_LEA = gpkg_lea_df$CSO_LEA[1:n()])

head(mapping_df)
write_csv(mapping_df, "D:/Documents/MSCHDS/RESEARCH_PROJECT/LEA_mapping_to_edit.csv")

#load the mapped data
mapping_fixed <- read_csv("D:/Documents/MSCHDS/RESEARCH_PROJECT/LEA_mapping_to_edit.csv") %>%
  mutate(LEA = trimws(LEA), CSO_LEA = trimws(CSO_LEA))

#load the merged accessibility data
library(sf)
accessibility_df <- st_read("D:/Documents/MSCHDS/RESEARCH_PROJECT/2Half/HDS_Project_2425_SN/combined_access_values_LEA.gpkg") %>%
  st_drop_geometry() %>%
  rename(CSO_LEA = CSO_LEA) %>%
  mutate(CSO_LEA = trimws(CSO_LEA))

#merge using mapping
vacc_dem_mapped <- vacc_dem_df %>%
  left_join(mapping_fixed, by = "LEA")
final_merged <- vacc_dem_mapped %>%
  left_join(accessibility_df, by = "CSO_LEA")

head(final_merged)
write_csv(final_merged, "D:/Documents/MSCHDS/RESEARCH_PROJECT/Final_Merged_Accessibility_FIXED.csv")
