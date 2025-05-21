library(readr)
library(dplyr)
library(sf)

#load merged vacc-accessibility data
merged_df <- read_csv("D:/Documents/MSCHDS/RESEARCH_PROJECT/2Half/HDS_Project_2425_SN/Final_Merged_Accessibility.csv") %>%
  mutate(CSO_LEA = trimws(toupper(CSO_LEA)))  # Ensure column exists and is clean

#load GP access data
gp_access_df <- st_read("D:/Documents/MSCHDS/RESEARCH_PROJECT/2Half/HDS_Project_2425_SN/access_gp.gpkg") %>%
  mutate(CSO_LEA = trimws(toupper(CSO_LEA)))  # Clean for match

#merge on CSO_LEA
final_merged <- merged_df %>%
  left_join(gp_access_df, by = "CSO_LEA")

head(final_merged)
write_csv(final_merged, "D:/Documents/MSCHDS/RESEARCH_PROJECT/2Half/HDS_Project_2425_SN/Final_Merged_dataset.csv")
