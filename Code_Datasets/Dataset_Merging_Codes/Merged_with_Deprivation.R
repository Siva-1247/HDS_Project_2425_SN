library(dplyr)
library(readr)
library(openxlsx)

#demographics + accessibility merged data
merged_df <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/demographics_merged_accessibility.csv")

# Define the URL of the Excel file
excel_url <- "https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Ireland_Deprivation_LEA22.xlsx"
# Create a temporary file path to download the Excel file
temp_xlsx <- tempfile(fileext = ".xlsx")
# Download the Excel file
download.file(excel_url, temp_xlsx, mode = "wb")
# Read the Excel file using openxlsx
deprivation <- read.xlsx(temp_xlsx)
#deprivation

deprivation_clean <- deprivation %>%
  select(
    CSO_LEA,
    Index22_rel,
    UNEMPM22,
    UNEMPF22
  )

#merge deprivation with demographics and accessibility data
merged_with_deprivation <- merged_df %>%
  left_join(deprivation_clean, by = "CSO_LEA")

head(merged_with_deprivation)
# write.csv(merged_with_deprivation, "Merged_with_Deprivation.csv", row.names = FALSE)
