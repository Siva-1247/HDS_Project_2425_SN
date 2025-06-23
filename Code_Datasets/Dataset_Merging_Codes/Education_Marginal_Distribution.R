#loading libraries
library(dplyr)
library(readr)
#reading the dataset
edu_data <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Education_transformed.csv")
head(edu_data)
# summing all categories (across sex) and grouping LEA and year
edu_margins <- edu_data %>%
  group_by(`CSO_LEA`, `Census Year`) %>%
  summarise(
    No_formal_education = sum(No_formal_education, na.rm = TRUE),
    Primary_edu = sum(Primary_edu, na.rm = TRUE),
    Upper_secondary_edu = sum(Upper_secondary_edu, na.rm = TRUE),
    Advanced_cert_or_apprenticeship = sum(Advanced_cert_or_apprenticeship, na.rm = TRUE),
    Honours_bachelor = sum(Honours_bachelor, na.rm = TRUE),
    Postgraduate = sum(Postgraduate, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  #adding new columns for marginal distributions (proportions)
  mutate(
    p_edu_NoFormal = No_formal_education / population,
    p_edu_Primary = Primary_edu / population,
    p_edu_UpperSecondary = Upper_secondary_edu / population,
    p_edu_Apprenticeship = Advanced_cert_or_apprenticeship / population,
    p_edu_HonoursBachelor = Honours_bachelor / population,
    p_edu_Postgraduate = Postgraduate / population
  )

print(edu_margins)
#write_csv(edu_margins, "Education_Marginal_Distribution.csv")
