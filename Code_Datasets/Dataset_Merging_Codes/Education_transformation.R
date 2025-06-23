#load libraries
library(dplyr)
library(tidyr)
#read the dataset
edu_df <- read.csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/SAP2022T10T4LEA22.20250310T100312.csv")

#transform the data
edu_df_transformed <- edu_df %>%
  
  #filter to remove the rows where the Sex is "Both Sexes". We're keeping only Male and Female
  filter(Sex != "Both Sexes") %>%
  mutate(
    #new variable to categorize education levels
    education_cat = case_when(
      `Highest.Level.of.Education.Completed` == "No formal education" ~ "No_formal_education",
      `Highest.Level.of.Education.Completed` == "Primary education" ~ "Primary_edu",
      `Highest.Level.of.Education.Completed` == "Upper secondary" ~ "Upper_secondary_edu",
      `Highest.Level.of.Education.Completed` == "Technical or vocational qualification" ~ "Advanced_cert_or_apprenticeship",
      `Highest.Level.of.Education.Completed` == "Honours bachelor degree, professional qualification or both" ~ "Honours_bachelor",
      `Highest.Level.of.Education.Completed` == "Postgraduate diploma or degree" ~ "Postgraduate",
      TRUE ~ NA_character_
    )
  ) %>%
  
  #filter to remove rows where NA - education_cat is missing
  filter(!is.na(education_cat)) %>%
  
  #grouping the data by LEA, year, sex and education category
  group_by(`CSO.Local.Electoral.Areas.2022`, `Census.Year`, Sex, education_cat) %>%
  
  #summarize the grouped data by summing the population counts (VALUE)
  summarise(popn = sum(VALUE), .groups = "drop") %>%
  
  #reshaping the data (pivoting wider) to make each education category as a separate column
  pivot_wider(
    names_from = education_cat,
    values_from = popn,
    values_fill = 0 #fill missing areas with 0
  ) %>%
  
  #ensure that all possible columns exist
  mutate(
    No_formal_education = coalesce(No_formal_education, 0),
    Primary_edu = coalesce(Primary_edu, 0),
    Upper_secondary_edu = coalesce(Upper_secondary_edu, 0),
    Advanced_cert_or_apprenticeship = coalesce(Advanced_cert_or_apprenticeship, 0),
    Honours_bachelor = coalesce(Honours_bachelor, 0),
    Postgraduate = coalesce(Postgraduate, 0),
    
    #calculating total population across all possible education variable
    Population = rowSums(across(c(No_formal_education, Primary_edu, Upper_secondary_edu,
                                  Advanced_cert_or_apprenticeship, Honours_bachelor, Postgraduate)))
  ) %>%
  #ordering the columns by selecting
  select(`CSO.Local.Electoral.Areas.2022`, `Census.Year`, Sex,
         No_formal_education, Primary_edu, Upper_secondary_edu,
         Advanced_cert_or_apprenticeship, Honours_bachelor, Postgraduate, Population)%>%
  rename(
    `CSO_LEA` = `CSO.Local.Electoral.Areas.2022`,
    `Census Year` = `Census.Year`,
    population = Population
  )

print(edu_df_transformed)
names(edu_df_transformed)
#calling the readr library and saving the file
#readr::write_csv(edu_df_transformed, "Education_transformed.csv")


