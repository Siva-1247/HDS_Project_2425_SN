library(dplyr)
library(readr)
library(tidyr)

#Age Transformed data
age_df <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Age_Transformed.csv")
#recode Sex to singular lowercase
age_df <- age_df %>%
  mutate(Sex = recode(Sex, "Males" = "male", "Females" = "female"))
#calculate total population per LEA
total_pop <- age_df %>%
  group_by(CSO_LEA) %>%
  summarise(Total_Pop = sum(Population), .groups = "drop")

#compute age group proportions per LEA and Sex
age_joint <- age_df %>%
  left_join(total_pop, by = "CSO_LEA") %>%
  mutate(
    p_age_12to17 = `A_12-17` / Total_Pop,
    p_age_18to54 = `A_18-54` / Total_Pop,
    p_age_55to64 = `A_55-64` / Total_Pop,
    p_age_65to70 = `A_65-70` / Total_Pop,
    p_age_71plus = `A_>71` / Total_Pop
  ) %>%
  select(CSO_LEA, Sex, p_age_12to17, p_age_18to54, p_age_55to64, p_age_65to70, p_age_71plus)

#pPivot to wide format (so that column names will be "_sex_male"/"_sex_female")
age_wide <- age_joint %>%
  pivot_wider(
    names_from = Sex,
    values_from = starts_with("p_age"),
    names_glue = "{.value}_sex_{Sex}"
  )

#compute proportion of male/female population per LEA
sex_dist <- age_df %>%
  group_by(CSO_LEA, Sex) %>%
  summarise(Sex_Pop = sum(Population), .groups = "drop") %>%
  left_join(total_pop, by = "CSO_LEA") %>%
  mutate(Proportion = Sex_Pop / Total_Pop) %>%
  select(CSO_LEA, Sex, Proportion) %>%
  pivot_wider(
    names_from = Sex,
    values_from = Proportion,
    names_glue = "p_sex_{Sex}"
  )
#main demographic+accessiblity+deprivation merged dataset
main_df <- read_csv("https://raw.githubusercontent.com/Siva-1247/HDS_Project_2425_SN/main/Code_Datasets/Dataset_Merging_Codes/Merged_with_Deprivation.csv") %>%
  mutate(CSO_LEA = as.character(CSO_LEA))
#final merge
final_df <- main_df %>%
  left_join(age_wide, by = "CSO_LEA") %>%
  left_join(sex_dist, by = "CSO_LEA")

names(final_df)
head(final_df)
#write_csv(final_df, "Complete_Merged_Dataset.csv")

