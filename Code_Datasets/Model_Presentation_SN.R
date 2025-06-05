library(mapview)
library(spdep)
library(sf)
library(dplyr)

gfile <- "Accessibility_Data\\CSO_Local_Electoral_Areas_National_Statistical_Boundaries_2022_Generalised_100m_-6420530397479472898.geojson"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE))
geo_data <- geo_data %>% mutate(CSO_LEA=case_when(CSO_LEA == "ATHLONE" & COUNTY == "WESTMEATH" ~ "ATHLONE_WESTMEATH",TRUE ~ CSO_LEA))
lea_mat <- nb2mat(poly2nb(geo_data), style = "B")
lea_mat[1:10, 1:10]
any(lea_mat != 0)


vax_data <- read.csv("Vacc_Rates&Geocoded_Data/CDC47_Stats.csv")
Final_Merged_Data <- read.csv("C:/Users/Sivagami Nedumaran/Downloads/Final_Merged_dataset.csv")
vax_data$LEA_Short <- toupper(sapply(strsplit(as.character(vax_data$Local.Electoral.Area), ","), `[`, 1))
vax_data$LEA_Short <- gsub("GRAIGUECULLEN -PORTARLINGTON", 
                           "GRAIGUECULLEN-PORTARLINGTON", 
                           vax_data$LEA_Short)
tail(vax_data)
vax_data$LEA_Short[grepl("Athlone, Westmeath", vax_data$Local.Electoral.Area, ignore.case = TRUE)] <- "ATHLONE_WESTMEATH"
# Filter rows with final vaccination rate
filtered_data <- vax_data %>%
  filter(Month == "2023 June", Age.Group == "12 years and over") %>%
  select(LEA_Short, `Primary.Course.Completed....`)

filtered_data <- filtered_data %>%
  rename(Primary_Vax_Rate = `Primary.Course.Completed....`)
dim(filtered_data)
Final_Data <-  merge(Final_Merged_Data, filtered_data, 
                               by.x = "CSO_LEA",        
                               by.y = "LEA_Short",
                               all.x = FALSE,          
                               all.y = FALSE)
dim(Final_Data)
head(Final_Data)
Final_Data <- Final_Data %>%
  rename(accessibility_GP10 = `accessibility_10`)

names(Final_Data)
write.csv(Final_Data, "Vacc_Rates&Geocoded_Data/Final_Data.csv", row.names = FALSE)
Final_Data <- read.csv("Vacc_Rates&Geocoded_Data/Final_Data.csv")
data <- Final_Data
data$Primary_Vax_Rate <- data$Primary_Vax_Rate / 100
d <- Final_Data
# Calculate row sums for age proportions
d$age_edu <- rowSums(d %>% select(starts_with("p_edu_")))

d %>% filter(abs(age_edu - 1) > 0.01)


# Apply Additive log-ratio transformation proportion column
data <- data %>%
  mutate(
    # Age groups (reference: p_age_71plus)
    age_12to17_logratio = log(p_age_12to17 / p_age_71plus),
    age_18to54_logratio = log(p_age_18to54 / p_age_71plus),
    age_55to64_logratio = log(p_age_55to64 / p_age_71plus),
    age_65to70_logratio = log(p_age_65to70 / p_age_71plus),
    
    # Education (reference: p_edu_Postgraduate)
    edu_NoFormal_logratio = log(p_edu_NoFormal / p_edu_Postgraduate),
    edu_Primary_logratio = log(p_edu_Primary / p_edu_Postgraduate),
    edu_UpperSecondary_logratio = log(p_edu_UpperSecondary / p_edu_Postgraduate),
    edu_Apprenticeship_logratio = log(p_edu_Apprenticeship / p_edu_Postgraduate),
    edu_HonoursBachelor_logratio = log(p_edu_HonoursBachelor / p_edu_Postgraduate),
    
    # Health (reference: p_health_VeryGood)
    health_VeryBad_logratio = log(p_health_VeryBad / p_health_VeryGood),
    health_Bad_logratio = log(p_health_Bad / p_health_VeryGood),
    health_Fair_logratio = log(p_health_Fair / p_health_VeryGood),
    health_Good_logratio = log(p_health_Good / p_health_VeryGood),
    
    #gender (reference: p_female)
    male_logratio = log(p_male / p_female)
    
  )


library(brms)

# Fit beta regression - Age & Sex
trial1 <- brm(
  formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio + male_logratio,
  data = data,
  family = Beta())

summary(trial1)


plot(marginal_effects(trial1, effects = "age_12to17_logratio"))
plot(marginal_effects(trial1, effects = "age_65to70_logratio"))

age_vars <- c("age_12to17_logratio", "age_18to54_logratio", "age_55to64_logratio", "age_65to70_logratio")
sex_vars <- c("male_logratio")
# Get posterior fitted values
fitted_vals <- fitted(trial2, summary = TRUE)[, "Estimate"]

# Compute residuals: observed - fitted
mean_resid <- trial2$data$Primary_Vax_Rate - fitted_vals

resid_df <- data.frame(CSO_LEA = data$CSO_LEA, resid = mean_resid)

# Merge with geo_data using CSO_LEA
sac_data <- geo_data %>%
  left_join(resid_df, by = "CSO_LEA")
neighbors <- poly2nb(sac_data)
weights <- nb2listw(neighbors, style = "W")
moran.test(sac_data$resid, weights)

#Spatial AutoCorrelation Plot
ggplot(sac_data) +
  geom_sf(aes(fill = resid), color = "grey30", size = 0.2) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, 
    name = "Residuals"
  ) +
  theme_minimal() +
  labs(
    title = "Spatial Distribution of Residuals",
    subtitle = "Residuals from Beta Regression of Vaccination Rate",
    caption = "Blue = underprediction, Red = overprediction"
  )


me_age <- marginal_effects(
  trial1,
  effects = c("age_12to17_logratio", "age_18to54_logratio", "age_55to64_logratio", "age_65to70_logratio"),  
  re_formula = NA,       # exclude group-level effects
  prob = c(0.05, 0.95)  # 90% CI
)

me_df <- bind_rows(
  me_age$age_12to17_logratio %>% rename(logratio = age_12to17_logratio) %>% mutate(variable = "Age 12–17"),
  me_age$age_18to54_logratio %>% rename(logratio = age_18to54_logratio) %>% mutate(variable = "Age 18–54"),
  me_age$age_55to64_logratio %>% rename(logratio = age_55to64_logratio) %>% mutate(variable = "Age 55–64"),
  me_age$age_65to70_logratio %>% rename(logratio = age_65to70_logratio) %>% mutate(variable = "Age 65–70")
)

# Plot
ggplot(me_df, aes(x = logratio, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    x = "Log Ratio",
    y = "Predicted Primary Vaccination Rate",
    color = "Age Group",
    fill = "Age Group"
  ) +
  theme_minimal(base_size = 14)

trial3 <- brm(
  formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio + edu_NoFormal_logratio + edu_Primary_logratio + edu_UpperSecondary_logratio + edu_Apprenticeship_logratio + edu_HonoursBachelor_logratio + health_VeryBad_logratio + health_Bad_logratio + health_Fair_logratio + health_Good_logratio + male_logratio +Wt_accessibility_Initial_Vacc ,
  data = data,
  family = Beta())

trial2 <- brm(
  formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio + edu_NoFormal_logratio + edu_Primary_logratio + edu_UpperSecondary_logratio + edu_Apprenticeship_logratio + edu_HonoursBachelor_logratio + health_VeryBad_logratio + health_Bad_logratio + health_Fair_logratio + health_Good_logratio + male_logratio +Wt_accessibility_Initial_Vacc + accessibility_Pharmacy10 + accessibility_GP10 ,
  data = data,
  family = Beta())


edu_vars <- c("edu_NoFormal_logratio", "edu_Primary_logratio", 
              "edu_UpperSecondary_logratio", "edu_Apprenticeship_logratio", 
              "edu_HonoursBachelor_logratio")

health_vars <- c("health_VeryBad_logratio", "health_Bad_logratio", 
                 "health_Fair_logratio", "health_Good_logratio")

access_vars <- c("Wt_accessibility_Initial_Vacc")

# Helper function to extract and label marginal effects
extract_marginal_effects <- function(var_list, model, label) {
  lapply(var_list, function(var) {
    me <- marginal_effects(model, effects = var)[[1]]
    names(me)[1] <- "x_val"  # Rename the x-axis variable for consistency
    me$variable <- var
    me$group <- label
    me
  }) %>% bind_rows()
}

# Extract marginal effects for each group
age_df_0 <- extract_marginal_effects(age_vars, trial1, "Age")
sex_df_0 <- extract_marginal_effects(sex_vars, trial1, "Sex")
age_df <- extract_marginal_effects(age_vars, trial2, "Age")
edu_df <- extract_marginal_effects(edu_vars, trial2, "Education Level")
health_df <- extract_marginal_effects(health_vars, trial2, "Health Status")
sex_df <- extract_marginal_effects(sex_vars, trial2, "Sex")
access_df <- extract_marginal_effects(access_vars, trial2, "Accessibility")

# Plot Age
p_0age <- ggplot(age_df_0, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Age",
    x = "Additive Log-Ratio of Age (baseline: age 71+)",
    y = "Predicted Vaccination Rate",
    color = "Age",
    fill = "Age"
  ) +
  theme_minimal(base_size = 14)

p_age <- ggplot(age_df, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Age",
    x = "Additive Log-Ratio of Age (baseline: age 71+)",
    y = "Predicted Vaccination Rate",
    color = "Age",
    fill = "Age"
  ) +
  theme_minimal(base_size = 14)



# Plot education
p_edu <- ggplot(edu_df, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Education Levels",
    x = "Additive Log-Ratio of Education Level (baseline: Postgraduate)",
    y = "Predicted Vaccination Rate",
    color = "Education",
    fill = "Education"
  ) +
  theme_minimal(base_size = 14)

# Plot health
p_health  <- ggplot(health_df, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Self-Reported Health Status",
    x = "Additive Log-Ratio of Education Level (baseline: Very_Good)",
    y = "Predicted Vaccination Rate",
    color = "Health",
    fill = "Health"
  ) +
  theme_minimal(base_size = 14)

p_access <- ggplot(access_df, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Access",
    x = "Accessibility Score",
    y = "Predicted Vaccination Rate",
    color = "Accessibility",
    fill = "Accessibility"
  ) +
  theme_minimal(base_size = 14)

p_sex <- ggplot(sex_df, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Sex",
    x = "Additive Log-Ratio of Male (baseline: Female)",
    y = "Predicted Vaccination Rate",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 14)

p_0sex <- ggplot(sex_df_0, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Sex",
    x = "Additive Log-Ratio of Male (baseline: Female)",
    y = "Predicted Vaccination Rate",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 14)

ggsave("marginal_effects_health.png", plot = p_health, width = 8, height = 6, dpi = 300)
ggsave("marginal_effects_education.png", plot = p_edu, width = 8, height = 6, dpi = 300)
ggsave("marginal_effects_age.png", plot = p_age, width = 8, height = 6, dpi = 300)
ggsave("marginal_effects_age0.png", plot = p_0age, width = 8, height = 6, dpi = 300)
ggsave("marginal_effects_access.png", plot = p_access, width = 8, height = 6, dpi = 300)
ggsave("marginal_effects_sex.png", plot = p_sex, width = 8, height = 6, dpi = 300)
ggsave("marginal_effects_sex0.png", plot = p_0sex, width = 8, height = 6, dpi = 300)



data$lea_id <- factor(1:nrow(data))
trial1_CAR_fixed <- brm(
  formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio +
    edu_NoFormal_logratio + edu_Primary_logratio + edu_UpperSecondary_logratio + 
    edu_Apprenticeship_logratio + edu_HonoursBachelor_logratio +
    health_VeryBad_logratio + health_Bad_logratio + health_Fair_logratio + health_Good_logratio +
    Wt_accessibility_Initial_Vacc + accessibility_Pharmacy10 + accessibility_GP10 + 
    car(lea_mat, gr = lea_id),
  data = data,
  data2 = list(lea_mat = lea_mat),
  family = Beta(),
  # INCREASED CONVERGENCE PARAMETERS
  iter = 30000,           
  warmup = 10000,         
  chains = 4,             
  cores = 4,
  control = list(
    adapt_delta = 0.999,  
    max_treedepth = 20,   
    stepsize = 0.01       
  )
) 
install.packages("bayesplot")
library(bayesplot)
mcmc_areas(trial1_CAR_fixed, pars = "car", prob = 0.95) +  
  labs(title = "Uncertainty in Spatial Autocorrelation (CAR)")

#Temporal Model
long_data <- vax_data %>%
  filter(Age.Group == "12 years and over") %>%
  select(LEA_Short, `Primary.Course.Completed....`, Month)

long_data$Month <- as.Date(paste0(long_data$Month, "-01"), format = "%Y %B-%d")
min_date <- min(long_data$Month, na.rm = TRUE)

# Create numeric month index starting at 1 for earliest month
long_data$Month_num <- as.numeric(format(long_data$Month, "%m")) + 
  12 * (as.numeric(format(long_data$Month, "%Y")) - as.numeric(format(min_date, "%Y")))
long_data$Month_num <- long_data$Month_num - min(long_data$Month_num) + 1
long_data <- long_data %>%
  rename
# Transform Primary_Vax_Rate to proportion
long_data <- long_data %>%
  rename(Primary_Vax_Rate = `Primary.Course.Completed....`)
long_data$Primary_Vax_Rate <- long_data$Primary_Vax_Rate/100
# Define the model formula
nl_formula <- bf(
  Primary_Vax_Rate ~ Asym / (1 + exp((xmid - Month_num) / scal)),
  Asym ~ 1 + (1 | LEA_Short),
  xmid ~ 1 + (1 | LEA_Short),
  scal ~ 1,
  nl = TRUE,
  family = Beta()
)

# Define priors with explicit lower bound for scal
nl_prior <- c(
  prior(normal(0.8, 0.1), nlpar = "Asym"),
  prior(normal(6, 2), nlpar = "xmid"),
  prior(exponential(1), nlpar = "scal", lb = 0)
)

# Fit the model
nl_model <- brm(
  nl_formula,
  data = long_data,
  prior = nl_prior,
  chains = 4,
  iter = 4000,
  control = list(adapt_delta = 0.95)
)

conditional_effects(nl_model, effects = "Month_num") %>% 
  plot(points = TRUE)

nl_noprior_model <- brm(
  nl_formula,
  data = long_data,
  chains = 4,
  iter = 30000,
  control = list(adapt_delta = 0.95)
)
library(tidybayes)

lea_effects <- ranef(nl_model)$LEA_Short %>%
  as_tibble(rownames = "LEA_Short") %>%
  arrange(desc(Estimate.Asym_Intercept))

top_leas <- head(lea_effects$LEA_Short, 5)
bottom_leas <- tail(lea_effects$LEA_Short, 5)

# Subset data
plot_data <- long_data %>%
  filter(LEA_Short %in% c(top_leas, bottom_leas)) %>%
  add_epred_draws(nl_model, ndraws = 50)

plot_data <- plot_data %>%
  mutate(
    group = case_when(
      LEA_Short %in% top_leas ~ "Top 5 Uptake",
      LEA_Short %in% bottom_leas ~ "Bottom 5 Uptake"
    )
  )
library(ggdist)

bottom_data <- plot_data %>% filter(LEA_Short %in% bottom_leas)

bottom_plot <- ggplot(bottom_data, aes(x = Month_num, y = .epred)) +
  stat_lineribbon(
    aes(group = .draw), 
    alpha = 0.05, fill = "#d7191c", color = "#d7191c", linewidth = 0.2
  ) +
  geom_point(
    aes(y = Primary_Vax_Rate), 
    size = 1.5, color = "#fdae61", alpha = 0.7
  ) +
  facet_wrap(~LEA_Short, nrow = 1) +
  labs(
    title = "Vaccination Growth in Bottom 5 LEAs (Lowest Uptake)",
    x = "Month Since Rollout",
    y = "Vaccination Rate"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

top_data <- plot_data %>% filter(LEA_Short %in% top_leas)

top_plot <- ggplot(top_data, aes(x = Month_num, y = .epred)) +
  stat_lineribbon(
    aes(group = .draw), 
    alpha = 0.05, fill = "#2b83ba", color = "#2b83ba", linewidth = 0.2
  ) +
  geom_point(
    aes(y = Primary_Vax_Rate), 
    size = 1.5, color = "#abdda4", alpha = 0.7
  ) +
  facet_wrap(~LEA_Short, nrow = 1) +
  labs(
    title = "Vaccination Growth in Top 5 LEAs (Highest Uptake)",
    x = "Month Since Rollout",
    y = "Vaccination Rate"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )
top_plot 
#############Depriviation############
dep_data <- read.csv("Complete_Merged_Dataset.csv")
names(dep_data)
data$Depriviation <- dep_data$Index22_rel

trial4 <- brm(
  formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio + edu_NoFormal_logratio + edu_Primary_logratio + edu_UpperSecondary_logratio + edu_Apprenticeship_logratio + edu_HonoursBachelor_logratio + health_VeryBad_logratio + health_Bad_logratio + health_Fair_logratio + health_Good_logratio + male_logratio +Wt_accessibility_Initial_Vacc + Depriviation,
  data = data,
  family = Beta())

dep_vars <- c("Depriviation")
age_dep <- extract_marginal_effects(age_vars, trial4, "Age")
dep_df <- extract_marginal_effects(dep_vars, trial4, "Depriviation")

# Plot Age
ggplot(age_dep, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Age",
    x = "Additive Log-Ratio of Age (baseline: age 71+)",
    y = "Predicted Vaccination Rate",
    color = "Age",
    fill = "Age"
  ) +
  theme_minimal(base_size = 14)

# Plot Depriviation
ggplot(dep_df, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Depriviation",
    x = "Depriviation Data",
    y = "Predicted Vaccination Rate",
    color = "Depriviation",
    fill = "Depriviation"
  ) +
  theme_minimal(base_size = 14)

###Joint Distributuion############
data_1 <- data %>% select(CSO_LEA, Primary_Vax_Rate)
dep_subset <- dep_data %>%
  select(CSO_LEA,
         p_age_12to17_females,
         p_age_12to17_males,
         p_age_18to54_females,
         p_age_18to54_males,
         p_age_55to64_females,
         p_age_55to64_males,
         p_age_65to70_females,
         p_age_65to70_males,
         p_age_71plus_females,
         p_age_71plus_males)

merged_data <- data_1 %>%
  left_join(dep_subset, by = "CSO_LEA")

# ALR transform for female age groups
merged_data <- merged_data %>%
  mutate(
    alr_age_12to17_females = log(p_age_12to17_females / p_age_71plus_females),
    alr_age_18to54_females = log(p_age_18to54_females / p_age_71plus_females),
    alr_age_55to64_females = log(p_age_55to64_females / p_age_71plus_females),
    alr_age_65to70_females = log(p_age_65to70_females / p_age_71plus_females)
  )

# ALR transform for male age groups
merged_data <- merged_data %>%
  mutate(
    alr_age_12to17_males = log(p_age_12to17_males / p_age_71plus_males),
    alr_age_18to54_males = log(p_age_18to54_males / p_age_71plus_males),
    alr_age_55to64_males = log(p_age_55to64_males / p_age_71plus_males),
    alr_age_65to70_males = log(p_age_65to70_males / p_age_71plus_males)
  )

trial5 <- brm(
  formula = Primary_Vax_Rate ~ 
    alr_age_12to17_females + 
    alr_age_18to54_females + 
    alr_age_55to64_females + 
    alr_age_65to70_females + 
    alr_age_12to17_males + 
    alr_age_18to54_males + 
    alr_age_55to64_males + 
    alr_age_65to70_males,
  data = merged_data,
  family = Beta()
)

age_female_vars <- c("alr_age_12to17_females","alr_age_18to54_females","alr_age_55to64_females","alr_age_65to70_females")
age_male_vars <- c("alr_age_12to17_males", "alr_age_18to54_males", "alr_age_55to64_males", "alr_age_65to70_males")


age_female <- extract_marginal_effects(age_female_vars, trial5, "AgeXFemale")

# Plot Age
ggplot(age_female, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: AgeXFemale",
    x = "Additive Log-Ratio of Female Age (baseline: age 71+)",
    y = "Predicted Vaccination Rate",
    color = "AgeXFemale",
    fill = "AgeXFemale"
  ) +
  theme_minimal(base_size = 14)

age_male <- extract_marginal_effects(age_male_vars, trial5, "AgeXMale")

# Plot Age
ggplot(age_male, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: AgeXMale",
    x = "Additive Log-Ratio of Male Age (baseline: age 71+)",
    y = "Predicted Vaccination Rate",
    color = "AgeXMale",
    fill = "AgeXMale"
  ) +
  theme_minimal(base_size = 14)
######Vaccination Rate Map
geo_data
filtered_data <- vax_data %>%
  filter(Month == "2023 June", Age.Group == "12 years and over") %>%
  select(LEA_Short, `Primary.Course.Completed....`)
vaccination_centers <- read.csv("geocoded_addresses_vac_final.csv")
plot_data <-  merge(geo_data, filtered_data, 
                                  by.x = "CSO_LEA",        
                                  by.y = "LEA_Short",
                                  all.x = FALSE,          
                                  all.y = FALSE)
plot_data$Primary_Vax_Rate <- plot_data$Primary_Vax_Rate / 100
color_palette <- colorBin(
  palette = "RdYlGn",
  domain = plot_data$Primary_Vax_Rate,
  bins = 5, 
  reverse = FALSE
)

# Step 5: Create a leaflet map
leaflet(plot_data) %>%
  addTiles("Stamen.Watercolor") %>%  
  addPolygons(
    fillColor = ~color_palette(Primary_Vax_Rate),  # Apply color palette
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste0(CSO_LEA, ": ", Primary_Vax_Rate),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = color_palette, 
    values = ~Primary_Vax_Rate, 
    opacity = 0.7, 
    title = "Final Primary Dose Vaccination Proportion",
    position = "bottomright"
  )

ggplot(data, aes(x = age_55to64_logratio, y = log(Primary_Vax_Rate / (1 - Primary_Vax_Rate)))) +
  geom_point() + geom_smooth()
library(DHARMa)
sims <- simulate(trial2, nsim = 1000)
res <- createDHARMa(sims, data$Primary_Vax_Rate)
plot(res)  # Check for uniformity, outliers.


fitted_vals <- fitted(trial1_CAR_fixed, summary = TRUE)[, "Estimate"]
# Plot logit-transformed rates vs. key predictorr

# Compute residuals: observed - fitted
mean_resid <- trial1_CAR_fixed$data$Primary_Vax_Rate - fitted_vals

resid_df <- data.frame(CSO_LEA = data$CSO_LEA, resid = mean_resid)

# Merge with geo_data using CSO_LEA
sac_data <- geo_data %>%
  left_join(resid_df, by = "CSO_LEA")
neighbors <- poly2nb(sac_data)
weights <- nb2listw(neighbors, style = "W")
moran.test(sac_data$resid, weights)