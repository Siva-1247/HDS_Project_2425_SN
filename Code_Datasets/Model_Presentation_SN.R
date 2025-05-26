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
Final_Merged_Data <- read.csv("Final_Merged_dataset.csv")
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
    health_Good_logratio = log(p_health_Good / p_health_VeryGood)
  )
library(brms)

# Fit beta regression
trial1 <- brm(
  formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio +
    edu_NoFormal_logratio + edu_Primary_logratio + edu_UpperSecondary_logratio + 
    edu_Apprenticeship_logratio + edu_HonoursBachelor_logratio +
    health_VeryBad_logratio + health_Bad_logratio + health_Fair_logratio + health_Good_logratio +
    Wt_accessibility_Initial_Vacc + accessibility_Pharmacy10 + accessibility_GP10,
  data = data,
  family = Beta())

summary(trial1)


plot(marginal_effects(trial1, effects = "age_12to17_logratio"))
plot(marginal_effects(trial1, effects = "age_65to70_logratio"))

age_vars <- c("age_12to17_logratio", "age_18to54_logratio", "age_55to64_logratio", "age_65to70_logratio")
# Get posterior fitted values
fitted_vals <- fitted(trial1, summary = TRUE)[, "Estimate"]

# Compute residuals: observed - fitted
mean_resid <- trial1$data$Primary_Vax_Rate - fitted_vals

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


edu_vars <- c("edu_NoFormal_logratio", "edu_Primary_logratio", 
              "edu_UpperSecondary_logratio", "edu_Apprenticeship_logratio", 
              "edu_HonoursBachelor_logratio")

health_vars <- c("health_VeryBad_logratio", "health_Bad_logratio", 
                 "health_Fair_logratio", "health_Good_logratio")

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
edu_df <- extract_marginal_effects(edu_vars, trial1, "Education Level")
health_df <- extract_marginal_effects(health_vars, trial1, "Health Status")

# Plot education
ggplot(edu_df, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Education Levels",
    x = "Log Ratio of Education Level",
    y = "Predicted Vaccination Rate",
    color = "Education",
    fill = "Education"
  ) +
  theme_minimal(base_size = 14)

# Plot health
ggplot(health_df, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Self-Reported Health Status",
    x = "Log Ratio of Health Status",
    y = "Predicted Vaccination Rate",
    color = "Health",
    fill = "Health"
  ) +
  theme_minimal(base_size = 14)




# formula_car <- update(formula, ~ . + car(lea_mat))
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

model_random_effects <- brm( formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio +
    edu_NoFormal_logratio + edu_Primary_logratio + edu_UpperSecondary_logratio + 
    edu_Apprenticeship_logratio + edu_HonoursBachelor_logratio +
    health_VeryBad_logratio + health_Bad_logratio + health_Fair_logratio + health_Good_logratio +
    Wt_accessibility_Initial_Vacc + accessibility_Pharmacy10 + accessibility_GP10 +
    (1 | lea_id),  # Random intercept instead of CAR
  data = data,
  family = Beta(),
  iter = 15000, warmup = 5000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.95),
  seed = 123
)
summary(trial1_CAR)
pairs(trial1_CAR)


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
  rename(Primary_Vax_Rate = `Primary.Course.Completed....`)
long_formula <- bf(Primary_Vax_Rate ~ Month_num + (Month_num | LEA_Short))

fit_long <- brm(
  formula = long_formula,
  data = long_data,
  family = inflat,
  iter = 4000,
  warmup = 2000,
  control = list(adapt_delta = 0.95)
)
install.packages("bayesplot")
library(bayesplot)
mcmc_areas(trial1_CAR_fixed, pars = "car", prob = 0.95) +  
  labs(title = "Uncertainty in Spatial Autocorrelation (CAR)")
