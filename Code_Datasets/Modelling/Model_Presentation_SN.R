library(mapview)
library(spdep)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)

#Read Vaccination data and LEA polygons

gfile <- "Accessibility_Data\\CSO_Local_Electoral_Areas_National_Statistical_Boundaries_2022_Generalised_100m_-6420530397479472898.geojson"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE))
geo_data <- geo_data %>% mutate(CSO_LEA=case_when(CSO_LEA == "ATHLONE" & COUNTY == "WESTMEATH" ~ "ATHLONE_WESTMEATH",TRUE ~ CSO_LEA))
lea_mat <- nb2mat(poly2nb(geo_data), style = "B")
lea_mat[1:10, 1:10]
any(lea_mat != 0)
Vax_data <- read.csv("Vacc_Rates&Geocoded_Data/CDC47_Stats.csv")

Final_Merged_Data <- read.csv("Final_Merged_Dataset/Final_Merged_dataset.csv")
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

#Save data
write.csv(Final_Data, "Vacc_Rates&Geocoded_Data/Final_Data.csv", row.names = FALSE)

#Reading final Data - final primary vaccination rate and demographic data
Final_Data <- read.csv("Vacc_Rates&Geocoded_Data/Final_Data.csv")

#Proportional data handling
data <- Final_Data
data$Primary_Vax_Rate <- data$Primary_Vax_Rate / 100

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

#Model adjusted with education health outcomes and access to initial vaccination center
trial3 <- brm(
  formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio + edu_NoFormal_logratio + edu_Primary_logratio + edu_UpperSecondary_logratio + edu_Apprenticeship_logratio + edu_HonoursBachelor_logratio + health_VeryBad_logratio + health_Bad_logratio + health_Fair_logratio + health_Good_logratio + male_logratio +Wt_accessibility_Initial_Vacc ,
  data = data,
  family = Beta())

#Model adjusted with education health outcomes and access to initial vaccination center and GP and pharmacies

  trial2 <- brm(
  formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio + edu_NoFormal_logratio + edu_Primary_logratio + edu_UpperSecondary_logratio + edu_Apprenticeship_logratio + edu_HonoursBachelor_logratio + health_VeryBad_logratio + health_Bad_logratio + health_Fair_logratio + health_Good_logratio + male_logratio +Wt_accessibility_Initial_Vacc + accessibility_Pharmacy10 + accessibility_GP10 ,
  data = data,
  family = Beta())

# Plot of marginal effects
age_vars <- c("age_12to17_logratio", "age_18to54_logratio", "age_55to64_logratio", "age_65to70_logratio")
sex_vars <- c("male_logratio")
  
edu_vars <- c("edu_NoFormal_logratio", "edu_Primary_logratio", 
              "edu_UpperSecondary_logratio", "edu_Apprenticeship_logratio", 
              "edu_HonoursBachelor_logratio")

health_vars <- c("health_VeryBad_logratio", "health_Bad_logratio", 
                 "health_Fair_logratio", "health_Good_logratio")

access_vars <- c("Wt_accessibility_Initial_Vacc")

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
age_df <- extract_marginal_effects(age_vars, trial3, "Age")
edu_df <- extract_marginal_effects(edu_vars, trial3, "Education Level")
health_df <- extract_marginal_effects(health_vars, trial3, "Health Status")
sex_df <- extract_marginal_effects(sex_vars, trial3, "Sex")
access_df <- extract_marginal_effects(access_vars, trial3, "Accessibility")

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

ggplot(age_df_0, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Age",
    y = "Predicted Vaccination Rate",
    color = "Age",
    fill = "Age"
  ) +
  scale_x_continuous(
    name = "Additive Log-Ratio of Age (baseline: age 71+)",
    breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3),
    sec.axis = dup_axis(
      trans = ~exp(.),
      labels = function(x) round(exp(x), 1),
      name = "Delogged Age Ratio (relative to 71+)"
    )
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

#Morans'I
# Get posterior fitted values
fitted_vals <- fitted(trial3, summary = TRUE)[, "Estimate"]

# Compute residuals: observed - fitted
mean_resid <- trial3$data$Primary_Vax_Rate - fitted_vals

resid_df <- data.frame(CSO_LEA = data$CSO_LEA, resid = mean_resid)

# Merge with geo_data using CSO_LEA
sac_data <- geo_data %>%
  left_join(resid_df, by = "CSO_LEA")
neighbors <- poly2nb(sac_data)
weights <- nb2listw(neighbors, style = "W")
moran.test(sac_data$resid, weights)

#CAR Model
data$lea_id <- factor(1:nrow(data))
trial1_CAR_fixed <- brm(
  formula = Primary_Vax_Rate ~ 
    age_12to17_logratio + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio +
    edu_NoFormal_logratio + edu_Primary_logratio + edu_UpperSecondary_logratio + 
    edu_Apprenticeship_logratio + edu_HonoursBachelor_logratio +
    health_VeryBad_logratio + health_Bad_logratio + health_Fair_logratio + health_Good_logratio +
    Wt_accessibility_Initial_Vacc+ 
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
long_data <- long_data %>%
  rename(CSO_LEA = `LEA_Short`)
head(mergelong_data)
mergelong_data <- long_data %>%
  left_join(data, by = "CSO_LEA")
names(data_1)
data_1 <- mergelong_data %>% select(CSO_LEA, Primary_Vax_Rate.x, Month, Month_num, age_18to54_logratio, age_55to64_logratio, age_65to70_logratio, male_logratio)
data_1 <- data_1 %>%
  rename(Primary_Vax_Rate = `Primary_Vax_Rate.x`)


# Define the model formula
nl_formula <- bf(
  Primary_Vax_Rate ~ Asym / (1 + exp((xmid - Month_num) / scal)),
  Asym ~ 1 + (1 | CSO_LEA),
  xmid ~ 1 + (1 | CSO_LEA),
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

nl2_formula <- bf(
  Primary_Vax_Rate ~ base + (Asym - base) / (1 + exp((xmid - Month_num) / scal)),
  Asym ~ 1 + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio + male_logratio + (1 | CSO_LEA),
  xmid ~ 1 + (1 | CSO_LEA),
  scal ~ 1,
  base ~ 0,
  nl = TRUE,
  family = Beta()
)

# Define priors with explicit lower bound for scal
nl2_prior <- c(
  prior(normal(0.8, 0.1), nlpar = "Asym"),
  prior(normal(6, 2), nlpar = "xmid"),
  prior(exponential(1), nlpar = "scal", lb = 0)
)

# Fit the model
nl2_model <- brm(
  nl2_formula,
  data = data_1,
  prior = nl2_prior,
  chains = 4,
  iter = 4000,
  control = list(adapt_delta = 0.95)
)

nl3_formula <- bf(
  Primary_Vax_Rate ~ base + (Asym - base) / (1 + exp((xmid - Month_num) / scal)),
  Asym ~ 1 + age_18to54_logratio + age_55to64_logratio + age_65to70_logratio + male_logratio +  (1 | CSO_LEA),
  base ~ 1 + (1 | CSO_LEA),
  xmid ~ 1 + (1 | CSO_LEA),
  scal ~ 1,
  nl = TRUE,
  family = Beta()
)

nl3_model <- brm(
  nl3_formula,
  data = data_1,
  prior = nl2_prior,
  chains = 4,
  iter = 4000,
  control = list(adapt_delta = 0.95)
)

library(gtsummary)
library(broom.mixed)
library(webshot2)
library(gt)

tbln <- nl3_model %>%
  tidy(effects = "fixed", conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  gt::gt() %>%
  gt::fmt_number(columns = c(estimate, conf.low, conf.high), 
                 decimals = 3) %>%
  gt::cols_label(
    term = "Variable",
    estimate = "Estimate",
    conf.low = "95% CI Lower",
    conf.high = "95% CI Upper"
  ) %>%
  gt::tab_header(title = "Temporal Model Results")
gtsave(tbln, filename = "modelnm_results.html")

library(tidybayes)

lea_effects <- ranef(nl_model)$CSO_LEA %>%
  as_tibble(rownames = "CSO_LEA") %>%
  arrange(desc(Estimate.Asym_Intercept))

top_leas <- head(lea_effects$CSO_LEA, 5)
bottom_leas <- tail(lea_effects$CSO_LEA, 5)

# Subset data
plot_data <- long_data %>%
  filter(CSO_LEA %in% c(top_leas, bottom_leas)) %>%
  add_epred_draws(nl_model, ndraws = 50)

plot_data <- plot_data %>%
  mutate(
    group = case_when(
      CSO_LEA %in% top_leas ~ "Top 5 Uptake",
      CSO_LEA %in% bottom_leas ~ "Bottom 5 Uptake"
    )
  )
library(ggdist)

bottom_data <- plot_data %>% filter(CSO_LEA %in% bottom_leas)

bottom_plot <- ggplot(bottom_data, aes(x = Month_num, y = .epred)) +
  stat_lineribbon(
    aes(group = .draw), 
    alpha = 0.05, fill = "#d7191c", color = "#d7191c", linewidth = 0.2
  ) +
  geom_point(
    aes(y = Primary_Vax_Rate), 
    size = 1.5, color = "#fdae61", alpha = 0.7
  ) +
  facet_wrap(~CSO_LEA, nrow = 2) +
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
bottom_plot
top_data <- plot_data %>% filter(CSO_LEA %in% top_leas)

top_plot <- ggplot(top_data, aes(x = Month_num, y = .epred)) +
  stat_lineribbon(
    aes(group = .draw), 
    alpha = 0.05, fill = "#d7191c", color = "#d7191c", linewidth = 0.2
  ) +
  geom_point(
    aes(y = Primary_Vax_Rate), 
    size = 1.5, color = "#fdae61", alpha = 0.7
  ) +
  facet_wrap(~CSO_LEA, nrow = 2) +
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


lea_effects2 <- ranef(nl3_model)$CSO_LEA %>%
  as_tibble(rownames = "CSO_LEA") %>%
  arrange(desc(Estimate.Asym_Intercept))

top_leas2 <- head(lea_effects2$CSO_LEA, 5)
bottom_leas2 <- tail(lea_effects2$CSO_LEA, 5)

# Subset data
plot_data2 <- data_1 %>%
  filter(CSO_LEA %in% c(top_leas2, bottom_leas2)) %>%
  add_epred_draws(nl3_model, ndraws = 50)

plot_data2 <- plot_data2 %>%
  mutate(
    group = case_when(
      CSO_LEA %in% top_leas2 ~ "Top 5 Uptake",
      CSO_LEA %in% bottom_leas2 ~ "Bottom 5 Uptake"
    )
  )

bottom_data2 <- plot_data2 %>% filter(CSO_LEA %in% bottom_leas2)

bottom_plot2 <- ggplot(bottom_data2, aes(x = Month_num, y = .epred)) +
  stat_lineribbon(
    aes(group = .draw), 
    alpha = 0.05, fill = "#d7191c", color = "#d7191c", linewidth = 0.2
  ) +
  geom_point(
    aes(y = Primary_Vax_Rate), 
    size = 1.5, color = "#fdae61", alpha = 0.7
  ) +
  facet_wrap(~CSO_LEA, nrow = 2) +
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
bottom_plot2
top_data2 <- plot_data2 %>% filter(CSO_LEA %in% top_leas2)

top_plot2 <- ggplot(top_data2, aes(x = Month_num, y = .epred)) +
  stat_lineribbon(
    aes(group = .draw), 
    alpha = 0.05, fill = "#d7191c", color = "#d7191c", linewidth = 0.2
  ) +
  geom_point(
    aes(y = Primary_Vax_Rate), 
    size = 1.5, color = "#fdae61", alpha = 0.7
  ) +
  facet_wrap(~CSO_LEA, nrow = 2) +
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
top_plot2
ggsave("Top_LEA.png", plot = top_plot2, width = 8, height = 6, dpi = 300)
ggsave("Bottom_LEA.png", plot = bottom_plot2, width = 8, height = 6, dpi = 300)

increase_dip_analysis <- long_data %>%
  group_by(CSO_LEA) %>%
  arrange(Month_num) %>%
  mutate(
    rate_change = Primary_Vax_Rate - lag(Primary_Vax_Rate),
    rate_change_2 = lead(Primary_Vax_Rate) - Primary_Vax_Rate
  ) %>%
  # Find peak vaccination rate and its timing
  mutate(
    peak_rate = max(Primary_Vax_Rate, na.rm = TRUE),
    is_peak = Primary_Vax_Rate == peak_rate,
    peak_month = ifelse(is_peak, Month_num, NA)
  ) %>%
  fill(peak_month, .direction = "downup") %>%
  # Calculate decline after peak
  mutate(
    months_after_peak = Month_num - peak_month,
    decline_after_peak = ifelse(months_after_peak > 0, 
                               peak_rate - Primary_Vax_Rate, 0)
  ) %>%
  ungroup()

# Identify LEAs with significant decline after peak
decline_leas <- increase_dip_analysis %>%
  group_by(CSO_LEA) %>%
  summarise(
    peak_rate = first(peak_rate),
    peak_month = first(peak_month),
    max_decline = max(decline_after_peak, na.rm = TRUE),
    final_rate = last(Primary_Vax_Rate),
    decline_magnitude = peak_rate - final_rate,
    has_late_decline = any(months_after_peak >= 2 & decline_after_peak > 0.05),
    .groups = "drop"
  ) %>%
  filter(decline_magnitude > 0.02 | has_late_decline) %>%
  arrange(desc(decline_magnitude))

decline_leas$CSO_LEA
##Model with Depriviation
dep_data <- read.csv("Final_Merged_Dataset/Complete_Merged_Dataset.csv")
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
depri_plot <- ggplot(dep_df, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
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

ggsave("depri_plot.png", plot = depri_plot, width = 8, height = 6, dpi = 300)


###Model with Joint Distributuions of Age and Sex
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

age_male <- extract_marginal_effects(age_male_vars, trial5, "AgeXMale")

library(patchwork)

# Plot for Age × Female
plot_female <- ggplot(age_female, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Age × Female",
    x = "Additive Log-Ratio of Female Age (baseline: age 71+)",
    y = "Predicted Vaccination Rate",
    color = "Age × Female",
    fill = "Age × Female"
  ) +
  theme_minimal(base_size = 14)

# Plot for Age × Male
plot_male <- ggplot(age_male, aes(x = x_val, y = estimate__, color = variable, fill = variable)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effects: Age × Male",
    x = "Additive Log-Ratio of Male Age (baseline: age 71+)",
    y = "Predicted Vaccination Rate",
    color = "Age × Male",
    fill = "Age × Male"
  ) +
  theme_minimal(base_size = 14)

# Combine side by side
plot_female + plot_male + plot_layout(ncol = 2)

ggsave("plot_male_jd.png", plot = plot_female, width = 8, height = 6, dpi = 300)
ggsave("plot_female_jd.png", plot = plot_male, width = 8, height = 6, dpi = 300)

## Vaccination Rate Map of final primary dose rate for Presentation
geo_data
filtered_data <- vax_data %>%
  filter(Month == "2023 June", Age.Group == "12 years and over") %>%
  select(LEA_Short, `Primary.Course.Completed....`)
vaccination_centers <- read.csv("Vacc_Rates&Geocoded_Data\\geocoded_addresses_vac_final.csv")
plot_data <-  merge(geo_data, filtered_data, 
                                  by.x = "CSO_LEA",        
                                  by.y = "LEA_Short",
                                  all.x = FALSE,          
                                  all.y = FALSE)
dim(plot_data)
plot_data$ Primary.Course.Completed.... <- plot_data$ Primary.Course.Completed.... / 100
color_palette <- colorBin(
  palette = "RdYlGn",
  domain = plot_data$ Primary.Course.Completed....,
  bins = 5, 
  reverse = FALSE
)

# Step 5: Create a leaflet map
Final_Rate_Primary_Dose <- leaflet(plot_data) %>%
  addTiles("Stamen.Watercolor") %>%  
  addPolygons(
    fillColor = ~color_palette( Primary.Course.Completed....),  # Apply color palette
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
    label = ~paste0(CSO_LEA, ": ",  Primary.Course.Completed....),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = color_palette, 
    values = ~ Primary.Course.Completed...., 
    opacity = 0.7, 
    title = "Final Primary Dose Vaccination Proportion",
    position = "bottomright"
  )

ggsave("Final_Rate_Primary_Dose.png", plot = Final_Rate_Primary_Dose, width = 8, height = 6, dpi = 300)
####Model Summaries
tbl1 <- trial1 %>%
  tidy(effects = "fixed", conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  gt::gt() %>%
  gt::fmt_number(columns = c(estimate, conf.low, conf.high), 
                 decimals = 3) %>%
  gt::cols_label(
    term = "Variable",
    estimate = "Estimate",
    conf.low = "95% CI Lower",
    conf.high = "95% CI Upper"
  ) %>%
  gt::tab_header(title = "Basic Demographic Model Results")
gtsave(tbl1, filename = "model1_results.html")

tbl2 <- trial3 %>%
  tidy(effects = "fixed", conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  gt::gt() %>%
  gt::fmt_number(columns = c(estimate, conf.low, conf.high), 
                 decimals = 3) %>%
  gt::cols_label(
    term = "Variable",
    estimate = "Estimate",
    conf.low = "95% CI Lower",
    conf.high = "95% CI Upper"
  ) %>%
  gt::tab_header(title = "Extended Model with Education, Health & Accessibility")
gtsave(tbl2, filename = "model2_results.html")

tbl3 <- trial1_CAR_fixed %>%
  tidy(effects = "fixed", conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  gt::gt() %>%
  gt::fmt_number(columns = c(estimate, conf.low, conf.high), 
                 decimals = 3) %>%
  gt::cols_label(
    term = "Variable",
    estimate = "Estimate",
    conf.low = "95% CI Lower",
    conf.high = "95% CI Upper"
  ) %>%
  gt::tab_header(title = "Spatial CAR Model Results")
gtsave(tbl3, filename = "model3_results.html")


tbl4 <- trial4 %>%
  tidy(effects = "fixed", conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  gt::gt() %>%
  gt::fmt_number(columns = c(estimate, conf.low, conf.high), 
                 decimals = 3) %>%
  gt::cols_label(
    term = "Variable",
    estimate = "Estimate",
    conf.low = "95% CI Lower",
    conf.high = "95% CI Upper"
  ) %>%
  gt::tab_header(title = "Extended Model with Deprivation Index")
gtsave(tbl4, filename = "model4_results.html")



tbl5 <- trial5 %>%
  tidy(effects = "fixed", conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  gt::gt() %>%
  gt::fmt_number(columns = c(estimate, conf.low, conf.high), 
                 decimals = 3) %>%
  gt::cols_label(
    term = "Variable",
    estimate = "Estimate",
    conf.low = "95% CI Lower",
    conf.high = "95% CI Upper"
  ) %>%
  gt::tab_header(title = "Age-Gender Stratified Model (ALR Transformed)")
gtsave(tbl5, filename = "model5_results.html")


library(modelsummary)

# Compare all models
model_list <- list(
  "Basic Demographics" = trial1,
  "Extended" = trial3,
  "Spatial CAR" = trial1_CAR_fixed,
  "With Deprivation" = trial4,
  "Age-Gender Stratified" = trial5
)

modelsummary(
  model_list,
  fmt = 3,
  statistic = "conf.int",
  conf_level = 0.95,
  title = "Comparison of All Vaccination Rate Models",
  notes = c("95% credible intervals in brackets.", 
            "All models use Beta regression with logit link.",
            "ALR = Additive Log-Ratio transformation")
)

# 4-parameter logistic function
logistic_4p <- function(x, base, Asym, xmid, scal) {
  base + (Asym - base) / (1 + exp((xmid - x) / scal))
}

# Time values (e.g., months since rollout)
x_vals <- seq(0, 12, length.out = 200)

# Parameter sets for illustration
param_grid <- expand.grid(
  param = c("base", "Asym", "xmid", "scal"),
  variant = c("low", "baseline", "high"),
  stringsAsFactors = FALSE
)

# Define parameter variations
param_values <- list(
  base     = c(low = 0.0, baseline = 0.1, high = 0.2),
  Asym     = c(low = 0.7, baseline = 0.8, high = 0.9),
  xmid     = c(low = 4,   baseline = 6,   high = 8),
  scal     = c(low = 0.8, baseline = 1.5, high = 3.0)
)

# Baseline values for all parameters
baseline_params <- list(base = 0.1, Asym = 0.8, xmid = 6, scal = 1.5)

curve_data <- param_grid %>%
  rowwise() %>%
  mutate(
    values = list({
      p <- baseline_params
      p[[param]] <- param_values[[param]][[variant]]
      y <- logistic_4p(x_vals, base = p$base, Asym = p$Asym, xmid = p$xmid, scal = p$scal)
      tibble(x = x_vals, y = y, variant = variant)
    })
  ) %>%
  unnest(values, names_sep = "_")

# Plot for each parameter variation
sigmoid_curve <- ggplot(curve_data, aes(x = values_x, y = values_y, color = variant)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ param, scales = "free_y") +
  scale_color_manual(values = c("low" = "red", "baseline" = "black", "high" = "blue")) +
  labs(
    title = "Effect of 4-Parameter Logistic Components on Vaccination Uptake Curves",
    x = "Time (e.g., Months Since Rollout)",
    y = "Vaccination Uptake",
    color = "Parameter Level"
  ) +
  theme_minimal(base_size = 14)
ggsave("sigmoid_curve.png", plot = sigmoid_curve, width = 8, height = 6, dpi = 300)
