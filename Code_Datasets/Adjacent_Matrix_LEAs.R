library(mapview)
library(spdep)
library(sf)
library(dplyr)

gfile <- "Accessibility_Data\\CSO_Local_Electoral_Areas_National_Statistical_Boundaries_2022_Generalised_100m_-6420530397479472898.geojson"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE))
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
epsilon <- 1e-4
data$Primary_Vax_Rate <- pmin(pmax(data$Primary_Vax_Rate, epsilon), 1 - epsilon)
# Apply log-ratio transformation proportion column
p_vars <- names(data)[grepl("^p_", names(data))]
sapply(data[p_vars], function(x) sum(x == 0 | x == 1))
data[p_vars] <- lapply(data[p_vars], function(x) {log(x / (1 - x))})
library(brms)
# Build formula dynamically
predictors <- setdiff(names(data), c("CSO_LEA", "Primary_Vax_Rate"))
formula <- as.formula(
  paste("Primary_Vax_Rate ~", paste(predictors, collapse = " + "))
)

# Fit beta regression
trial1 <- brm(
  formula = formula,
  data = data,
  family = Beta(),
  chains = 4,
  cores = 4,
  iter = 2000,
  control = list(adapt_delta = 0.95)
)
