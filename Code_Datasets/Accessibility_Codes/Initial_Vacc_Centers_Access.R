library(sf)
library(tidyverse)
library(osrm)
library(leaflet)

##ORS for batch generation of isochrones
library(openrouteservice)

ors_api_key("5b3ce3597851110001cf62489dfc4d5fac624b6e8a71af1fe67888f7")

# Function to generate isochrones for multiple pharmacies in batches using OpenRouteService
generate_batch_isochrones_ors <- function(loc_sf, travel_time, batch_size = 2, pause = 2) {
  # Extract coordinates of all pharmacies
  loc_coords <- st_coordinates(loc_sf) %>% 
    as.data.frame() %>%
    rename(lon = X, lat = Y)
  
  # Convert coordinates into the format expected by ORS (array of lon/lat pairs)
  locations <- as.matrix(loc_coords[, c("lon", "lat")])
  
  # Split the locations into batches of size 'batch_size'
  num_batches <- ceiling(nrow(locations) / batch_size)
  
  # Initialize a list to store isochrones for all pharmacies
  all_isochrones <- list()
  
  for (i in 1:num_batches) {
    # Get the current batch of locations
    cat("Processing batch", i, "of", num_batches, "\n")
    rows <- ((i - 1) * batch_size + 1):min(i * batch_size, nrow(locations))
    batch_locations <- locations[rows, , drop = FALSE]
    
    # Generate isochrones for the current batch
    batch_isochrones <- ors_isochrones(
      locations = batch_locations,  
      profile = "driving-car",  
      range = rep(travel_time, nrow(batch_locations)),
      output = "sf"  
    )
    
    # Store the result for the current batch
    all_isochrones[[i]] <- batch_isochrones
  }
  
  # Combine the results from all batches into one SF object
  combined_isochrones <- do.call(rbind, all_isochrones)
  
  return(combined_isochrones)
}

library(dplyr)
library(ggplot2)

# Load LEA Boundaries
gfile <- "Accessibility_Data\\CSO_Local_Electoral_Areas_National_Statistical_Boundaries_2022_Generalised_100m_-6420530397479472898.geojson"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE))
geo_data %>% 
  st_drop_geometry() %>% 
  group_by(CSO_LEA) %>%
  summarize(count = n())%>%
  filter(count > 1)
geo_data %>% filter(CSO_LEA == "ATHLONE")
geo_data <- geo_data %>% mutate(CSO_LEA=case_when(CSO_LEA == "ATHLONE" & COUNTY == "WESTMEATH" ~ "ATHLONE_WESTMEATH",TRUE ~ CSO_LEA))
#Check drop
geo_data %>% filter(CSO_LEA == "ATHLONE")
# Population data 
pop_data <- read.csv("Accessibility_Data\\LEA_POP22.csv", stringsAsFactors = FALSE)
names(pop_data)
# Standardize LEA names
pop_data$CSO_LEA <- ifelse(pop_data$CSO_LEA == "ATHLONE-LEA 5", "ATHLONE_WESTMEATH",
                       ifelse(pop_data$CSO_LEA == "ATHLONE-LEA 6", "ATHLONE",
                              toupper(pop_data$CSO_LEA)))  

pop_data %>% filter(CSO_LEA == "ATHLONE")
# Merging population data with geo_data using cso_lea as the key
geo_data <- geo_data %>%
  left_join(pop_data, by = "CSO_LEA")  
sum(is.na(geo_data))
head(geo_data)

#Reading Initial Vaccination Center Data

vacc_center <- read.csv("Vacc_rates&Geocoded_Data\\geocoded_addresses_vac_final.csv", stringsAsFactors = FALSE)

loc_sf <- st_as_sf(vacc_center, coords = c("longitude", "latitude"), crs = 4326)
dim(loc_sf)

# Generate 10-minute isochrones for pharmacies

#isochrones <- generate_batch_isochrones_ors(loc_sf, travel_time = 600)
#isochrones_geometry <- isochrones %>% select(geometry)
dim(isochrones)


#st_write(isochrones_geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones.geojson", driver = "GeoJSON")

#Read isochrones from Accessibility_Data

isochrones <- st_read("Accessibility_Data\\isochrones.geojson")

# Ensuring valid geometries before spatial operations
geo_data <- st_make_valid(geo_data)
isochrones <- st_make_valid(isochrones_geometry)

#Sampling 10 LEAs
set.seed(452)
geo_sample<- geo_data
head(geo_sample)
class(pop_data$TOTPOP22)
head(pop_data$TOTPOP22)
geo_sample$TOTPOP22 <- gsub(",", "", geo_sample$TOTPOP22)
geo_sample$TOTPOP22 <- as.numeric(geo_sample$TOTPOP22)
isochrones$isochrone_id <- 1:nrow(isochrones)
dim(geo_sample)

# Ensuring CRS is consistent
isochrones_10min <- st_transform(isochrones, st_crs(geo_sample))

# Calculating intersection areas between LEAs and 10-minute isochrones
intersections_10min <- st_intersection(geo_sample, isochrones_10min)
intersections_10min$intersection_area <- st_area(intersections_10min)
dim(intersections_10min)
# Normalizing intersection areas to get C_ij
isochrones_area <- isochrones_10min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)
C_ij_10min <- intersections_10min %>%
  left_join(isochrones_area, by = "isochrone_id") %>%
  mutate(
    access_share = as.numeric(intersection_area / total_area)
  ) %>%
  group_by(CSO_LEA) %>%
  summarize(
    accessibility = sum(access_share, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  )%>%
  st_drop_geometry()

#LEAs not in c_ij_10mins
LEA_no_intersection <- setdiff(geo_sample$CSO_LEA, C_ij_10min$CSO_LEA)
LEA_no_intersection

access_values <- C_ij_10min %>%
  left_join(
    geo_sample %>%
      st_drop_geometry() %>%
      select(CSO_LEA, TOTPOP22),
    by = "CSO_LEA"
  ) %>%
  mutate(
    relative_accessibility = accessibility / TOTPOP22
  )

access_values


#Add accessibility measures across all the different isochrones
#Add the data together
#Build the X's - with naive measures too - 30 and 60 minute centers more necessary for initial vacc centers and not for the GPs nd pharmacies

ggplot(access_values) +
  geom_sf(aes(fill = accessibility)) +
  scale_fill_viridis_c(name = "Accessibility\nper capita", option = "viridis") +
  theme_minimal() +
  labs(title = "Per Capita Accessibility by Local Electoral Area",
       subtitle = "Accessibility normalized by population (TOTPOP22)")

## 20, 30 and 60 min isochrones

#isochrones_20 <- generate_batch_isochrones_ors(loc_sf, travel_time = 1200)
#isochrones_20geometry <- isochrones_20 %>% select(geometry)
#isochrones_30 <- generate_batch_isochrones_ors(loc_sf, travel_time = 1800)
#isochrones_30geometry <- isochrones_30 %>% select(geometry)
#isochrones_60 <- generate_batch_isochrones_ors(loc_sf, travel_time = 3600)
#isochrones_60geometry <- isochrones_60 %>% select(geometry)

#st_write(isochrones_geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_iv.geojson", driver = "GeoJSON")
#st_write(isochrones_20geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_2iv.geojson", driver = "GeoJSON")
#st_write(isochrones_30geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_3iv.geojson", driver = "GeoJSON")
#st_write(isochrones_60geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_6iv.geojson", driver = "GeoJSON")

#Read isochrones from Accessibility_Data
isochrones_10 <- st_read("Accessibility_Data\\isochrones_iv.geojson")
isochrones_20 <- st_read("Accessibility_Data\\isochrones_2iv.geojson")
isochrones_30 <- st_read("Accessibility_Data\\isochrones_3iv.geojson")
isochrones_60 <- st_read("Accessibility_Data\\isochrones_6iv.geojson")

# Ensuring valid geometries before spatial operations
geo_data <- st_make_valid(geo_data)
isochrones_10 <- st_make_valid(isochrones_10)
isochrones_20 <- st_make_valid(isochrones_20)
isochrones_30 <- st_make_valid(isochrones_30)
isochrones_60 <- st_make_valid(isochrones_60)

#Sampling 10 LEAs
set.seed(452)
geo_sample<- geo_data
head(geo_sample)
class(pop_data$TOTPOP22)
head(pop_data$TOTPOP22)
geo_sample$TOTPOP22 <- gsub(",", "", geo_sample$TOTPOP22)
geo_sample$TOTPOP22 <- as.numeric(geo_sample$TOTPOP22)
isochrones_10$isochrone_id <- 1:nrow(isochrones_10)
isochrones_20$isochrone_id <- 1:nrow(isochrones_20)
isochrones_30$isochrone_id <- 1:nrow(isochrones_30)
isochrones_60$isochrone_id <- 1:nrow(isochrones_60)

# Ensuring CRS is consistent
isochrones_10min <- st_transform(isochrones_10, st_crs(geo_sample))
isochrones_20min <- st_transform(isochrones_20, st_crs(geo_sample))
isochrones_30min <- st_transform(isochrones_30, st_crs(geo_sample))
isochrones_60min <- st_transform(isochrones_60, st_crs(geo_sample))
isochrones_20min <- isochrones_20min %>%
  group_by(isochrone_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
isochrones_30min <- isochrones_30min %>%
  group_by(isochrone_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Calculating intersection areas between LEAs and 10, 20, 30, 60-minute isochrones
intersections_10min <- st_intersection(geo_sample, isochrones_10min)
intersections_10min$intersection_area <- st_area(intersections_10min)
intersections_20min <- st_intersection(geo_sample, isochrones_20min)
intersections_20min$intersection_area <- st_area(intersections_20min)
dim(intersections_20min)
intersections_30min <- st_intersection(geo_sample, isochrones_30min)
intersections_30min$intersection_area <- st_area(intersections_30min)
intersections_60min <- st_intersection(geo_sample, isochrones_60min)
intersections_60min$intersection_area <- st_area(intersections_60min)
# Normalizing intersection areas to get C_ij
isochrones_area_10 <- isochrones_10min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)
isochrones_area_20 <- isochrones_20min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)
isochrones_area_30 <- isochrones_30min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)
isochrones_area_60 <- isochrones_60min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)


C_ij_10min <- intersections_10min %>%
  left_join(isochrones_area_10, by = "isochrone_id") %>%
  mutate(access_share_10 = as.numeric(intersection_area / total_area)) %>%
  group_by(CSO_LEA) %>%
  reframe(
    accessibility_10 = sum(access_share_10, na.rm = TRUE)
  )

C_ij_20min <- intersections_20min %>%
  left_join(isochrones_area_20, by = "isochrone_id") %>%
  mutate(access_share_20 = as.numeric(intersection_area / total_area)) %>%
  group_by(CSO_LEA) %>%
  reframe(accessibility_20 = sum(access_share_20, na.rm = TRUE))

C_ij_30min <- intersections_30min %>%
  left_join(isochrones_area_30, by = "isochrone_id") %>%
  mutate(access_share_30 = as.numeric(intersection_area / total_area)) %>%
  group_by(CSO_LEA) %>%
  reframe(accessibility_30 = sum(access_share_30, na.rm = TRUE))

C_ij_60min <- intersections_60min %>%
  left_join(isochrones_area_60, by = "isochrone_id") %>%
  mutate(access_share_60 = as.numeric(intersection_area / total_area)) %>%
  group_by(CSO_LEA) %>%
  reframe(accessibility_60 = sum(access_share_60, na.rm = TRUE))


#LEAs not in c_ij_20_30_mins
LEA_no_10intersection <- setdiff(geo_sample$CSO_LEA, C_ij_20min$CSO_LEA)
LEA_no_10intersection
LEA_no_20intersection <- setdiff(geo_sample$CSO_LEA, C_ij_20min$CSO_LEA)
LEA_no_20intersection
LEA_no_30intersection <- setdiff(geo_sample$CSO_LEA, C_ij_30min$CSO_LEA)
LEA_no_30intersection
LEA_no_60intersection <- setdiff(geo_sample$CSO_LEA, C_ij_60min$CSO_LEA)
LEA_no_60intersection


combined_access_values <- geo_sample %>%
  select(CSO_LEA, TOTPOP22, geometry) %>%
  left_join(C_ij_10min, by = "CSO_LEA") %>%
  left_join(C_ij_20min, by = "CSO_LEA") %>%
  left_join(C_ij_30min, by = "CSO_LEA") %>%
  left_join(C_ij_60min, by = "CSO_LEA") %>%
  mutate(
    accessibility_10 = ifelse(is.na(accessibility_10), 0, accessibility_10),
    accessibility_20 = ifelse(is.na(accessibility_20), 0, accessibility_20),
    accessibility_30 = ifelse(is.na(accessibility_30), 0, accessibility_30),
    accessibility_60 = ifelse(is.na(accessibility_60), 0, accessibility_60),
    relative_accessibility_10min = accessibility_10 / TOTPOP22,
    relative_accessibility_20min = accessibility_20 / TOTPOP22,
    relative_accessibility_30min = accessibility_30 / TOTPOP22,
    relative_accessibility_60min = accessibility_60 / TOTPOP22,
    weighted_accessibility = (
      (relative_accessibility_10min / 10) +
        (relative_accessibility_20min / 20) +
        (relative_accessibility_30min / 30) +
        (relative_accessibility_60min / 60)
    ) / (
      (1 / 10) + (1 / 20) + (1 / 30) + (1 / 60)
    )
  )

combined_access_values



#Plotting 60 min isochrone accessibility

C_ij_20min %>%
  filter(duplicated(CSO_LEA) | duplicated(CSO_LEA, fromLast = TRUE)) %>%
  arrange(CSO_LEA) %>%
  head(20)



ggplot(combined_access_values) + geom_sf(aes(fill=relative_accessibility_60min)) + scale_fill_viridis_c(
  option = "plasma",
  direction = -1,
  name = "Relative Accessibility"
) +  geom_sf(data = loc_sf, color = "white", size = 2, shape = 21, fill = "white", stroke = 1) +
  labs(
    title = "Relative Accessibility to Services within 60 Minutes") +
  theme_minimal()

ggplot(combined_access_values) + geom_sf(aes(fill=accessibility_60)) + scale_fill_viridis_c(
  option = "plasma",
  direction = -1,
  name = "Accessibility"
) + geom_sf(data = loc_sf, color = "white", size = 2, shape = 21, fill = "white", stroke = 1) +
  labs(
    title = "Accessibility to Services within 60 Minutes") +
  theme_minimal()

#Function to carry out above code
calculate_accessibility <- function(geo_sample, isochrones_list) {
  # Add unique IDs
  for (minutes in names(isochrones_list)) {
    isochrones_list[[minutes]]$isochrone_id <- 1:nrow(isochrones_list[[minutes]])
  }
  
  # Transform CRS
  isochrones_transformed <- lapply(isochrones_list, function(iso) {
    st_transform(iso, st_crs(geo_sample))
  })
  
  # Compute intersections and accessibility metrics
  access_results <- list()
  for (time in names(isochrones_transformed)) {
    iso <- isochrones_transformed[[time]]
    intersections <- st_intersection(geo_sample, iso)
    intersections$intersection_area <- st_area(intersections)
    iso_areas <- iso %>%
      mutate(total_area = st_area(geometry)) %>%
      st_drop_geometry() %>%
      select(isochrone_id, total_area)
    
    C_ij <- intersections %>%
      left_join(iso_areas, by = "isochrone_id") %>%
      mutate(access_share = as.numeric(intersection_area / total_area)) %>%
      group_by(CSO_LEA) %>%
      reframe(!!paste0("accessibility_", time) := sum(access_share, na.rm = TRUE))
    
    access_results[[time]] <- C_ij
  }
  
  # Combine all accessibility metrics
  combined <- geo_sample %>%
    select(CSO_LEA, TOTPOP22, geometry)
  
  for (time in names(access_results)) {
    combined <- left_join(combined, access_results[[time]], by = "CSO_LEA")
    combined[[paste0("accessibility_", time)]] <- ifelse(
      is.na(combined[[paste0("accessibility_", time)]]), 0,
      combined[[paste0("accessibility_", time)]]
    )
    combined[[paste0("relative_accessibility_", time, "min")]] <-
      combined[[paste0("accessibility_", time)]] / combined$TOTPOP22
  }
  
  # Calculate weighted accessibility
  time_weights <- c("10" = 1/10, "20" = 1/20, "30" = 1/30, "60" = 1/60)
  numerator <- 0
  denominator <- 0
  
  for (time in names(time_weights)) {
    var <- paste0("relative_accessibility_", time, "min")
    if (var %in% colnames(combined)) {
      numerator <- numerator + time_weights[[time]] * combined[[var]]
      denominator <- denominator + time_weights[[time]]
    }
  }
  
  combined$weighted_accessibility <- numerator / denominator
  return(combined)
}
