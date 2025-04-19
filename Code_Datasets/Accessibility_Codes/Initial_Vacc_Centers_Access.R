library(sf)
library(tidyverse)
library(osrm)
library(leaflet)

##ORS for batch generation of isochrones
library(openrouteservice)

ors_api_key("5b3ce3597851110001cf62486cbc3afb79d04cf492c9e160d17ae49c")

# Function to generate isochrones for multiple pharmacies in batches using OpenRouteService
generate_batch_isochrones_ors <- function(loc_sf, travel_time, batch_size = 5) {
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
    batch_locations <- locations[((i - 1) * batch_size + 1):min(i * batch_size, nrow(locations)), ]
    
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
geo_data <- geo_data %>%
  filter(!grepl("Athlone", CSO_LEA, ignore.case = TRUE))
#Must check Athlone duplication
#geo_data <- geo_data %>% mutate(CSO_LEA=case_when(CSO_LEA == "ATHLONE" & COUNTY == "WESTMEATH" ~ "ATHLONE_WESTMEATH",TRUE ~ CSO_LEA))
#Check drop
geo_data %>% filter(CSO_LEA == "ATHLONE")
# Population data 
pop_data <- read.csv("Accessibility_Data\\LEA_POP22.csv", stringsAsFactors = FALSE)
names(pop_data)
#Athlone lea - 5 and athlone lea - 6?
# Merging population data with geo_data using cso_lea as the key
geo_data <- geo_data %>%
  left_join(pop_data, by = "CSO_LEA")  
sum(is.na(geo_data))
head(geo_data)


vacc_center <- read.csv("Vacc_rates&Geocoded_Data\\geocoded_addresses_vac_final.csv", stringsAsFactors = FALSE)

loc_sf <- st_as_sf(vacc_center, coords = c("longitude", "latitude"), crs = 4326)
dim(loc_sf)
# Generate 10-minute isochrones for pharmacies

isochrones <- generate_batch_isochrones_ors(loc_sf, travel_time = 600)
isochrones_geometry <- isochrones %>% select(geometry)
dim(isochrones)


#st_write(isochrones_geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones.geojson", driver = "GeoJSON")

#isochrones <- st_read("Accessibility_Data\\isochrones.geojson")

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

#need to join geometry to get choropleth

#Add accessibility measures across all the different isochrones
#Add the data together
#Build the X's - with naive measures too - 30 and 60 minute centers more necessary for initial vacc centers and not for the GPs nd pharmacies

ggplot(access_values) +
  geom_sf(aes(fill = accessibility)) +
  scale_fill_viridis_c(name = "Accessibility\nper capita", option = "viridis") +
  theme_minimal() +
  labs(title = "Per Capita Accessibility by Local Electoral Area",
       subtitle = "Accessibility normalized by population (TOTPOP22)",
       caption = "Source: Your Data Source")

## 

isochrones_20 <- generate_batch_isochrones_ors(loc_sf, travel_time = 1200)
isochrones_20geometry <- isochrones_20 %>% select(geometry)
isochrones_30 <- generate_batch_isochrones_ors(loc_sf, travel_time = 1800)
isochrones_30geometry <- isochrones_30 %>% select(geometry)
isochrones_60 <- generate_batch_isochrones_ors(loc_sf, travel_time = 3600)
isochrones_60geometry <- isochrones_60 %>% select(geometry)

#st_write(isochrones_geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_iv.geojson", driver = "GeoJSON")
#st_write(isochrones_20geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_2iv.geojson", driver = "GeoJSON")
#st_write(isochrones_30geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_3iv.geojson", driver = "GeoJSON")

#isochrones <- st_read("C:/Users/Sivagami Nedumaran/Downloads/isochrones_3iv.geojson")

# Ensuring valid geometries before spatial operations
geo_data <- st_make_valid(geo_data)
isochrones_20 <- st_make_valid(isochrones_20geometry)
isochrones_30 <- st_make_valid(isochrones_30geometry)

#Sampling 10 LEAs
set.seed(452)
geo_sample<- geo_data
head(geo_sample)
class(pop_data$TOTPOP22)
head(pop_data$TOTPOP22)
geo_sample$TOTPOP22 <- gsub(",", "", geo_sample$TOTPOP22)
geo_sample$TOTPOP22 <- as.numeric(geo_sample$TOTPOP22)
isochrones_20$isochrone_id <- 1:nrow(isochrones_20)
isochrones_30$isochrone_id <- 1:nrow(isochrones_30)

# Ensuring CRS is consistent
isochrones_20min <- st_transform(isochrones_20, st_crs(geo_sample))
isochrones_30min <- st_transform(isochrones_30, st_crs(geo_sample))

# Calculating intersection areas between LEAs and 10-minute isochrones
intersections_20min <- st_intersection(geo_sample, isochrones_20min)
intersections_20min$intersection_area <- st_area(intersections_20min)
dim(intersections_20min)
intersections_30min <- st_intersection(geo_sample, isochrones_30min)
intersections_30min$intersection_area <- st_area(intersections_30min)
dim(intersections_30min)
# Normalizing intersection areas to get C_ij
isochrones_area_20 <- isochrones_20min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)
isochrones_area_30 <- isochrones_30min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)

C_ij_20min <- intersections_20min %>%
  left_join(isochrones_area, by = "isochrone_id") %>%
  mutate(
    access_share_20 = as.numeric(intersection_area / total_area)
  ) %>%
  group_by(CSO_LEA) %>%
  summarize(
    accessibility_20 = sum(access_share_20, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  )%>%
  st_drop_geometry()

C_ij_30min <- intersections_30min %>%
  left_join(isochrones_area, by = "isochrone_id") %>%
  mutate(
    access_share_30 = as.numeric(intersection_area / total_area)
  ) %>%
  group_by(CSO_LEA) %>%
  summarize(
    accessibility_30 = sum(access_share_30, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  )%>%
  st_drop_geometry()

#LEAs not in c_ij_20_30_mins
LEA_no_20intersection <- setdiff(geo_sample$CSO_LEA, C_ij_20min$CSO_LEA)
LEA_no_20intersection
LEA_no_30intersection <- setdiff(geo_sample$CSO_LEA, C_ij_30min$CSO_LEA)
LEA_no_30intersection

combined_access_values <- geo_sample %>%
  select(CSO_LEA, TOTPOP22, geometry) %>%
  left_join(
    C_ij_10min %>% st_drop_geometry() %>% select(CSO_LEA, accessibility),
    by = "CSO_LEA"
  ) %>%
  left_join(
    C_ij_20min %>% st_drop_geometry() %>% select(CSO_LEA, accessibility_20),
    by = "CSO_LEA"
  ) %>%
  left_join(
    C_ij_30min %>% st_drop_geometry() %>% select(CSO_LEA, accessibility_30),
    by = "CSO_LEA"
  ) %>%
  left_join(
    geo_sample %>% st_drop_geometry() %>% select(CSO_LEA, TOTPOP22),
    by = "CSO_LEA"
  ) %>%
  # Replace NA values with 0 for areas with no intersection
  mutate(
    relative_accessibility_10min = accessibility / TOTPOP22,
    relative_accessibility_20min = accessibility_20 / TOTPOP22,
    relative_accessibility_30min = accessibility_30 / TOTPOP22
  )