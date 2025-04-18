library(sf)
library(tidyverse)
library(osrm)
library(leaflet)

##ORS for batch generation of isochrones
library(openrouteservice)

ors_api_key("YOUR API KEY")

# Function to generate isochrones for multiple pharmacies in batches using OpenRouteService
generate_batch_isochrones_ors <- function(pharmacies_sf, travel_time, batch_size = 5) {
  # Extract coordinates of all pharmacies
  pharmacy_coords <- st_coordinates(pharmacies_sf) %>%
    as.data.frame() %>%
    rename(lon = X, lat = Y)
  
  # Convert coordinates into the format expected by ORS (array of lon/lat pairs)
  locations <- as.matrix(pharmacy_coords[, c("lon", "lat")])
  
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

# Population data 
pop_data <- read.csv("Accessibility_Data\\LEA_POP22.csv", stringsAsFactors = FALSE)
names(pop_data)

# Merging population data with geo_data using cso_lea as the key
geo_data <- geo_data %>%
  left_join(pop_data, by = "CSO_LEA")  
dim(geo_data)
head(geo_data)
#Reading pharmacy locations

pharmacies <- read.csv("geocoded_addresses_p_final.csv", stringsAsFactors = FALSE) %>%
  filter(grepl("COVID-19", COVID.19_Vaccines_Offered, ignore.case = TRUE))

pharmacies_sf <- st_as_sf(pharmacies, coords = c("longitude", "latitude"), crs = 4326)

# Generate 10-minute isochrones for pharmacies

isochrones <- generate_batch_isochrones_ors(pharmacies_sf, travel_time = 600)
isochrones_geometry <- isochrones %>% select(geometry)

#st_write(isochrones_geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones.geojson", driver = "GeoJSON")

isochrones <- st_read("Accessibility_Data\\isochrones.geojson")
# Ensuring valid geometries before spatial operations
geo_data <- st_make_valid(geo_data)
isochrones <- st_make_valid(isochrones)

#Sampling 10 LEAs
set.seed(452)
geo_sample<- geo_data %>% filter(COUNTY=="GALWAY")
head(geo_sample)
class(pop_data$TOTPOP22)
head(pop_data$TOTPOP22)
geo_sample$TOTPOP22 <- gsub(",", "", geo_sample$TOTPOP22)
geo_sample$TOTPOP22 <- as.numeric(geo_sample$TOTPOP22)
isochrones$isochrone_id <- 1:nrow(isochrones)

# Ensuring CRS is consistent
isochrones_10min <- st_transform(isochrones, st_crs(geo_sample))

# Calculating intersection areas between LEAs and 10-minute isochrones
intersections_10min <- st_intersection(geo_sample, isochrones_10min)
intersections_10min$intersection_area <- st_area(intersections_10min)

# Normalizing intersection areas to get C_ij

C_ij_10min <- intersections_10min %>%
  group_by(CSO_LEA, isochrone_id) %>%
  summarize(a_ij = sum(intersection_area), geometry = st_union(geometry), .groups = "drop") %>%
  group_by(CSO_LEA) %>%
  mutate(C_ij = a_ij / sum(a_ij)) %>%
  ungroup()

C_ij_10min <- st_join(C_ij_10min, geo_sample %>% select(CSO_LEA, TOTPOP22), left = TRUE)

names(C_ij_10min)
accessibility_results <- C_ij_10min %>%
  group_by(CSO_LEA.x) %>%
  summarize(
    accessibility = sum(C_ij * TOTPOP22, na.rm = TRUE),  
    .groups = "drop"
  ) %>%
  left_join(
    C_ij_10min %>%
      group_by(CSO_LEA.x) %>%
      summarize(geometry = st_union(geometry), .groups = "drop"),
    by = "CSO_LEA.x"
  ) %>%
  st_as_sf()

#Retry
access_values <- C_ij_10min %>%
  st_drop_geometry() %>%
  group_by(CSO_LEA.x) %>%
  summarize(
    accessibility = sum(C_ij * TOTPOP22, na.rm = TRUE),
    .groups = "drop"
  )
access_values <- access_values %>%
  left_join(geo_sample %>% select(CSO_LEA, TOTPOP22), by = c("CSO_LEA.x" = "CSO_LEA")) %>%
  mutate(
    relative_accessibility = accessibility / TOTPOP22
  )
access_geoms <- C_ij_10min %>%
  group_by(CSO_LEA.x) %>%
  reframe(geometry = st_union(geometry)) %>%
  st_as_sf()

access_data <- access_values %>% left_join(access_geoms, by = "CSO_LEA.x") %>%
  st_as_sf()

ggplot(access_data) + geom_sf(aes(fill=relative_accessibility)) + scale_fill_viridis_c(
  option = "plasma",
  
  direction = -1,
  name = "Relative Accessibility"
) +
  labs(
    title = "Relative Accessibility to Services within 10 Minutes") +
  theme_minimal()