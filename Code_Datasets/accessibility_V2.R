library(sf)
library(tidyverse)
library(osrm)
library(leaflet)
st_drivers()

##ORS for batch generation of isochrones
library(openrouteservice)

ors_api_key("5b3ce3597851110001cf62486cbc3afb79d04cf492c9e160d17ae49c")

# Function to generate isochrones for multiple pharmacies in batches using OpenRouteService
generate_batch_isochrones_ors <- function(pharmacies_sf, travel_time, batch_size = 5) {
  # Extract coordinates of all pharmacies (geometry column is named 'geometry')
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
      locations = batch_locations,  # Batch of [lon, lat] pairs
      profile = "driving-car",  # Travel mode: "driving-car" 
      range = rep(travel_time, nrow(batch_locations)),
      output = "sf"  # Return as Simple Feature object (sf)
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
gfile <- "C:/Users/Sivagami Nedumaran/Downloads/Merged_Data_Final.shp"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE)) %>%
  filter(month == "2022 January") %>%  # Keep only January 2022 data
  st_transform(crs = 4326)  # Ensure correct CRS

# Population data 
pop_data <- read.csv("C:/Users/Sivagami Nedumaran/Downloads/LEA_POP22.csv", stringsAsFactors = FALSE)

# Standardizing column names for merging
pop_data <- pop_data %>% rename(cso_lea = CSO_LEA)

# Merging population data with geo_data using cso_lea as the key
geo_data <- geo_data %>%
  left_join(pop_data, by = "cso_lea")  
dim(geo_data)

#Reading pharmacy locations

pharmacies <- read.csv("geocoded_addresses_p_final.csv", stringsAsFactors = FALSE) %>%
  filter(grepl("COVID-19", COVID.19_Vaccines_Offered, ignore.case = TRUE))

pharmacies_sf <- st_as_sf(pharmacies, coords = c("longitude", "latitude"), crs = 4326)

# Generate 10-minute isochrones for pharmacies

isochrones <- generate_batch_isochrones_ors(pharmacies_sf, travel_time = 600)
isochrones_geometry <- isochrones %>% select(geometry)

st_write(isochrones_geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones.geojson", driver = "GeoJSON")

# Ensuring valid geometries before spatial operations
geo_data <- st_make_valid(geo_data)
isochrones <- st_make_valid(isochrones)

#Sampling 10 LEAs
set.seed(452)
geo_sample<- geo_data %>% sample_n(10)
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
  group_by(cso_lea, isochrone_id) %>%
  summarize(a_ij = sum(intersection_area), geometry = st_union(geometry), .groups = "drop") %>%
  group_by(cso_lea) %>%
  mutate(C_ij = a_ij / sum(a_ij)) %>%
  ungroup()

C_ij_10min <- st_join(C_ij_10min, geo_sample %>% select(cso_lea, TOTPOP22), left = TRUE)


accessibility_results <- C_ij_10min %>%
  group_by(cso_lea.x) %>%
  summarize(
    accessibility = sum(C_ij * TOTPOP22, na.rm = TRUE),  
    geometry = st_union(geometry),  
    .groups = "drop"
  ) %>%
  st_as_sf()

C_ij_10min
accessibility_results


