library(sf)
library(tidyverse)
library(osrm)
library(leaflet)
st_drivers()

library(openrouteservice)

ors_api_key("5b3ce3597851110001cf62486cbc3afb79d04cf492c9e160d17ae49c")

# Function to generate isochrones for multiple pharmacies in batches using OpenRouteService
generate_batch_isochrones_ors <- function(pharmacies_sf, travel_time, batch_size = 5) {
  # Extract coordinates of all pharmacies (geometry column is named 'geometry')
  pharmacy_coords <- st_coordinates(pharmacies_sf) %>%
    as.data.frame() %>%
    rename(lon = X, lat = Y)  # Rename to lon and lat for proper API request format
  
  # Convert coordinates into the format expected by ORS (array of lon/lat pairs)
  locations <- as.matrix(pharmacy_coords[, c("lon", "lat")])  # Matrix with lon and lat pairs
  
  # Split the locations into batches of size 'batch_size' (5 in this case)
  num_batches <- ceiling(nrow(locations) / batch_size)
  
  # Initialize a list to store isochrones for all pharmacies
  all_isochrones <- list()
  
  for (i in 1:num_batches) {
    # Get the current batch of locations
    batch_locations <- locations[((i - 1) * batch_size + 1):min(i * batch_size, nrow(locations)), ]
    
    # Generate isochrones for the current batch
    batch_isochrones <- ors_isochrones(
      locations = batch_locations,  # Batch of [lon, lat] pairs
      profile = "driving-car",  # Travel mode: "driving-car" (can also use "walking", "cycling", etc.)
      range = rep(travel_time, nrow(batch_locations)),  # Travel time (e.g., 600s for 10 minutes)
      output = "sf"  # Return as Simple Feature object (sf)
    )
    
    # Store the result for the current batch
    all_isochrones[[i]] <- batch_isochrones
  }
  
  # Combine the results from all batches into one SF object
  combined_isochrones <- do.call(rbind, all_isochrones)
  
  return(combined_isochrones)
}

library(sf)
library(dplyr)
library(ggplot2)

# Load spatial data (Neighborhood Boundaries)
gfile <- "C:/Users/Sivagami Nedumaran/Downloads/Merged_Data_Final.shp"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE)) %>%
  filter(month == "2022 January") %>%  # Keep only January 2022 data
  st_transform(crs = 4326)  # Ensure correct CRS

# Load population data (assuming a CSV with neighborhood population & CSO_LEA)
pop_data <- read.csv("C:/Users/Sivagami Nedumaran/Downloads/LEA_POP22.csv", stringsAsFactors = FALSE)

# Standardize column names for merging
pop_data <- pop_data %>% rename(cso_lea = CSO_LEA)

# Merge population data with geo_data using cso_lea as the key
geo_data <- geo_data %>%
  left_join(pop_data, by = "cso_lea")  # Match on 'cso_lea'
dim(geo_data)
# Load pharmacy locations (opportunities)
pharmacies <- read.csv("geocoded_addresses_p_final.csv", stringsAsFactors = FALSE) %>%
  filter(grepl("COVID-19", COVID.19_Vaccines_Offered, ignore.case = TRUE))

pharmacies_sf <- st_as_sf(pharmacies, coords = c("longitude", "latitude"), crs = 4326)

# Generate 10-minute isochrones for pharmacies
isochrones <- generate_batch_isochrones_ors(pharmacies_sf, travel_time = 600)
isochrones_geometry <- isochrones %>% select(geometry)

st_write(isochrones_geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones.geojson", driver = "GeoJSON")

# Ensure valid geometries before spatial operations
geo_data <- st_make_valid(geo_data)
isochrones <- st_make_valid(isochrones)
set.seed(452)
geo_sample<- geo_data %>% sample_n(10)
head(geo_sample)
class(pop_data$TOTPOP22)
head(pop_data$TOTPOP22)
geo_sample$TOTPOP22 <- gsub(",", "", geo_sample$TOTPOP22)
geo_sample$TOTPOP22 <- as.numeric(geo_sample$TOTPOP22)
# Ensure CRS is consistent
isochrones$isochrone_id <- 1:nrow(isochrones)
isochrones_10min <- st_transform(isochrones, st_crs(geo_sample))

# Calculate intersection areas between LEAs and 10-minute isochrones
intersections_10min <- st_intersection(geo_sample, isochrones_10min)
intersections_10min$intersection_area <- st_area(intersections_10min)

# Normalize intersection areas to get C_ij

# Ensure geometry is retained from the original dataset
C_ij_10min <- intersections_10min %>%
  group_by(cso_lea, isochrone_id) %>%
  summarize(a_ij = sum(intersection_area), geometry = st_union(geometry), .groups = "drop") %>%
  group_by(cso_lea) %>%
  mutate(C_ij = a_ij / sum(a_ij)) %>%
  ungroup()

# Retain geometries when summarizing
accessibility_results <- C_ij_10min %>%
  group_by(cso_lea) %>%
  summarize(accessibility = sum(C_ij, na.rm = TRUE), geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()

C_ij_10min
accessibility_results

final_results <- geo_sample %>%
  left_join(accessibility_results, by = "cso_lea")

# Display final results
head(final_results)


