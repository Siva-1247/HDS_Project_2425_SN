library(sf)
library(tidyverse)
library(osrm)
library(leaflet)

# Load the spatial data (neighborhood boundaries)
gfile <- "C:/Users/Sivagami Nedumaran/Downloads/Merged_Data_Final.shp"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE))

# Filter for January 2022
geo_data_jan <- geo_data %>%
  filter(month == "2022 January")
geo_data_jan <- st_transform(geo_data_jan, crs = 4326)

geo_data_jan <- geo_data_jan %>%
  mutate(neighborhood_area = st_area(.))

pharmacies <- read.csv("geocoded_addresses_p_final.csv", stringsAsFactors = FALSE)
pharmacies$COVID.19_Vaccines_Offered
pharm <- pharmacies %>% filter(grepl("COVID-19", COVID.19_Vaccines_Offered, ignore.case = TRUE))
pharmacies_sf <- st_as_sf(pharm, coords = c("longitude", "latitude"), crs = 4326)

head(pharmacies_sf)
str(pharmacies_sf)

# Load necessary libraries
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

# Example: Generate isochrones for all pharmacies (e.g., 10-minute isochrones)
isochrones <- generate_batch_isochrones_ors(pharmacies_sf, 600)

library(ggplot2)
ggplot() +
  geom_sf(data = isochrones, aes(fill = factor(value)), alpha = 0.5) +
  scale_fill_manual(values = c("blue", "green", "red")) +
  ggtitle("10-Minute Isochrones for Pharmacies")

set.seed(420)  # For reproducibility
# Compute total area of each neighborhood
geo_data_jan <- geo_data_jan %>%
  mutate(neighborhood_area = st_area(.))
random_neighborhoods <- geo_data_jan %>%
  sample_n(10)
head(random_neighborhoods)
# Perform spatial intersection to find areas covered by isochrones
# Ensure geometries are valid
random_neighborhoods <- st_make_valid(random_neighborhoods)
isochrones <- st_make_valid(isochrones)

# Perform spatial intersection again
accessibility <- st_intersection(random_neighborhoods, isochrones)

str(accessibility)
unique(accessibility$lea_id)


# Compute the area covered by isochrones in each neighborhood
geo_data_jan_clean <- geo_data_jan %>%
  group_by(lea_id) %>%
  summarize(neighborhood_area = mean(neighborhood_area, na.rm = TRUE))  # Adjust aggregation as needed

accessibility_summary <- accessibility %>%
  group_by(lea_id) %>%
  summarize(covered_area = sum(st_area(geometry))) %>%
  left_join(geo_data_jan_clean, by = "lea_id") %>%
  mutate(coverage_percentage = (covered_area / neighborhood_area) * 100)
