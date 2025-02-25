#install.packages("osrm")

library(sf)
library(osrm)
library(dplyr)

# Load LEA and pharmacy data
lea_data <- read.csv("BC_data.csv", stringsAsFactors = FALSE)
pharmacies <- read.csv("geocoded_addresses_p_final.csv", stringsAsFactors = FALSE)
pharmacies$COVID.19_Vaccines_Offered
pharm <- pharmacies %>% filter(grepl("COVID-19", COVID.19_Vaccines_Offered, ignore.case = TRUE))
head(pharm)
# Convert to spatial format
lea_sf <- st_as_sf(lea_data, coords = c("longitude", "latitude"), crs = 4326)
pharmacies_sf <- st_as_sf(pharm, coords = c("longitude", "latitude"), crs = 4326)
nrow(lea_sf)
nrow(pharmacies_sf)
nrow(pharm)

## 15/30 min travle times from LEAs give no access, makes sense, so approach must be diff
retry_osrmRoute <- function(src, dst, max_retries = 3) {
  attempt <- 1
  while (attempt <= max_retries) {
    try({
      route <- osrmRoute(src = src, dst = dst)
      return(route)
    }, silent = TRUE)
    
    # Retry if it fails
    Sys.sleep(5)  # Wait for 5 seconds before retrying
    attempt <- attempt + 1
  }
  
  # Return NULL if all attempts fail
  return(NULL)
}

# Loop to calculate the travel time
lea_with_pharmacy_access <- numeric(nrow(lea_sf))

for(i in 1:nrow(lea_sf)) {
  for(j in 1:nrow(pharmacies_sf)) {
    # Calculate route time between LEA centroid and pharmacy
    route <- retry_osrmRoute(lea_sf[i,], pharmacies_sf[j,])
    if(!is.null(route) && route$duration < 30*60) {  # Less than 15 minutes
      lea_with_pharmacy_access[i] <- 1
      break  # Stop after finding a valid route
    }
  }
}

# Add this information to the LEA data
lea_sf$has_pharmacy_access <- lea_with_pharmacy_access
summary(lea_sf$has_pharmacy_access)
#######################################################
## No.of pharmacies within 5 km radius of a centroid
pharmacies_buffer <- st_buffer(pharmacies_sf, dist = 5000)

# Check if LEA centroids are within the 5 km buffer zone of any pharmacy
intersects_matrix <- st_intersects(lea_sf, pharmacies_buffer, sparse = FALSE)
lea_sf$has_pharmacy_access <- apply(intersects_matrix, 1, function(x) sum(x))



gfile <- "C:/Users/Sivagami Nedumaran/Downloads/Merged_Data_Final.shp"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE))

# Filter for "2022 January" data
geo_data_jan <- geo_data %>%
  filter(month == "2022 January")

# Transform to WGS84 CRS (in case it's not already)
geo_data_jan <- st_transform(geo_data_jan, crs = 4326)

# Check the structure of the data to see the fields (LEA name, geometry)
head(geo_data_jan)

lea_sf$cso_lea <- as.character(lea_sf$cso_lea)
geo_data_jan$cso_lea <- as.character(geo_data_jan$cso_lea)

# Merge the pharmacy access information with the polygon data
geo_data_jan <- st_join(geo_data_jan, lea_sf %>% select(cso_lea, has_pharmacy_access))

# Check the merged data
head(geo_data_jan)

color_palette <- colorBin(
  palette = "YlOrRd",  # Yellow -> Orange -> Red gradient
  domain = geo_data_jan$has_pharmacy_access,  # Numeric range of pharmacy access
  bins = 10,  # Define 10 bins for better visualization
  pretty = TRUE  # Ensure rounded breakpoints for readability
)

leaflet(geo_data_jan) %>%
  addProviderTiles("OpenStreetMap") %>%  # Choose base map tiles
  addPolygons(
    fillColor = ~color_palette(has_pharmacy_access),  # Color polygons based on pharmacy access bins
    weight = 1,  # Border width
    color = "black",  # Border color
    opacity = 1,  # Border opacity
    fillOpacity = 0.7,  # Polygon fill opacity
    popup = ~paste(
      "LEA: ", geo_data_jan$cso_lea, "<br>",
      "Pharmacy Count (20km): ", has_pharmacy_access
    )  # Popup showing the pharmacy count
  ) %>%
  addLegend(
    "bottomright", 
    pal = color_palette,  
    values = geo_data_jan$has_pharmacy_access, 
    title = "Pharmacy Access (Count within 20km)",
    opacity = 1
  )