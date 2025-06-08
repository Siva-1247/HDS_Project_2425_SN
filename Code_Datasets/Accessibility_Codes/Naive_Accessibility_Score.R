#install.packages("openrouteservice") - LEA centroid based measures
#Load libraries
library(sf)
library(osrm)
library(dplyr)

# Load LEA and pharmacy data
lea_data <- read.csv("Vacc_Rates&Geocoded_Data/BC_data.csv", stringsAsFactors = FALSE)
pharmacies <- read.csv("Vacc_Rates&Geocoded_Data/geocoded_addresses_p_final.csv", stringsAsFactors = FALSE)
pharmacies$COVID.19_Vaccines_Offered
pharm <- pharmacies %>% filter(grepl("COVID-19", COVID.19_Vaccines_Offered, ignore.case = TRUE))
head(pharm)
head(lea_data)

# Convert to spatial format
lea_sf <- st_as_sf(lea_data, coords = c("longitude", "latitude"), crs = 4326)
pharmacies_sf <- st_as_sf(pharm, coords = c("longitude", "latitude"), crs = 4326)
head(lea_data)
nrow(pharmacies_sf)
lea_sf$centroid <- st_centroid(lea_sf$geometry)

# Extract longitude & latitude of the centroid
lea_sf$centroid_longitude <- st_coordinates(lea_sf$centroid)[, 1]
lea_sf$centroid_latitude <- st_coordinates(lea_sf$centroid)[, 2]

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
  palette = "YlOrRd",  
  domain = geo_data_jan$has_pharmacy_access,  #
  bins = 10,  
  pretty = TRUE)

leaflet(geo_data_jan) %>%
  addProviderTiles() 
  addPolygons(
    fillColor = ~color_palette(has_pharmacy_access),  # Color polygons based on pharmacy access bins
    weight = 1,  # Border width
    color = "black",  # Border color
    opacity = 1,  # Border opacity
    fillOpacity = 0.7,  # Polygon fill opacity
    popup = ~paste(
      "LEA: ", geo_data_jan$cso_lea, "<br>",
      "Pharmacy Count (5km): ", has_pharmacy_access
    )  # Popup showing the pharmacy count
  ) %>%
  addLegend(
    "bottomright", 
    pal = color_palette,  
    values = geo_data_jan$has_pharmacy_access, 
    title = "Pharmacy Access (Count within 20km)",
    opacity = 1
  )

################################################################################################
library(sf)
library(osrm)
library(dplyr)
pop_data <- read.csv("C:/Users/Sivagami Nedumaran/Downloads/LEA_POP22.csv")
lea_sf <- lea_sf %>%
  rename(CSO_LEA = cso_lea)
lea_sf <- lea_sf %>%
  left_join(pop_data, by = "CSO_LEA")
lea_sf$TOTPOP22 <- as.numeric(lea_sf$TOTPOP22)

# Initialize a vector for accessibility scores
lea_accessibility <- numeric(nrow(lea_sf))

# Compute Pharmacy-to-Population Ratio (Rj)
pharmacy_ratios <- numeric(nrow(pharmacies_sf))

for (j in 1:nrow(pharmacies_sf)) {
  accessible_leas <- numeric(nrow(lea_sf))  # Ensure it's a numeric vector
  
  for (i in 1:nrow(lea_sf)) {
    route <- retry_osrmRoute(lea_sf$centroid[i], pharmacies_sf[j,])
    
    if (!is.null(route) && !is.na(route$duration) && route$duration < 30*60) {
      # Ensure population is numeric before assignment
      if (!is.na(lea_sf$TOTPOP22[i])) {
        accessible_leas[i] <- lea_sf$TOTPOP22[i]  # Assign population for accessible LEAs
      }
    }
  }
  
  # Compute supply-to-demand ratio (Rj) - sum of accessible population (handle NA values)
  pharmacy_ratios[j] <- ifelse(sum(accessible_leas, na.rm = TRUE) == 0, NA, 1 / sum(accessible_leas, na.rm = TRUE))
}

# Step 2: Compute Accessibility Score (Ai)
lea_accessibility <- numeric(nrow(lea_sf))  # Initialize the vector to store accessibility scores

for (i in 1:nrow(lea_sf)) {
  accessibility_score <- 0  # Initialize accessibility score for each LEA
  
  for (j in 1:nrow(pharmacies_sf)) {
    route <- retry_osrmRoute(lea_sf$centroid[i], pharmacies_sf[j,])
    
    # If route is valid and duration is within the threshold
    if (!is.null(route) && !is.na(route$duration) && route$duration < 30*60) {
      # Add the pharmacy ratio to the accessibility score
      accessibility_score <- accessibility_score + pharmacy_ratios[j]
    }
  }
  
  lea_accessibility[i] <- accessibility_score  # Store the accessibility score for each LEA
}

# Assign the accessibility scores to the LEA data frame
lea_sf$accessibility_score <- lea_accessibility
summary(lea_sf$accessibility_score)


##############################################################################################

# Define travel time threshold (minutes)
travel_time_threshold <- 15 * 60  # Convert to seconds

# Create an empty vector to store pharmacy counts
lea_sf$pharmacy_count <- 0

for (i in 1:nrow(lea_sf)) {
  count <- 0
  for (j in 1:nrow(pharmacies_sf)) 
    {
    # Calculate travel time between LEA centroid and pharmacy
    route <- retry_osrmRoute(lea_sf[i,], pharmacies_sf[j,])
    
    # If travel time is within the threshold, count this pharmacy
    if (!is.null(route) && route$duration < travel_time_threshold) {
      count <- count + 1
    }
  }
  lea_sf$pharmacy_count[i] <- count  # Store pharmacy count for each LEA
}

# Pharmacy count per LEA
lea_sf$FCA_score <- lea_sf$pharmacy_count