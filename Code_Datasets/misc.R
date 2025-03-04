install.packages(c("spatstat", "raster", "terra", "stars"))
library(sf)
library(spatstat)
library(raster)
library(dplyr)

# Load LEA and Pharmacy Data
lea_data <- read.csv("BC_data.csv", stringsAsFactors = FALSE)
pharmacies <- read.csv("geocoded_addresses_p_final.csv", stringsAsFactors = FALSE)

# Filter pharmacies offering COVID-19 vaccines
pharm <- pharmacies %>% filter(grepl("COVID-19", COVID.19_Vaccines_Offered, ignore.case = TRUE))

# Convert LEA and Pharmacy Data to Spatial Format
lea_sf <- st_as_sf(lea_data, coords = c("longitude", "latitude"), crs = 4326) 
pharmacies_sf <- st_as_sf(pharm, coords = c("longitude", "latitude"), crs = 4326)

# Transform to a projected CRS (meters) for distance calculations
proj_crs <- 3857  # Web Mercator (or choose a local UTM CRS)
lea_sf <- st_transform(lea_sf, proj_crs)
pharmacies_sf <- st_transform(pharmacies_sf, proj_crs)
head(pharmacies_sf)
# Convert pharmacy points to a `spatstat` ppp object
pharmacies_sp <- as(pharmacies_sf, "Spatial")

# Extract coordinates
coords <- coordinates(pharmacies_sp)

# Define the study window (bounding box of the pharmacies)
window <- as.owin(st_bbox(pharmacies_sf))

# Create a ppp object (point pattern)
pharm_ppp <- ppp(x = coords[,1], y = coords[,2], window = window)

# Check the output
print(pharm_ppp)

# Define the study window (bounding box around LEAs)
study_window <- as.owin(st_bbox(lea_sf))

# Compute Kernel Density Estimate (KDE)
kde <- density(pharm_ppp, sigma = 34000, eps = 500)  # Bandwidth = 5km, resolution = 500m

# Convert KDE result to Raster
kde_raster <- raster(kde)

# Assign KDE values to LEAs by extracting the mean KDE value per area
crs(kde_raster) <- st_crs(3857)$proj4string

lea_sf <- st_transform(lea_sf, st_crs(kde_raster))

lea_sf$accessibility_score <- extract(kde_raster, lea_sf, fun = mean, na.rm = TRUE)

# Check results
summary(lea_sf$accessibility_score)


summary(values(kde_raster))
plot(kde_raster)  # Plot the kernel density estimate
plot(lea_sf, add = TRUE, col = "red")  # Add LEA polygons to the map

########################################################################################################
# Get the extent of your data to understand the scale
extent_pharm <- st_bbox(pharmacies_sf)
print(extent_pharm)

# Calculate a reasonable bandwidth based on your data extent
# A common rule of thumb is to use 1-10% of the largest dimension
x_range <- extent_pharm["xmax"] - extent_pharm["xmin"]
y_range <- extent_pharm["ymax"] - extent_pharm["ymin"]
max_range <- max(x_range, y_range)

# Try different bandwidth options (1%, 5%, 10% of the max dimension)
bandwidth_1pct <- max_range * 0.01
bandwidth_5pct <- max_range * 0.05
bandwidth_10pct <- max_range * 0.10

print(paste("Suggested bandwidths (meters):"))

# Check CRS of each dataset
print("CRS of LEA data:")
print(st_crs(lea_sf))

print("CRS of pharmacy data:")
print(st_crs(pharmacies_sf))

print("CRS of KDE raster:")
print(crs(kde_raster))

# Ensure all are using the same projection
# If needed, transform them all to the same CRS
lea_sf <- st_transform(lea_sf, proj_crs)
pharmacies_sf <- st_transform(pharmacies_sf, proj_crs)
crs(kde_raster) <- st_crs(proj_crs)$proj4string  # Proper way to set CRS for raster

# Visualize to confirm alignment
plot(kde_raster)
plot(st_geometry(lea_sf), add = TRUE, border = "blue")
plot(st_geometry(pharmacies_sf), add = TRUE, col = "red", pch = 16)
print(paste("1% of max range:", bandwidth_1pct))
print(paste("5% of max range:", bandwidth_5pct))
print(paste("10% of max range:", bandwidth_10pct))

# Try computing KDE with a larger bandwidth
kde_larger <- density(pharm_ppp, sigma = bandwidth_5pct, eps = 500)