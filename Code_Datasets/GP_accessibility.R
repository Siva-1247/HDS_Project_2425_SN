library(openrouteservice)

simple_isochrones <- function(loc_sf, travel_time = 600, batch_size = 3, 
                              max_retries = 3, pause_time = 2, timeout = 30) {
  require(dplyr)
  require(sf)
  require(openrouteservice)
  require(httr)
  
  # Configure timeout for API requests
  httr::set_config(httr::config(timeout = timeout))
  
  # Extract coordinates
  coords_df <- st_coordinates(loc_sf) %>%
    as.data.frame() %>%
    rename(lon = X, lat = Y)
  
  successful_isochrones <- list()
  failed_points <- c()
  
  # Process each location
  for (i in 1:nrow(coords_df)) {
    cat(sprintf("Processing location %d of %d\n", i, nrow(coords_df)))
    
    current_loc <- as.matrix(coords_df[i, c("lon", "lat"), drop = FALSE])
    success <- FALSE
    
    # Try up to max_retries times
    for (attempt in 1:max_retries) {
      cat(sprintf("  Attempt %d\n", attempt))
      
      # Try to get isochrone
      result <- tryCatch({
        iso <- ors_isochrones(
          locations = current_loc,
          profile = "driving-car",
          range = travel_time,
          output = "sf"
        )
        
        # Store successful result
        successful_isochrones[[length(successful_isochrones) + 1]] <- iso
        cat("  Success!\n")
        success <- TRUE
        break
        
      }, error = function(e) {
        cat(sprintf("  Failed: %s\n", conditionMessage(e)))
        return(NULL)
      })
      
      # If successful, no need for more attempts
      if (success) break
      
      # If not successful and more retries left, pause before retrying
      if (attempt < max_retries) {
        cat(sprintf("  Pausing for %d seconds before retry...\n", pause_time))
        Sys.sleep(pause_time)
      }
    }
    
    # Record failed point if all attempts failed
    if (!success) {
      cat("  All attempts failed. Adding to failed list.\n")
      failed_points <- c(failed_points, i)
    }
    
    # Pause between batches
    if (i %% batch_size == 0 || i == nrow(coords_df)) {
      cat(sprintf("Batch complete. Pausing for %d seconds...\n", pause_time))
      Sys.sleep(pause_time)
    }
  }
  
  # Combine all successful isochrones
  if (length(successful_isochrones) > 0) {
    combined_isochrones <- do.call(rbind, successful_isochrones)
  } else {
    combined_isochrones <- st_sf(geometry = st_sfc(crs = st_crs(loc_sf)))
  }
  
  # Extract failed points as SF object
  failed_sf <- if (length(failed_points) > 0) {
    loc_sf[failed_points, ]
  } else {
    st_sf(geometry = st_sfc(crs = st_crs(loc_sf)))
  }
  
  # Print summary
  success_count <- nrow(coords_df) - length(failed_points)
  cat(sprintf("\nSummary: %d successful, %d failed (%.1f%% success rate)\n", 
              success_count, length(failed_points), 
              100 * success_count / nrow(coords_df)))
  
  # Return both successful isochrones and failed points
  return(list(
    isochrones = combined_isochrones,
    failed_points = failed_sf,
    failed_indices = failed_points
  ))
}

GPs <- read.csv("geocoded_addresses_final.csv", stringsAsFactors = FALSE)
ors_api_key("5b3ce3597851110001cf6248593ddf72ba834df08541f1fc2275615d")
gp_sf <- st_as_sf(GPs, coords = c("longitude", "latitude"), crs = 4326)
library(dplyr)
gp_sf_batch1 <- gp_sf %>% slice(1:400)
gp_sf_batch2 <- gp_sf %>% slice(401:800)
gp_sf_batch3 <- gp_sf %>% slice(801:1200)
gp_sf_batch4 <- gp_sf %>% slice(1201:1557)

isochrones_5gp_1 <- simple_isochrones (gp_sf_batch1)
isochrones_5gp_2 <- simple_isochrones (gp_sf_batch2)
isochrones_5gp_3 <- simple_isochrones (gp_sf_batch3)
isochrones_5gp_4 <- simple_isochrones (gp_sf_batch4)

isochrones_5gp_failed <- isochrones_5gp_2$failed_points
isochrones_5gp_5 <- simple_isochrones(isochrones_5gp_failed)
isochrones_5gp_failed_2 <- isochrones_5gp_4$failed_points
isochrones_5gp_6 <- simple_isochrones(isochrones_5gp_failed)

# Combine all successful isochrones
all_isochrones <- rbind(
  isochrones_5gp_1$isochrones,
  isochrones_5gp_2$isochrones,
  isochrones_5gp_3$isochrones,
  isochrones_5gp_4$isochrones,
  isochrones_5gp_5$isochrones
)

# Extract center coordinates
coords_matrix <- do.call(rbind, all_isochrones$center)
colnames(coords_matrix) <- c("center_lon", "center_lat")

# Bind the extracted coordinates into the data frame
all_isochrones_clean <- all_isochrones
all_isochrones_clean$center_lon <- coords_matrix[, 1]
all_isochrones_clean$center_lat <- coords_matrix[, 2]

# Drop the original list-column 'center'
all_isochrones_clean$center <- NULL

# Write to GeoJSON
st_write(
  all_isochrones_clean,
  "C:/Users/Sivagami Nedumaran/Downloads/isochronesgp_walk.geojson",
  driver = "GeoJSON",
  delete_dsn = TRUE
)



