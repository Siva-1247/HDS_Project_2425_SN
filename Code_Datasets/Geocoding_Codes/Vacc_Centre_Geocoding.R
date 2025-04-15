#install.packages("httr")       
#install.packages("jsonlite")     

# Step 2: Load the Required Libraries
library(tidyverse)
library(httr)
library(jsonlite)


----------------------------------------------------------------------
  
  geocode_address_vaccentre <- function(address, api_key) {
    if (is.null(address) || nchar(trimws(address)) == 0) {
      return(list(latitude = NA_real_, longitude = NA_real_))
    }
    
    # Add Ireland to the address
    full_address <- paste(trimws(address), "Ireland")
    
    url <- sprintf(
      "https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s",
      URLencode(full_address, reserved = TRUE),
      api_key
    )
    
    tryCatch({
      response <- GET(url)
      result <- fromJSON(rawToChar(response$content))
      
      if (result$status == "OK") {
        lat <- as.numeric(result$results$geometry$location$lat[1])
        lng <- as.numeric(result$results$geometry$location$lng[1])
        return(list(latitude = lat, longitude = lng))
      } else {
        return(list(latitude = NA_real_, longitude = NA_real_))
      }
    }, error = function(e) {
      return(list(latitude = NA_real_, longitude = NA_real_))
    })
  }
  
  process_data <- function(data, api_key) {
    # Clean up column names
    names(data) <- gsub(" ", "_", names(data))
    
    latitudes <- numeric(nrow(data))
    longitudes <- numeric(nrow(data))
    
    for(i in 1:nrow(data)) {
      Sys.sleep(0.1)  # Rate limiting
      
      result <- geocode_address_vaccentre(
        address = data$Address[i],
        api_key = api_key
      )
      
      latitudes[i] <- result$latitude
      longitudes[i] <- result$longitude
      
      if(i %% 10 == 0) {
        cat(sprintf("Processed %d/%d addresses\n", i, nrow(data)))
      }
    }
    
    data$latitude <- latitudes
    data$longitude <- longitudes
    
    return(data)
  }
  
  # Main execution
  api_key <- "API_Key"
  
  # Read and process the data
  result_df <- read_csv("vaccination_centres.csv")
  result_df <- process_data(result_df, api_key)
  
  # Write results and display summary
  write_csv(result_df, "geocoded_addresses_vac_temp.csv")
  cat("\nSummary of geocoding results:\n")
  print(summary(result_df[c("latitude", "longitude")]))
  success_rate <- (sum(!is.na(result_df$latitude)) / nrow(result_df)) * 100
  cat(sprintf("\nSuccessfully geocoded %.1f%% of addresses\n", success_rate))
  write_csv(result_df, "geocoded_addresses_vac_final.csv")