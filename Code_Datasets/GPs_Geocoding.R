#install.packages("httr")       
#install.packages("jsonlite")     

# Step 2: Load the Required Libraries
library(tidyverse)
library(httr)
library(jsonlite)

data <- read_csv("gp_data_final.csv")
----------------------------------------------------------------------
  
  geocode_address <- function(address, api_key) {
    if (is.null(address) || nchar(trimws(address)) == 0) {
      return(data.frame(latitude = NA_real_, longitude = NA_real_))
    }
    
    if (!grepl("Ireland", address, ignore.case = TRUE)) {
      address <- paste(address, "Ireland")
    }
    
    url <- sprintf(
      "https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s",
      URLencode(address, reserved = TRUE),
      api_key
    )
    
    tryCatch({
      response <- GET(url)
      result <- fromJSON(rawToChar(response$content))
      
      if (result$status == "OK") {
        lat <- as.numeric(result$results$geometry$location$lat[1])
        lng <- as.numeric(result$results$geometry$location$lng[1])
        return(list(latitude = lat, longitude = lng))  # Return as list instead of data frame
      } else {
        return(list(latitude = NA_real_, longitude = NA_real_))
      }
    }, error = function(e) {
      return(list(latitude = NA_real_, longitude = NA_real_))
    })
  }


process_data <- function(data, api_key) {
  
  latitudes <- numeric(nrow(data))
  longitudes <- numeric(nrow(data))
  
  for(i in 1:nrow(data)) {
    Sys.sleep(0.1)  # Rate limiting
    
    result <- geocode_address(data$GP_Address[i], api_key)
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


api_key <- "API_Key"  # Replace with your API key

result_df <- data

result_df <- process_data(result_df, api_key)

write_csv(result_df, "geocoded_addresses_temp.csv")


cat("\nSummary of geocoding results:\n")
print(summary(result_df[c("latitude", "longitude")]))

success_rate <- (sum(!is.na(result_df$latitude)) / nrow(result_df)) * 100
cat(sprintf("\nSuccessfully geocoded %.1f%% of addresses\n", success_rate))

write_csv(result_df, "geocoded_addresses_final.csv")