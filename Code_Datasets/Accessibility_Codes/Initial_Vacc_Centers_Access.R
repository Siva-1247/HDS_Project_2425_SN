library(sf)
library(tidyverse)
library(osrm)
library(leaflet)
library(htmlwidgets)
library(leaflet)
library(RColorBrewer)
library(viridis)

##ORS for batch generation of isochrones
library(openrouteservice)

ors_api_key("your api")

# Function to generate isochrones for multiple pharmacies in batches using OpenRouteService
generate_batch_isochrones_ors <- function(loc_sf, travel_time, batch_size = 2, pause = 2) {
  # Extract coordinates of all pharmacies
  loc_coords <- st_coordinates(loc_sf) %>% 
    as.data.frame() %>%
    rename(lon = X, lat = Y)
  
  # Convert coordinates into the format expected by ORS (array of lon/lat pairs)
  locations <- as.matrix(loc_coords[, c("lon", "lat")])
  
  # Split the locations into batches of size 'batch_size'
  num_batches <- ceiling(nrow(locations) / batch_size)
  
  # Initialize a list to store isochrones for all pharmacies
  all_isochrones <- list()
  
  for (i in 1:num_batches) {
    # Get the current batch of locations
    cat("Processing batch", i, "of", num_batches, "\n")
    rows <- ((i - 1) * batch_size + 1):min(i * batch_size, nrow(locations))
    batch_locations <- locations[rows, , drop = FALSE]
    
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
geo_data %>% 
  st_drop_geometry() %>% 
  group_by(CSO_LEA) %>%
  summarize(count = n())%>%
  filter(count > 1)
#Rename Athlone
geo_data %>% filter(CSO_LEA == "ATHLONE")
geo_data <- geo_data %>% mutate(CSO_LEA=case_when(CSO_LEA == "ATHLONE" & COUNTY == "WESTMEATH" ~ "ATHLONE_WESTMEATH",TRUE ~ CSO_LEA))
# Population data 
pop_data <- read.csv("Accessibility_Data\\LEA_POP22.csv", stringsAsFactors = FALSE)
names(pop_data)
# Standardize LEA names
pop_data$CSO_LEA <- ifelse(pop_data$CSO_LEA == "ATHLONE-LEA 5", "ATHLONE_WESTMEATH",
                       ifelse(pop_data$CSO_LEA == "ATHLONE-LEA 6", "ATHLONE",
                              toupper(pop_data$CSO_LEA)))  

pop_data %>% filter(CSO_LEA == "ATHLONE")

# Merging population data with geo_data using cso_lea as the key
geo_data <- geo_data %>%
  left_join(pop_data, by = "CSO_LEA")  
sum(is.na(geo_data))
head(geo_data)

#Reading Initial Vaccination Center Data

vacc_center <- read.csv("Vacc_rates&Geocoded_Data\\geocoded_addresses_vac_final.csv", stringsAsFactors = FALSE)

loc_sf <- st_as_sf(vacc_center, coords = c("longitude", "latitude"), crs = 4326)
dim(loc_sf)

# Generate 10-minute isochrones for pharmacies

#isochrones <- generate_batch_isochrones_ors(loc_sf, travel_time = 600)
#isochrones_geometry <- isochrones %>% select(geometry)
dim(isochrones)

#st_write(isochrones_geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones.geojson", driver = "GeoJSON")

#Read isochrones from Accessibility_Data

isochrones <- st_read("Accessibility_Data\\isochrones.geojson")

# Ensuring valid geometries before spatial operations
geo_data <- st_make_valid(geo_data)
isochrones <- st_make_valid(isochrones_geometry)

#Including Population
geo_sample<- geo_data
head(geo_sample)
class(pop_data$TOTPOP22)
head(pop_data$TOTPOP22)
geo_sample$TOTPOP22 <- gsub(",", "", geo_sample$TOTPOP22)
geo_sample$TOTPOP22 <- as.numeric(geo_sample$TOTPOP22)
isochrones$isochrone_id <- 1:nrow(isochrones)
dim(geo_sample)

# Ensuring CRS is consistent
isochrones_10min <- st_transform(isochrones, st_crs(geo_sample))

# Calculating intersection areas between LEAs and 10-minute isochrones
intersections_10min <- st_intersection(geo_sample, isochrones_10min)
intersections_10min$intersection_area <- st_area(intersections_10min)
dim(intersections_10min)

# Calculating total intersection areas to get accessibility measure C_ij
isochrones_area <- isochrones_10min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)
C_ij_10min <- intersections_10min %>%
  left_join(isochrones_area, by = "isochrone_id") %>%
  mutate(
    access_share = as.numeric(intersection_area / total_area)
  ) %>%
  group_by(CSO_LEA) %>%
  summarize(
    accessibility = sum(access_share, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  )%>%
  st_drop_geometry()

#LEAs not in c_ij_10mins
LEA_no_intersection <- setdiff(geo_sample$CSO_LEA, C_ij_10min$CSO_LEA)
LEA_no_intersection

#accessibility normalized by population
access_values <- C_ij_10min %>%
  left_join(
    geo_sample %>%
      st_drop_geometry() %>%
      select(CSO_LEA, TOTPOP22),
    by = "CSO_LEA"
  ) %>%
  mutate(
    relative_accessibility = accessibility / TOTPOP22
  )

access_values


#Add accessibility measures across all the different isochrones
#Add the data together

ggplot(access_values) +
  geom_sf(aes(fill = accessibility)) +
  scale_fill_viridis_c(name = "Accessibility\nper capita", option = "viridis") +
  theme_minimal() +
  labs(title = "Per Capita Accessibility by Local Electoral Area",
       subtitle = "Accessibility normalized by population (TOTPOP22)")

## 20, 30 and 60 min isochrones

#isochrones_20 <- generate_batch_isochrones_ors(loc_sf, travel_time = 1200)
#isochrones_20geometry <- isochrones_20 %>% select(geometry)
#isochrones_30 <- generate_batch_isochrones_ors(loc_sf, travel_time = 1800)
#isochrones_30geometry <- isochrones_30 %>% select(geometry)
#isochrones_60 <- generate_batch_isochrones_ors(loc_sf, travel_time = 3600)
#isochrones_60geometry <- isochrones_60 %>% select(geometry)

#st_write(isochrones_geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_iv.geojson", driver = "GeoJSON")
#st_write(isochrones_20geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_2iv.geojson", driver = "GeoJSON")
#st_write(isochrones_30geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_3iv.geojson", driver = "GeoJSON")
#st_write(isochrones_60geometry, "C:/Users/Sivagami Nedumaran/Downloads/isochrones_6iv.geojson", driver = "GeoJSON")

#Read isochrones from Accessibility_Data
isochrones_10 <- st_read("Accessibility_Data\\isochrones_iv.geojson")
isochrones_20 <- st_read("Accessibility_Data\\isochrones_2iv.geojson")
isochrones_30 <- st_read("Accessibility_Data\\isochrones_3iv.geojson")
isochrones_60 <- st_read("Accessibility_Data\\isochrones_6iv.geojson")

# Ensuring valid geometries before spatial operations
geo_data <- st_make_valid(geo_data)
isochrones_10 <- st_make_valid(isochrones_10)
isochrones_20 <- st_make_valid(isochrones_20)
isochrones_30 <- st_make_valid(isochrones_30)
isochrones_60 <- st_make_valid(isochrones_60)

geo_sample<- geo_data
head(geo_sample)
class(pop_data$TOTPOP22)
head(pop_data$TOTPOP22)
geo_sample$TOTPOP22 <- gsub(",", "", geo_sample$TOTPOP22)
geo_sample$TOTPOP22 <- as.numeric(geo_sample$TOTPOP22)
isochrones_10$isochrone_id <- 1:nrow(isochrones_10)
isochrones_20$isochrone_id <- 1:nrow(isochrones_20)
isochrones_30$isochrone_id <- 1:nrow(isochrones_30)
isochrones_60$isochrone_id <- 1:nrow(isochrones_60)

# Ensuring CRS is consistent
isochrones_10min <- st_transform(isochrones_10, st_crs(geo_sample))
isochrones_20min <- st_transform(isochrones_20, st_crs(geo_sample))
isochrones_30min <- st_transform(isochrones_30, st_crs(geo_sample))
isochrones_60min <- st_transform(isochrones_60, st_crs(geo_sample))
isochrones_20min <- isochrones_20min %>%
  group_by(isochrone_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
isochrones_30min <- isochrones_30min %>%
  group_by(isochrone_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Calculating intersection areas between LEAs and 10, 20, 30, 60-minute isochrones
intersections_10min <- st_intersection(geo_sample, isochrones_10min)
intersections_10min$intersection_area <- st_area(intersections_10min)
intersections_20min <- st_intersection(geo_sample, isochrones_20min)
intersections_20min$intersection_area <- st_area(intersections_20min)
dim(intersections_20min)
intersections_30min <- st_intersection(geo_sample, isochrones_30min)
intersections_30min$intersection_area <- st_area(intersections_30min)
intersections_60min <- st_intersection(geo_sample, isochrones_60min)
intersections_60min$intersection_area <- st_area(intersections_60min)

# Normalizing intersection areas to get C_ij
isochrones_area_10 <- isochrones_10min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)
isochrones_area_20 <- isochrones_20min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)
isochrones_area_30 <- isochrones_30min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)
isochrones_area_60 <- isochrones_60min  %>% mutate(total_area= st_area(geometry)) %>% st_drop_geometry() %>% select(isochrone_id, total_area)


C_ij_10min <- intersections_10min %>%
  left_join(isochrones_area_10, by = "isochrone_id") %>%
  mutate(access_share_10 = as.numeric(intersection_area / total_area)) %>%
  group_by(CSO_LEA) %>%
  reframe(
    accessibility_10 = sum(access_share_10, na.rm = TRUE)
  )

C_ij_20min <- intersections_20min %>%
  left_join(isochrones_area_20, by = "isochrone_id") %>%
  mutate(access_share_20 = as.numeric(intersection_area / total_area)) %>%
  group_by(CSO_LEA) %>%
  reframe(accessibility_20 = sum(access_share_20, na.rm = TRUE))

C_ij_30min <- intersections_30min %>%
  left_join(isochrones_area_30, by = "isochrone_id") %>%
  mutate(access_share_30 = as.numeric(intersection_area / total_area)) %>%
  group_by(CSO_LEA) %>%
  reframe(accessibility_30 = sum(access_share_30, na.rm = TRUE))

C_ij_60min <- intersections_60min %>%
  left_join(isochrones_area_60, by = "isochrone_id") %>%
  mutate(access_share_60 = as.numeric(intersection_area / total_area)) %>%
  group_by(CSO_LEA) %>%
  reframe(accessibility_60 = sum(access_share_60, na.rm = TRUE))


#LEAs not in c_ij_20_30_mins
LEA_no_10intersection <- setdiff(geo_sample$CSO_LEA, C_ij_20min$CSO_LEA)
LEA_no_10intersection
LEA_no_20intersection <- setdiff(geo_sample$CSO_LEA, C_ij_20min$CSO_LEA)
LEA_no_20intersection
LEA_no_30intersection <- setdiff(geo_sample$CSO_LEA, C_ij_30min$CSO_LEA)
LEA_no_30intersection
LEA_no_60intersection <- setdiff(geo_sample$CSO_LEA, C_ij_60min$CSO_LEA)
LEA_no_60intersection

#Final accessibility score not normalized by population as it lead to misleading scores for densely populated areas
#Only final sum of intersected area as accessibility score - inversely weighted by travel times
combined_access_values <- geo_sample %>%
  select(CSO_LEA, TOTPOP22, geometry) %>%
  left_join(C_ij_10min, by = "CSO_LEA") %>%
  left_join(C_ij_20min, by = "CSO_LEA") %>%
  left_join(C_ij_30min, by = "CSO_LEA") %>%
  left_join(C_ij_60min, by = "CSO_LEA") %>%
  mutate(
    accessibility_10 = ifelse(is.na(accessibility_10), 0, accessibility_10),
    accessibility_20 = ifelse(is.na(accessibility_20), 0, accessibility_20),
    accessibility_30 = ifelse(is.na(accessibility_30), 0, accessibility_30),
    accessibility_60 = ifelse(is.na(accessibility_60), 0, accessibility_60),
    weighted_accessibility = (
      ( accessibility_10 / 10) +
        ( accessibility_20 / 20) +
        ( accessibility_30 / 30) +
        ( accessibility_60 / 60)
    ) / (
      (1 / 10) + (1 / 20) + (1 / 30) + (1 / 60)
    )
  )

combined_access_values



#Plotting weighted isochrone accessibility

ggplot(combined_access_values) + geom_sf(aes(fill=weighted_accessibility)) + scale_fill_viridis_c(
  option = "plasma",
  direction = -1,
  name = "Accessibility"
) + geom_sf(data = loc_sf, color = "white", size = 2, shape = 21, fill = "white", stroke = 1) +
  labs(
    title = "Accessibility to Services within 10, 20, 30 and 60 mins") +
  theme_minimal()

selected_lea <- geo_sample %>% filter(CSO_LEA == "GALWAY CITY EAST")

# Filter 10 min intersections inside Galway City East
int_10 <- intersections_10min %>% filter(CSO_LEA == "GALWAY CITY EAST")
ggplot() +
  geom_sf(data = selected_lea, fill = NA, color = "black", size = 1) +
  geom_sf(data = int_10, aes(fill = "10 min"), alpha = 0.4) +
  theme_minimal() +
  labs(title = "Isochrone Intersections in Galway City East")

center <- loc_sf[1, ]

# Extract lon/lat coordinates
coords <- st_coordinates(center)

# Carlow center's isochrone visuaization
iso_10min_Carlow_Vacc <- ors_isochrones(
  locations = matrix(coords, nrow = 1),
  profile = "driving-car",
  range = 600,
  output = "sf"
)
iso_20min_Carlow_Vacc <- ors_isochrones(
  locations = matrix(coords, nrow = 1),
  profile = "driving-car",
  range = 1200,
  output = "sf"
)
iso_30min_Carlow_Vacc <- ors_isochrones(
  locations = matrix(coords, nrow = 1),
  profile = "driving-car",
  range = 1800,
  output = "sf"
)
iso_60min_Carlow_Vacc <- ors_isochrones(
  locations = matrix(coords, nrow = 1),
  profile = "driving-car",
  range = 3600,
  output = "sf"
)

carlow_leas <- geo_data %>%
  filter(str_detect(tolower(CSO_LEA), "carlow"))

#LEAs that touch Carlow
selected_leas <- geo_data %>%
  filter(
    str_detect(tolower(CSO_LEA), "athy") |
      str_detect(tolower(CSO_LEA), "carlow") |
      str_detect(tolower(CSO_LEA), "tullow") |
      str_detect(tolower(CSO_LEA), "graiguecullen-portarlington")|
      str_detect(tolower(CSO_LEA), "baltinglass")|
      str_detect(tolower(CSO_LEA), "muinebeag") |
      str_detect(tolower(CSO_LEA), "castlecomer")|
      str_detect(tolower(CSO_LEA), "kilkenny")
  )

ggplot() +
  geom_sf(data = selected_leas, fill = "white", color = "black", alpha = 0.5) +
  geom_sf(data = iso_60min_Carlow_Vacc, aes(fill = "60 min"), alpha = 0.3, color = NA) +
  geom_sf(data = iso_30min_Carlow_Vacc, aes(fill = "30 min"), alpha = 0.4, color = NA) +
  geom_sf(data = iso_20min_Carlow_Vacc, aes(fill = "20 min"), alpha = 0.5, color = NA) +
  geom_sf(data = iso_10min_Carlow_Vacc, aes(fill = "10 min"), alpha = 0.6, color = NA) +
  
  geom_sf(data = center, color = "red", size = 3) +
  geom_sf_text(data = selected_leas, aes(label = CSO_LEA), size = 2, color = "black") +
  
  scale_fill_manual(
    name = "Isochrone Time",
    values = c(
      "10 min" = "#08519c",
      "20 min" = "#3182bd",
      "30 min" = "#6baed6",
      "60 min" = "#9ecae1"
    )
  ) +
  labs(title = "Driving Time Isochrones Around Carlow Vaccination Center") +
  theme_minimal() +
  theme(legend.position = "right")



#Function to carry out accessibility measure calculation efficiently
calculate_accessibility_per_isochrone <- function(geo_sample, isochrone_sf, time_label) {
  isochrone_sf$isochrone_id <- 1:nrow(isochrone_sf)
  
  # Ensure geometries are valid
  geo_sample <- st_make_valid(geo_sample)
  isochrone_sf <- st_make_valid(isochrone_sf)
  
  # Transform to common CRS
  isochrone_sf <- st_transform(isochrone_sf, st_crs(geo_sample))
  
  # Compute intersection
  intersections <- st_intersection(geo_sample, isochrone_sf)
  intersections$intersection_area <- st_area(intersections)
  
  # Total area per isochrone
  iso_areas <- isochrone_sf %>%
    mutate(total_area = st_area(geometry)) %>%
    st_drop_geometry() %>%
    select(isochrone_id, total_area)
  
  # Join and compute share
  C_ij <- intersections %>%
    left_join(iso_areas, by = "isochrone_id") %>%
    mutate(access_share = as.numeric(intersection_area / total_area)) %>%
    group_by(CSO_LEA) %>%
    reframe(!!paste0("accessibility_", time_label) := sum(access_share, na.rm = TRUE))
  
  return(C_ij)
}


#Pharmacy accessibility - 5, 10 and 20 minute isochrones
isochrones_p5 <- st_read("C:/Users/Sivagami Nedumaran/Downloads/isochronesp05.geojson")
isochrones_p10 <- st_read("C:/Users/Sivagami Nedumaran/Downloads/isochronesp10.geojson")
isochrones_p20 <- st_read("C:/Users/Sivagami Nedumaran/Downloads/isochronesp20.geojson")
isochrones_list_p = list("5" = isochrones_p5,"10" = isochrones_p10,"20" = isochrones_p20)
geo_sample <- st_make_valid(geo_sample)

#Accessibility measure calculation for 5 and 10 minute isochrones
isochrones_p5 <- st_make_valid(isochrones_p5)
access_5 <- calculate_accessibility_per_isochrone(geo_sample, isochrones_p5, "5")
st_write(access_5, "C:/Users/Sivagami Nedumaran/Downloads/access_5p.gpkg", delete_dsn = TRUE)
access_10 <- calculate_accessibility_per_isochrone(geo_sample, isochrones_p10, "10")
access_5 <- st_read("C:/Users/Sivagami Nedumaran/Downloads/access_5p.gpkg")
st_write(access_10, "C:/Users/Sivagami Nedumaran/Downloads/access_10p.gpkg", delete_dsn = TRUE)
access_p10 <- st_read("C:/Users/Sivagami Nedumaran/Downloads/access_10p.gpkg")
access_p10 <- access_p10 %>% rename(accessibility_Pharmacy10 = accessibility_10)

#Combining accessibility measure for initial vaccination center and 10 min pharmacy drive time
combined_access_values_LEA <- combined_access_values %>%
  left_join(access_p10, by = "CSO_LEA")
combined_access_values_LEA <- combined_access_values_LEA %>% select(-accessibility_10, -accessibility_20, -accessibility_30, -accessibility_60, -TOTPOP22)
combined_access_values_LEA <- combined_access_values_LEA %>% rename(Wt_accessibility_Initial_Vacc = weighted_accessibility )
st_write(combined_access_values, "C:/Users/Sivagami Nedumaran/Downloads/combined_access_values.gpkg", delete_dsn = TRUE)
st_write(combined_access_values_LEA, "combined_access_values_LEA.gpkg", delete_dsn = TRUE)

#Visualize pharmacy accessibility measure
ggplot(combined_access_values_LEA) + geom_sf(aes(fill=accessibility_Pharmacy10)) + scale_fill_viridis_c(
  option = "plasma",
  direction = -1,
  name = "Accessibility"
)+
  labs(
    title = "Accessibility to Pharmacy within a 10 min drive") +
  theme_minimal()

combined_access_values_LEA <- st_transform(combined_access_values_LEA, 4326)

#Testing html widget for accessibility plots
basic_test <- leaflet(combined_access_values_LEA) %>%
  addTiles() %>%
  addPolygons()

print("Testing basic polygons...")
basic_test

if(exists("basic_test")) {
  
  # Create color palette (matching your ggplot exactly)
  pal <- colorNumeric(
    palette = rev(plasma(100)),  # Reversed plasma like your ggplot
    domain = combined_access_values_LEA$accessibility_Pharmacy10,
    na.color = "#808080"
  )
  
  # Full styled map
  accessibility_map <- leaflet(combined_access_values_LEA) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(accessibility_Pharmacy10),
      weight = 1,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      popup = ~paste0(
        "<strong>", CSO_LEA, "</strong><br/>",
        "Accessibility: ", round(accessibility_Pharmacy10, 2)
      ),
      highlight = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~accessibility_Pharmacy10,
      title = "Pharmacy Accessibility 10 min drive",
      position = "bottomright",
      opacity = 0.7
    )
  
  # Display the styled map
  accessibility_map
}
saveWidget(accessibility_map, file = "accessibility_map.html", selfcontained = TRUE)

#Visualization for initial vaccination center accessibility
pal1 <- colorNumeric(
  palette = rev(plasma(100)),  # Reversed plasma like your ggplot
  domain = combined_access_values_LEA$Wt_accessibility_Initial_Vacc,
  na.color = "#808080"
)
accessibility_IVmap <- leaflet(combined_access_values_LEA) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal1(Wt_accessibility_Initial_Vacc),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    popup = ~paste0(
      "<strong>", CSO_LEA, "</strong><br/>",
      "Accessibility: ", round(Wt_accessibility_Initial_Vacc, 2)
    ),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>% addCircleMarkers(
    data = loc_sf,
    radius = 4,
    color = "black",
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~paste("Vaccination Center:", Centre_Name)  
  ) %>% addCircleMarkers(
    data = top20_access,
    lng = ~st_coordinates(st_centroid(geometry))[,1],
    lat = ~st_coordinates(st_centroid(geometry))[,2],
    radius = 6,
    color = "#FFA500",       # soft orange border
    fillColor = "red", # subtle fill
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0("<strong>Top LEA: ", CSO_LEA, "</strong><br>",
                    "Accessibility: ", round(Wt_accessibility_Initial_Vacc, 2)),
    group = "Top 20 LEAs"
  ) %>% 
  addLegend(
    pal = pal,
    values = ~Wt_accessibility_Initial_Vacc,
    title = "Accessibility to Vaccination Center within 10, 20, 30 and 60 mins",
    position = "bottomright",
    opacity = 0.7
  )


accessibility_IVmap

#Visualization for initial vaccination center accessibility - top 20 centers

top20_access <- combined_access_values_LEA %>%
  arrange(desc(Wt_accessibility_Initial_Vacc)) %>%
  slice(1:20)

ggplot(top10_access, aes(x = reorder(CSO_LEA, Wt_accessibility_Initial_Vacc), 
                         y = Wt_accessibility_Initial_Vacc)) +
  geom_col(fill = "#2b8cbe") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(top10_access$Wt_accessibility_Initial_Vacc)), by = 0.5)
  )+
  labs(
    title = "Top 10 LEAs by Accessibility to Vaccination Centers",
    x = "LEA",
    y = "Weighted Accessibility Score"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()
  )

#Map of GP isochrones
leaflet() %>%
  addTiles() %>%
  
  # 10-minute GP isochrones
  addPolygons(
    data = all_isochrones_fixed,
    fillColor = "#9ecae1",
    fillOpacity = 0.6,
    color = "#3182bd",
    weight = 1,
    popup = ~paste0("Center: ", center, "<br>Group: ", group_index)
  ) %>%
  
  # GP location points
  addCircleMarkers(
    data = gp_sf,
    radius = 4,
    fillColor = "darkred",
    fillOpacity = 0.5,
    stroke = FALSE,
    popup = ~paste0("GP: ", GP_Name)  # Replace 'Name' with actual column
  ) %>%
    addLegend(
    position = "bottomright",
    colors = "#9ecae1",
    labels = "10-Min GP Isochrone",
    opacity = 0.4,
    title = "GP Accessibility"
  )


# Extract the failed points for GPs
failed_gps <- isochrones_5gp_7$failed_points
st_write(failed_gps, "failed_gps.gpkg", delete_dsn = TRUE)
leaflet() %>%
  addTiles() %>%
  
  # Failed GP points added
  addCircleMarkers(
    data = failed_gps,
    radius = 8,
    fillColor = "orange",
    color = "red",
    weight = 2,
    fillOpacity = 0.9,
    popup = ~paste0("<strong>", GP_Name, "</strong><br>",
                    GP_Address, "<br>",
                    "<a href='", GP_LocationLink, "' target='_blank'>Google Maps</a>")
  ) %>%
    addLabelOnlyMarkers(
    data = failed_gps,
    label = ~GP_Name,
    labelOptions = labelOptions(noHide = TRUE, direction = "top", 
                                textsize = "12px", textOnly = TRUE)
  )
