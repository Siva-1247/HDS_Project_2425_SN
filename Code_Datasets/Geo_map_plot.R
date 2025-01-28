library(sf)
library(leaflet)
library(dplyr)

# Step 1: Read the file
gfile <- "Merged_Data_Final.shp"
geo_data <- st_read(gfile)
head(geo_data)
# Step 2: Transform to longlat (WGS84)
geo_data <- st_transform(geo_data, crs = 4326)

# Step 3: (Optional) Simplify the geometry to improve performance
geo_data_simplified <- st_simplify(geo_data, dTolerance = 100)
vaccination_centers <- read.csv("Initial_Vacc.csv")
# Step 4: Define a color palette based on a numeric column (e.g., vaccination percentage)
color_palette <- colorNumeric(
  palette = "PuRd",  # Change to pinks and purples
  domain = geo_data_simplified$prmry_cm
)

# Step 5: Create a leaflet map
leaflet(geo_data_simplified) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(
    fillColor = ~color_palette(prmry_cm),  # Apply color palette
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste0(cso_lea, ": ", prmry_cm, "%"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>% addMarkers(
    data = vaccination_centers,
    lng = ~longitude,
    lat = ~latitude,
    popup = ~paste0(Centre_Name, ", ", County),
    label = ~Centre_Name
  )%>%
  addLegend(
    pal = color_palette, 
    values = ~prmry_cm, 
    opacity = 0.7, 
    title = "Vaccination Percentage",
    position = "bottomright"
  )
