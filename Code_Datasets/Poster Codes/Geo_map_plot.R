#Display leaflet based primary vaccination dose rate for Jan 2022 with initial vaccination center locations highlighted

library(sf)
library(leaflet)
library(dplyr)

# Step 1: Read the file
gfile <- "C:/Users/Sivagami Nedumaran/Downloads/Merged_Data_Final.shp"
geo_data <- st_read(gfile)
head(geo_data)

# Step 2: Transform to longlat (WGS84) and use only Jan 2022 data
geo_data_jan <- geo_data %>%
  filter(month == "2022 January")
head(geo_data_jan)
geo_data_jan <- st_transform(geo_data_jan, crs = 4326)

vaccination_centers <- read.csv("Vacc_Rates&Geocoded_Data/Initial_Vacc.csv")

# Step 3: Defining a color palette based primary vaccination dose percentage
color_palette <- colorBin(
  palette = "RdYlGn",
  domain = geo_data_jan$prmry_cm,
  bins = 8, 
  reverse = FALSE
)

# Step 4: Creating a leaflet map
leaflet(geo_data_jan) %>%
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
  ) %>% addCircleMarkers(
    data = vaccination_centers,
    lng = ~longitude,
    lat = ~latitude,
    popup = ~paste0(Centre_Name, ", ", County),
    label = ~Centre_Name,
    color = 'black',
    radius = 3)%>%
  addLegend(
    pal = color_palette, 
    values = ~prmry_cm, 
    opacity = 0.7, 
    title = "Vaccination Percentage",
    position = "bottomright"
  )
