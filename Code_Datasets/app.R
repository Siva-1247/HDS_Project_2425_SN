library(shiny)
library(leaflet)
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(stringr)
library(htmlwidgets)
library(shinythemes)

#Data
gfile <- "combined_access_values_LEA.gpkg"
combined_access_values_LEA <- suppressWarnings(st_read(gfile, quiet = TRUE))
all_isochrones_fixed <- st_read("C:/Users/Sivagami Nedumaran/Downloads/all_isochrones.geojson")

# Check the geometry column name and fix the top20_access calculation
geom_col <- attr(combined_access_values_LEA, "sf_column")
print(paste("Geometry column name:", geom_col))

top20_access <- combined_access_values_LEA %>%
  arrange(desc(Wt_accessibility_Initial_Vacc)) %>%
  slice(1:20) %>%
  mutate(centroid = st_centroid(st_geometry(.))) %>%
  mutate(lon = st_coordinates(centroid)[, 1],
         lat = st_coordinates(centroid)[, 2])

# Also create top10_access for the plot
top10_access <- combined_access_values_LEA %>%
  arrange(desc(Wt_accessibility_Initial_Vacc)) %>%
  slice(1:10)

names(top20_access)

GPs <- read.csv("geocoded_addresses_final.csv", stringsAsFactors = FALSE)
gp_sf <- st_as_sf(GPs, coords = c("longitude", "latitude"), crs = 4326)
failed_gps <- st_read("failed_gps.gpkg")

vacc_center <- read.csv("Vacc_rates&Geocoded_Data\\geocoded_addresses_vac_final.csv", stringsAsFactors = FALSE)

loc_sf <- st_as_sf(vacc_center, coords = c("longitude", "latitude"), crs = 4326)
center <- loc_sf[1, ]

# Extract lon/lat coordinates
coords <- st_coordinates(center)

# Check if openrouteservice package is loaded
if (!require(openrouteservice, quietly = TRUE)) {
  stop("Please install and load the openrouteservice package")
}

# Generate the isochrones (assuming ors_isochrones function is available)
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

# Fix: Use combined_access_values_LEA instead of undefined geo_data
carlow_leas <- combined_access_values_LEA %>%
  filter(str_detect(tolower(CSO_LEA), "carlow"))

#LEAs that touch Carlow
selected_leas <- combined_access_values_LEA %>%
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

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Healthcare Accessibility Mapping Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore isochrones and accessibility across Carlow and neighbouring LEAs."),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Isochrones Map",
                 leafletOutput("isoMap", height = "600px")),
        
        tabPanel("Accessibility: Pharmacy 10-min",
                 leafletOutput("pharmacyMap", height = "600px")),
        
        tabPanel("Accessibility: Initial Vaccination",
                 leafletOutput("vaccinationMap", height = "600px")),
        
        tabPanel("Top 10 LEAs Accessibility Plot",
                 plotOutput("top10Plot", height = "500px")),
        
        tabPanel("GP Isochrones",
                 leafletOutput("gpIsoMap", height = "600px")),
        
        tabPanel("Failed GP Isochrones",
                 leafletOutput("failedGPMap", height = "600px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$isoMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = selected_leas, color = "black", fillOpacity = 0.2) %>%
      addPolygons(data = iso_60min_Carlow_Vacc, fillColor = "#9ecae1", fillOpacity = 0.3) %>%
      addPolygons(data = iso_30min_Carlow_Vacc, fillColor = "#6baed6", fillOpacity = 0.4) %>%
      addPolygons(data = iso_20min_Carlow_Vacc, fillColor = "#3182bd", fillOpacity = 0.5) %>%
      addPolygons(data = iso_10min_Carlow_Vacc, fillColor = "#08519c", fillOpacity = 0.6) %>%
      addCircleMarkers(data = center, color = "red", radius = 6, label = "Carlow Vaccination Center")
  })
  
  output$pharmacyMap <- renderLeaflet({
    pal <- colorNumeric(rev(plasma(100)), domain = combined_access_values_LEA$accessibility_Pharmacy10, na.color = "#808080")
    leaflet(combined_access_values_LEA) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(accessibility_Pharmacy10),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste0("<strong>", CSO_LEA, "</strong><br/>Accessibility: ", round(accessibility_Pharmacy10, 2))
      ) %>%
      addLegend(pal = pal, values = ~accessibility_Pharmacy10, title = "Pharmacy Accessibility", position = "bottomright")
  })
  
  output$vaccinationMap <- renderLeaflet({
    pal1 <- colorNumeric(rev(plasma(100)),
                         domain = combined_access_values_LEA$Wt_accessibility_Initial_Vacc,
                         na.color = "#808080")
    
    leaflet(combined_access_values_LEA) %>%
      addTiles() %>%
      
      # LEA polygons shaded by accessibility
      addPolygons(
        fillColor = ~pal1(Wt_accessibility_Initial_Vacc),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste0("<strong>", CSO_LEA, "</strong><br/>Accessibility: ", round(Wt_accessibility_Initial_Vacc, 2))
      ) %>%
      
      # Vaccination centers
      addCircleMarkers(
        data = loc_sf,
        radius = 4,
        color = "black",
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste("Vaccination Center:", Centre_Name)
      ) %>%
      
      # Top 20 LEAs - highlight with orange borders + red centroids
      addPolygons(
        data = top20_access,
        color = "#FFA500",
        weight = 2,
        fillOpacity = 0,
        popup = ~paste0("<strong>Top LEA: ", CSO_LEA, "</strong><br>Accessibility: ", round(Wt_accessibility_Initial_Vacc, 2))
      ) %>%
      addCircleMarkers(
        data = top20_access,
        lng = ~lon,
        lat = ~lat,
        radius = 6,
        color = "#FFA500",
        fillColor = "red",
        fillOpacity = 0.6,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0("<strong>Top LEA: ", CSO_LEA, "</strong><br>Accessibility: ", round(Wt_accessibility_Initial_Vacc, 2))
      ) %>%
      
      # Legend
      addLegend(pal = pal1,
                values = ~Wt_accessibility_Initial_Vacc,
                title = "Vaccination Accessibility",
                position = "bottomright")
  })
  
  output$top10Plot <- renderPlot({
    ggplot(top10_access, aes(x = reorder(CSO_LEA, Wt_accessibility_Initial_Vacc), y = Wt_accessibility_Initial_Vacc)) +
      geom_col(fill = "#2b8cbe") +
      coord_flip() +
      scale_y_continuous(breaks = seq(0, ceiling(max(top10_access$Wt_accessibility_Initial_Vacc)), by = 0.5)) +
      labs(title = "Top 10 LEAs by Accessibility to Vaccination Centers", x = "LEA", y = "Weighted Accessibility Score") +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
  })
  
  output$gpIsoMap <- renderLeaflet({
    # Check if gp_sf exists, if not create a placeholder or handle gracefully
    if (exists("gp_sf")) {
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = all_isochrones_fixed, fillColor = "#9ecae1", fillOpacity = 0.6, color = "#3182bd", weight = 1,
                    popup = ~paste0("Center: ", center, "<br>Group: ", group_index)) %>%
        addCircleMarkers(data = gp_sf, radius = 4, fillColor = "darkred", fillOpacity = 0.5, stroke = FALSE,
                         popup = ~paste0("GP: ", GP_Name))
    } else {
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = all_isochrones_fixed, fillColor = "#9ecae1", fillOpacity = 0.6, color = "#3182bd", weight = 1,
                    popup = ~paste0("Center: ", center, "<br>Group: ", group_index))
    }
  })
  
  output$failedGPMap <- renderLeaflet({
    # Check if the required data exists
    if (exists("isochrones_5gp_7") && !is.null(isochrones_5gp_7$failed_points)) {
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          data = isochrones_5gp_7$failed_points,
          radius = 8,
          fillColor = "orange",
          color = "red",
          weight = 2,
          fillOpacity = 0.9,
          popup = ~paste0("<strong>", GP_Name, "</strong><br>", GP_Address, "<br><a href='", GP_LocationLink, "' target='_blank'>Google Maps</a>")
        ) %>%
        addLabelOnlyMarkers(
          data = isochrones_5gp_7$failed_points,
          label = ~GP_Name,
          labelOptions = labelOptions(noHide = TRUE, direction = "top", textsize = "12px", textOnly = TRUE)
        )
    } else {
      leaflet() %>%
        addTiles() %>%
        addMarkers(lng = -6.9, lat = 52.8, popup = "No failed GP data available")
    }
  })
}

shinyApp(ui, server)