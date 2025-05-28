library(shiny)
library(leaflet)
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(stringr)
library(htmlwidgets)
library(shinythemes)
library(scales)
library(openrouteservice)

#Data
gfile <- "data/combined_access_values_LEA.gpkg"
combined_access_values_LEA <- suppressWarnings(st_read(gfile, quiet = TRUE))
all_isochrones_fixed <- st_read("data/all_isochrones.geojson")

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

GPs <- read.csv("data/geocoded_addresses_final.csv", stringsAsFactors = FALSE)
gp_sf <- st_as_sf(GPs, coords = c("longitude", "latitude"), crs = 4326)
failed_gps <- st_read("data/failed_gps.gpkg")

vacc_center <- read.csv("data/geocoded_addresses_vac_final.csv", stringsAsFactors = FALSE)

loc_sf <- st_as_sf(vacc_center, coords = c("longitude", "latitude"), crs = 4326)
center <- loc_sf[1, ]

# Extract lon/lat coordinates
coords <- st_coordinates(center)

Sys.setenv(ORS_API_KEY = "5b3ce3597851110001cf62483cbc75348054423dbe9c47d6a80f9ddb")

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
      helpText("Exploring isochrones and accessibility across LEAs in Ireland."),
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
        
        tabPanel("Top 20 LEAs Accessibility Plot",
                 plotOutput("top20Plot", height = "500px")),
        
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
    # Define colors and labels for the legend
    colors <- c("#9ecae1", "#6baed6", "#3182bd", "#08519c")
    labels <- c("60 min", "30 min", "20 min", "10 min")
    
    # Create the map
    map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      # Add LEA boundaries with labels
      addPolygons(
        data = selected_leas, 
        color = "black", 
        fillColor = "white",
        weight = 1,
        fillOpacity = 0.5,
        popup = ~paste0("<strong>", CSO_LEA, "</strong>"),
        label = ~CSO_LEA,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      
      # Add isochrones in order (largest to smallest)
      addPolygons(
        data = iso_60min_Carlow_Vacc, 
        fillColor = "#9ecae1", 
        fillOpacity = 0.3,
        color = "#9ecae1",
        weight = 1,
        popup = "60 minute drive time",
        group = "60 min"
      ) %>%
      addPolygons(
        data = iso_30min_Carlow_Vacc, 
        fillColor = "#6baed6", 
        fillOpacity = 0.4,
        color = "#6baed6",
        weight = 1,
        popup = "30 minute drive time",
        group = "30 min"
      ) %>%
      addPolygons(
        data = iso_20min_Carlow_Vacc, 
        fillColor = "#3182bd", 
        fillOpacity = 0.5,
        color = "#3182bd",
        weight = 1,
        popup = "20 minute drive time",
        group = "20 min"
      ) %>%
      addPolygons(
        data = iso_10min_Carlow_Vacc, 
        fillColor = "#08519c", 
        fillOpacity = 0.6,
        color = "#08519c",
        weight = 1,
        popup = "10 minute drive time",
        group = "10 min"
      ) %>%
      
      # Add vaccination center
      addCircleMarkers(
        data = center, 
        color = "red", 
        fillColor = "red",
        radius = 6, 
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 2,
        popup = "Carlow Vaccination Center",
        label = "Carlow Vaccination Center"
      ) %>%
      
      # Add custom legend
      addLegend(
        position = "bottomright",
        colors = colors,
        labels = labels,
        title = "Isochrone Time",
        opacity = 0.7
      ) %>%
      
      # Add layer control
      addLayersControl(
        overlayGroups = c("60 min", "30 min", "20 min", "10 min"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    return(map)
  })
  
  output$pharmacyMap <- renderLeaflet({
    pal <- colorNumeric(rev(plasma(100)), domain = combined_access_values_LEA$accessibility_Pharmacy10, na.color = "#808080")
    leaflet(combined_access_values_LEA) %>%
      addTiles("Stamen.Watercolor") %>%
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
      addTiles("Stamen.Watercolor") %>%
      
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
        color = "darkred",
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste("Vaccination Center:", Centre_Name)
      ) %>%
      
      # Top 20 LEAs - orange borders, no fill
      addPolygons(
        data = top20_access,
        color = "#FFA500",
        weight = 2,
        fillOpacity = 0,
        popup = ~paste0("<strong>Top LEA: ", CSO_LEA, "</strong><br>Accessibility: ", round(Wt_accessibility_Initial_Vacc, 2))
      ) %>%
      
      # Top 20 LEAs - black thicker borders on top for highlight
      addPolygons(
        data = top20_access,
        color = "black",
        weight = 1,
        fillOpacity = 0,
        opacity = 0.8,
        popup = ~paste0("<strong>Top LEA: ", CSO_LEA, "</strong><br>Accessibility: ", round(Wt_accessibility_Initial_Vacc, 2))
      ) %>%
      # Legend
      addLegend(pal = pal1,
                values = ~Wt_accessibility_Initial_Vacc,
                title = "Vaccination Accessibility",
                position = "bottomright")
  })
  
  output$top20Plot <- renderPlot({
    ggplot(top20_access, aes(x = reorder(CSO_LEA, Wt_accessibility_Initial_Vacc), y = Wt_accessibility_Initial_Vacc)) +
      geom_col(fill = "#2b8cbe") +
      coord_flip() +
      scale_y_continuous(breaks = seq(0, ceiling(max(top10_access$Wt_accessibility_Initial_Vacc)), by = 0.5)) +
      labs(title = "Top 20 LEAs by Accessibility to Vaccination Centers", x = "LEA", y = "Weighted Accessibility Score") +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
  })
  
  output$gpIsoMap <- renderLeaflet({
    if (exists("gp_sf")) {
      leaflet() %>%
        addTiles("Stamen.TonerLite") %>%
        addPolygons(
          data = combined_access_values_LEA,
          fill = FALSE,
          color = "black",
          weight = 0.1,
          opacity = 1,
          label = ~CSO_LEA
        ) %>%
        addPolygons(
          data = all_isochrones_fixed, 
          fillColor = "#4d4d4d", 
          fillOpacity = seq(0.1, 1, length.out = nrow(all_isochrones_fixed)),
          color = "#2b2b2b", 
          weight = 1,
          popup = ~paste0("Center: ", center, "<br>Group: ", group_index)
        ) %>%
        addCircleMarkers(
          data = gp_sf, 
          radius = 2, 
          fillColor = "#FF6F61", 
          fillOpacity = 0.2, 
          stroke = FALSE,
          popup = ~paste0("GP: ", GP_Name)
        )
    } else {
      leaflet() %>%
        addTiles("CartoDB.Positron") %>%
        addPolygons(
          data = combined_access_values_LEA,
          fill = FALSE,
          color = "black",
          weight = 0.5,
          opacity = 1,
          label = ~CSO_LEA
        ) %>%
        addPolygons(
          data = all_isochrones_fixed, 
          fillColor = "#9ecae1", 
          fillOpacity = seq(0.1, 1, length.out = nrow(all_isochrones_fixed)),
          color = "#3182bd", 
          weight = 1,
          popup = ~paste0("Center: ", center, "<br>Group: ", group_index)
        )
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

