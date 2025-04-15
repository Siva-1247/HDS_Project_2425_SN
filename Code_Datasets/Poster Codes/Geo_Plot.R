library(sf)
library(ggplot2)
library(viridis)
#Data<-st_read("C:/Users/Sivagami Nedumaran/Downloads/Boundary_CSO.shp")
# filtered_data <- Data[grepl("12 years and over", Data$age_grp), ]
#st_write(filtered_data, "C:/Users/Sivagami Nedumaran/Downloads/Merged_Data_Final.shp", append = FALSE)
Mdata <- st_read("C:/Users/Sivagami Nedumaran/Downloads/Merged_Data_Final.shp")
st_is_valid(Mdata)
Data_84 <- st_transform(Mdata,4326)
##Plot not rendering
#plot <- ggplot(Data_84) + 
  geom_sf(aes(fill = prmry_cm)) +  
  scale_fill_viridis(name = "Primary Vaccination Rate (%)") +
  theme_bw() +
  labs(title = "Primary Vaccination Rates by Local Electoral Area",
       subtitle = "Ireland") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
#print(plot)
class(Data_84)
summary(Data_84$geometry)
sum(is.na(Data_84$prmry_cm))
class(Data_84$prmry_cm)
library(leaflet)

#Too big to process by R with whole geometries
#Too large to even simplify 
#Data_84 <- st_simplify(Data_84, dTolerance = 0.05)
#pal <- colorNumeric(palette = "YlOrRd", domain = Data_84$prmry_cm)
#l <- leaflet(Data_84) %>% addTiles() %>% addPolygons(color = "white", fillColor = ~ pal(prmry_cm),fillOpacity = 0.8) %>%
  # addLegend(pal = pal, values = ~prmry_cm, opacity = 0.8)
##Centroid Distance was easier to plot
Data_84_centroids <- st_centroid(Data_84)
leaflet(Data_84_centroids) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~prmry_cm / 20,  # Adjust size based on variable
    fillColor = "#8B475D",
    fillOpacity = 0.5,
    stroke = FALSE,
    popup = ~paste("<strong>LEA:</strong>", cso_lea, "<br>",
                   "<strong>Vaccination Rate:</strong>", prmry_cm, "%")
  )

##################################################################
##Scatter Plot
library(geodist)
gfile <- "C:/Users/Sivagami Nedumaran/Downloads/Merged_Data_Final.shp"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE))
# Step 2: Transform to longlat (WGS84)
geo_data_jan <- geo_data %>%
  filter(month == "2022 January")
geo_data_jan <- st_transform(geo_data_jan, crs = 4326)
centroids <- st_centroid(geo_data_jan$geometry)
boundary_centroids <- geo_data_jan
boundary_centroids$longitude <- st_coordinates(centroids)[,1]
boundary_centroids$latitude <- st_coordinates(centroids)[,2]
head(boundary_centroids)
BC_Data <- boundary_centroids %>%  st_drop_geometry() %>% select(cso_lea, longitude, latitude, prmry_cm) %>% distinct()
Initial_Vacc <- read.csv("geocoded_addresses_vac_final.csv")
LEA_Cord <- data.frame(lon_l=BC_Data$longitude, lat_l =BC_Data$latitude)
Vac_Cord <- data.frame(lon_v=Initial_Vacc$longitude, lat_v =Initial_Vacc$latitude)
Dist <-  geodist(
  LEA_Cord,
  Vac_Cord,
  measure = "geodesic")
rownames(Dist)<-BC_Data$cso_lea
colnames(Dist)<-Initial_Vacc$Centre_Name
min_dist <- apply(Dist, 1, min)
BC_Data$min_dist <- min_dist
write.csv(BC_Data, "BC_Data.csv")