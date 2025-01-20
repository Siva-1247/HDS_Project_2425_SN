library(sf)
Data <- st_read("Boundary_CSO.shp")
head(Data)
#install.packages("geodist")
library(geodist)
# Cannot use as in IZRENZET95 system rather than WGS84
#centroids <- st_centroid(Data$geometry)
#boundary_centroids <- Data
#boundary_centroids$longitude <- st_coordinates(centroids)[,1]
#boundary_centroids$latitude <- st_coordinates(centroids)[,2]
#head(boundary_centroids)

Initial_Vacc <- read.csv("geocoded_addresses_vac_final.csv")
head(Initial_Vacc)

LEA_Cord <- data.frame(lon_l=boundary_centroids$longitude, lat_l =boundary_centroids$latitude)
Vac_Cord <- data.frame(lon_v=Initial_Vacc$longitude, lat_v =Initial_Vacc$latitude)
## Dist returned N/A as coordinates are not in WGS84 format
Dist <-  geodist(
  LEA_Cord,
  Vac_Cord,
  measure = "geodesic")
Dist
##Conversion to stnd coordinate system
st_crs(Data)
Data_84 <- st_transform(Data,4326)
centroids <- st_centroid(Data_84$geometry)
boundary_centroids <- Data_84
boundary_centroids$longitude <- st_coordinates(centroids)[,1]
boundary_centroids$latitude <- st_coordinates(centroids)[,2]
head(boundary_centroids)
##Retry distances
LEA_Cord <- data.frame(lon_l=boundary_centroids$longitude, lat_l =boundary_centroids$latitude)
Vac_Cord <- data.frame(lon_v=Initial_Vacc$longitude, lat_v =Initial_Vacc$latitude)
Dist <-  geodist(
  LEA_Cord,
  Vac_Cord,
  measure = "geodesic")
Dist
rownames(Dist)<-boundary_centroids$cso_lea
colnames(Dist)<-Initial_Vacc$Centre_Name

library(reshape2)
Dist_long <- melt(Dist)
names(Dist_long) <- c("LEA", "Center", "Distance")
write.csv(Dist_long, "Centroid_distances.csv", row.names = TRUE)
Dist_long <- read.csv("Centroid_distances.csv")

library(ggplot2)
library(viridis)
##Heatmap
Dist_viz <- ggplot(Dist_long, aes(x=Center, y=LEA, fill=Distance)) + geom_tile() +
  scale_fill_viridis(name = "Distance (m)") +
  theme_minimal() +
  labs(title = "Distances from LEA centroids to Initial Vaccination Centers",
         x="Initial Vaccination Centers", y="LEA in IRL")
print(Dist_viz)
##Not appealing
Dist_long$County <- boundary_centroids$county
head(Dist_long)
library(dplyr)
filtered_data <- Dist_long %>%
  group_by(County) %>%
  arrange(Distance) %>%
  slice_head(n = 5) %>%
  ungroup()
Dist_viz1 <- ggplot(Dist_long, aes(x=Center, y=County, fill=Distance)) + geom_tile() +
  scale_fill_viridis(name = "Distance (m)", option = "C") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "5 closest Initial Vaccination Centers to Counties",
       x="Initial Vaccination Centers", y="Counties")
print(Dist_viz1)
##Still not convinced
closest_center_data <- Dist_long %>%
  group_by(LEA) %>%
  slice_min(order_by = Distance, n = 1) %>%  # Select the center with the smallest distance
  ungroup()
bar_plot <- ggplot(closest_center_data, aes(x = reorder(LEA, Distance), y = Distance, fill = Center)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_viridis_d(name = "Closest Center", option = "C") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  ) +
  labs(
    title = "20 LEA with most distance from Initial Vac Centers ",
    x = "LEA in IRL",
    y = "Distance (m)"
  )

print(bar_plot)
###
# Calculate average distance by county and find nearest center
summary_viz <- Dist_long %>%
  group_by(County) %>%
  summarise(
    min_dist = min(Distance)/1000,  # Convert to km
    nearest_center = Center[which.min(Distance)]
  ) %>%
  arrange(min_dist) %>%
  head(10)

# Create single clear visualization
ggplot(summary_viz, aes(x = reorder(County, min_dist), y = min_dist)) +
  geom_bar(stat = "identity", fill = "#EEB4B4", alpha = 0.8) +
  geom_text(aes(label = round(min_dist, 1)), 
            hjust = -0.1, size = 3) +
  coord_flip() +  # Horizontal bars for better label readability
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Minimum Distance to Vaccination Centers by County",
    x = "County",
    y = "Distance (km)"
  )