library(sf)
Data <- st_read("C:/Users/Sivagami Nedumaran/Downloads/Merged_Data_Final.shp")
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
nrow(Initial_Vacc)
names(Data)
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
library(dplyr)
Data_84 <- st_transform(Data,4326)
head(Data_84)
nrow(Data_84)
library(plyr)
centroids <- st_centroid(Data_84$geometry)
boundary_centroids <- Data_84
boundary_centroids$longitude <- st_coordinates(centroids)[,1]
boundary_centroids$latitude <- st_coordinates(centroids)[,2]
head(boundary_centroids)
BC_Data <- boundary_centroids %>%  st_drop_geometry() %>% select(cso_lea, longitude, latitude) %>% distinct()
head(BC_Data)
nrow(BC_Data)
##Retry distances
LEA_Cord <- data.frame(lon_l=BC_Data$longitude, lat_l =BC_Data$latitude)
Vac_Cord <- data.frame(lon_v=Initial_Vacc$longitude, lat_v =Initial_Vacc$latitude)
Dist <-  geodist(
  LEA_Cord,
  Vac_Cord,
  measure = "geodesic")
Dist
rownames(Dist)<-BC_Data$cso_lea
colnames(Dist)<-Initial_Vacc$Centre_Name

library(reshape2)
Dist_long <- melt(Dist)
names(Dist_long) <- c("LEA", "Center", "Distance")
head(Dist_long)
write.csv(Dist_long, "Centroid_distances.csv", row.names = TRUE)
Dist_long <- read.csv("Centroid_distances.csv")

library(ggplot2)
library(viridis)
###########
library(scales)
Dist_l <- Dist_long[Dist_long$LEA %in% unique(Dist_long$LEA)[1:20], ]
Dist_l<- ddply(Dist_l, .(Center), transform,
               Scaled_Distance = rescale(Distance))
(p <- ggplot(Dist_l, aes(Center, LEA)) + geom_tile(aes(fill = Scaled_Distance),colour = "white") + 
    scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(label=round(Scaled_Distance,1))) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text.y = element_text(size = 6))+
  labs(title = "Distances 20 LEAs to Initial Vaccination Centers",
       x="Initial Vaccination Centers", y="LEA in IRL"))
##Heatmap
Dist_viz <- ggplot(Dist_long, aes(x=Center, y=LEA, fill=Distance)) + geom_tile() +
  scale_fill_viridis(name = "Distance (m)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text.y = element_text(size = 6)) +
  labs(title = "Distances from LEA centroids to Initial Vaccination Centers",
         x="Initial Vaccination Centers", y="LEA in IRL")
print(Dist_viz)
##Not appealing

clean_data <- Dist_long %>%
  select(-X) %>%  # Remove the X column
  mutate(
    Center = gsub(" Vaccination Centre", "", Center)  # Shorten center names for better display
  )

summary_stats <- Dist_Long %>%
  group_by(LEA) %>%
  summarize(
    Mean_Distance = mean(Distance),
    Min_Distance = min(Distance),
    Max_Distance = max(Distance),
    Nearest_Center = Center[which.min(Distance)],
    Farthest_Center = Center[which.max(Distance)]
  ) %>%
  ungroup()

# Top 5 nearest LEAs
top_nearest <- summary_stats %>% arrange(Mean_Distance) %>% head(5)

# Top 5 farthest LEAs
top_farthest <- summary_stats %>% arrange(desc(Mean_Distance)) %>% head(5)

# Add a 'Type' column for grouping
top_nearest <- top_nearest %>% mutate(Type = "Nearest")
top_farthest <- top_farthest %>% mutate(Type = "Farthest")

# Combine the data for plotting
highlight_data <- bind_rows(top_nearest, top_farthest)
library(ggplot2)

# Bar chart
ggplot(highlight_data, aes(x = reorder(LEA, Mean_Distance), y = Mean_Distance, fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip for horizontal bars
  scale_fill_manual(values = c("Nearest" = "blue", "Farthest" = "red")) +
  labs(
    title = "Top 5 Nearest and Farthest LEAs",
    x = "LEA",
    y = "Average Distance",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

################################
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
  geom_text(aes(label = sprintf("%0.1f km\n%s", min_dist, nearest_center)), 
            hjust = -0.1, size = 3) +
  coord_flip() +  
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