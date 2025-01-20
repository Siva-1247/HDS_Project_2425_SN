#install.packages("sf")
library(sf)
library(dplyr)
##GPKG to CSV
boundaries <- st_read("CSO_Local_Electoral_Areas_National_Statistical_Boundaries_2022_Ungeneralised_view_-90546080205670867 (1).gpkg")

# Save as shapefile:
st_write(boundaries, "LEA_Boundary.shp")
# Read Shapefile
lea_boundaries <- st_read("LEA_Boundary.shp")
head(lea_boundaries)

#CSV CSO Stats
vax_data <- read.csv("CDC47_Stats.csv")

# Create new column with uppercase elements before comma
vax_data$LEA_Short <- toupper(sapply(strsplit(as.character(vax_data$Local.Electoral.Area), ","), `[`, 1))

# View the result
head(vax_data[c("Local.Electoral.Area", "LEA_Short")])
##Inner Join
merged_data <- merge(boundaries, vax_data, 
                     by.x = "CSO_LEA",        
                     by.y = "LEA_Short",
                     all.x = FALSE,          
                     all.y = FALSE)
head(merged_data)
colSums(is.na(merged_data))
##Find unmatched rows if any
unmatched_in_shapefile <- boundaries[!(boundaries$CSO_LEA %in% vax_data$LEA_Short), ]
unmatched_in_csv <- vax_data[!(vax_data$LEA_Short %in% boundaries$CSO_LEA), ]
nrow(unmatched_in_shapefile)
nrow(unmatched_in_csv)
# View unmatched rows in each dataset
unmatched_in_shapefile
unmatched_in_csv
#Fixing Graiguecullen -Portarlington - Only unmatched lea
vax_data$LEA_Short <- gsub("GRAIGUECULLEN -PORTARLINGTON", 
                                        "GRAIGUECULLEN-PORTARLINGTON", 
                                        vax_data$LEA_Short)
#Remerge
merged_data_2 <- merge(boundaries, vax_data, 
                     by.x = "CSO_LEA",        
                     by.y = "LEA_Short",
                     all.x = FALSE,          
                     all.y = FALSE)
#Re-Check
unmatched_in_shapefile <- boundaries[!(boundaries$CSO_LEA %in% vax_data$LEA_Short), ]
unmatched_in_csv <- vax_data[!(vax_data$LEA_Short %in% boundaries$CSO_LEA), ]
nrow(unmatched_in_shapefile)
nrow(unmatched_in_csv)
##Write error due to long column names, cleaned and rewriiten new shape file
names(merged_data_2)
# Rename columns
merged_data_2_clean <- merged_data_2 %>%
  rename(
    cso_lea = CSO_LEA,
    lea_guid = LEA_GUID,
    lea_official = LEA_OFFICIAL,
    lea_id = LEA_ID,
    county = COUNTY,
    month = Month,
    electoral_area = Local.Electoral.Area,
    age_group = Age.Group,
    primary_complete = Primary.Course.Completed....,
    primary_change = Monthly.Percentage.Point.Change.in.Primary.Course.Completed....,
    booster1 = Booster.1....,
    booster1_change = Monthly.Percentage.Point.Change.in.Booster.1....,
    booster2 = Booster.2....,
    booster2_change = Monthly.Percentage.Point.Change.in.Booster.2....,
    booster3 = Booster.3....,
    booster3_change = Monthly.Percentage.Point.Change.in.Booster.3....,
    booster4 = Booster.4....,
    booster4_change = Monthly.Percentage.Point.Change.in.Booster.4....,
    geometry = SHAPE
  )
names(merged_data_2_clean)
##Write Output
st_write(merged_data_2_clean, "Boundary_CSO.shp")