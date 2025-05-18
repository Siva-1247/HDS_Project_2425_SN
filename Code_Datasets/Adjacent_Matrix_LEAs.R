library(mapview)
library(spdep)
library(sf)

gfile <- "Accessibility_Data\\CSO_Local_Electoral_Areas_National_Statistical_Boundaries_2022_Generalised_100m_-6420530397479472898.geojson"
geo_data <- suppressWarnings(st_read(gfile, quiet = TRUE))
lea_mat <- nb2mat(poly2nb(geo_data), style = "B")
lea_mat[1:10, 1:10]
any(lea_mat != 0)

