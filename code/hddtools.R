# hddtools

# see ROpenSci post
# http://ropensci.org/blog/blog/2017/03/07/hddtools

# packs <- c("zoo", "sp", "RCurl", "XML", "rnrfa", "Hmisc", "raster",
#            "stringr", "devtools", "leaflet")
# new_packages <- packs[!(packs %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)

# development version
# devtools::install_github("ropensci/hddtools")

library("hddtools")

# Plot Pompeii on a map
library(raster)
Italy <- getData("GADM", country = "IT", level = 1)
US <- getData("GADM", country = "US", level = 1)

US <- SpatialPoints(coords = data.frame(x = 14.4870, y = 40.7510))

plot(Italy, col = NA, border = "darkgrey")

plot(Pompeii, col = "red", new=TRUE)

# Define and plot a bounding box centred in Pompeii (Italy)
areaBox <- raster::extent(Pompeii@coords[[1]] - 0.5, Pompeii@coords[[1]] + 0.5,
                          Pompeii@coords[[2]] - 0.5, Pompeii@coords[[2]] + 0.5)
plot(areaBox, add = TRUE, col = "red")

KGClimateClass(areaBox = areaBox, updatedBy = "Kottek", verbose = TRUE)

# GRDC full catalogue
GRDC_catalogue_all <- catalogueGRDC()


# Filter GRDC catalogue based on a country code
GRDC_IT <- catalogueGRDC(columnName = "country_code", columnValue = "US")

# Convert the table to a SpatialPointsDataFrame
hydro <- SpatialPointsDataFrame(coords = GRDC_IT[, c("long", "lat")],
                                data = GRDC_IT)

# Plot the stations on the map
plot(Italy, col = NA, border = "darkgrey")
plot(hydro, col = "blue", pch = 1, add = TRUE )


