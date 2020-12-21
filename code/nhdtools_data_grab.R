# download NHD data for CA 
# see map here and turn on Hydrography Layer: https://viewer.nationalmap.gov/basic/?basemap=b1&category=nhd&title=NHD%20View
# HUC2 is 18 for CA 
# Sacramento: 1802, SJ: 1804, Kern: 1803

# Load Library ------------------------------------------------------------

library(nhdplusTools)
library(sf)
library(beepr)
library(mapview)
library(dplyr)

# set up data dir and geopackage name
data_dir <- "/Users/ryanpeek/Dropbox/nhdplus_data"

# Download NHD+ Hi-res Data for ENTIRE HUC02 Region ------------------------------------

# check urls
(hr_urls <- download_nhdplushr(nhd_dir = data_dir, "18", download_files = FALSE))

# Download some NHDPlusHR Data (these are LARGE)
#hr_data_dir <- download_nhdplushr(nhd_dir = data_dir, hu_list = c("1802"), download_files = TRUE)
#beepr::beep(2)

# set up and save all out to the geopackage
(ca_gpkg <- file.path(data_dir, "sac_nhdplus_data.gpkg"))

# pull data out and put into a geopackage
# hr_data <- get_nhdplushr(hr_dir = data_dir, 
#                          layers = c("NHDFlowline", "NHDPlusCatchment"),
#                          #layers = NULL, # include all layers
#                          overwrite = TRUE,
#                          out_gpkg = ca_gpkg, 
#                          pattern = "1802",
#                          min_size_sqkm = 50,
#                          proj = "+init=epsg:5070")
# warning is ok

hr_data <- ca_gpkg

# check layers
st_layers(ca_gpkg)


# plot
plot(st_geometry(hr_data$NHDFlowline), 
     lwd = hr_data$NHDFlowline$StreamOrde/4, col = "blue")

# Download and Stage the NHD Seamless Data --------------------------------

# download here: https://www.epa.gov/waterdata/nhdplus-national-data
# the zipped file is 7GB and the unzipped lower 48 is over 15GB!
nhd_dir <- "/Users/ryanpeek/Dropbox/nhdplus_data/seamless/NHDPlusNationalData"

# set up nhdpath
nhdplus_path(file.path(nhd_dir, "NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"))

# stage (save sep data out) this took about an hour on my computer
staged_nhdplus <- stage_national_data(simplified = TRUE)

# if these files already exist it can just use them
staged_data <- stage_national_data(output_path = file.path(nhd_dir))
staged_data

flowline <- readRDS(staged_data$flowline)
names(flowline)[1:10]

# Find Everything Upstream ------------------------------------------------

# use mapedit to create a point!
library(mapedit)
library(USAboundaries)
ca <- USAboundaries::us_boundaries(type="state", states="ca")
tst_map <- mapedit::drawFeatures(mapview(ca)) # try kings river

start_point <- tst_map %>% 
  rename(locality = feature_type) %>% 
  mutate(locality = "Kings River") %>% 
  select(-X_leaflet_id)

# use a lat lon
# start_point <- sf::st_sfc(sf::st_point(c(-121.80072, 38.03816)),
#                           crs = 4269)
 
# now get COMID
start_comid <- discover_nhdplus_id(start_point)
start_comid

# check whats avail
nldi_feature <- list(featureSource = "comid", featureID = start_comid)
discover_nldi_navigation(nldi_feature)

# or try this for a specific gage
# NFA: 11427000
# NFY: 11413000
# Merced Happy Isles: 11264500

nldi_feature <- list(featureSource = "nwissite", featureID = "USGS-11264500")
discover_nldi_navigation(nldi_feature)

# options: upstreamMain, upstreamTributaries, downstreamMain, downstreamDiversions

# now go grab flowlines for this point!
flowline_DM <- navigate_nldi(nldi_feature, 
                               mode = "DM", 
                               data_source = "")

flowline_UM <- navigate_nldi(nldi_feature, 
                             mode = "UM", 
                             data_source = "")

flowline_UT <- navigate_nldi(nldi_feature, 
                             mode = "UT", 
                             data_source = "")

mapview(flowline_UM, color="darkblue", lwd=2, legend=FALSE) + 
  mapview(flowline_UT, color="steelblue", lwd=1, legend=FALSE) +
  mapview(flowline_DM, color="blue", lwd=3, legend=FALSE)


# finally you can add
output_geopkg_download <- file.path(data_dir, "nfy_11413000_nhdplus_full.gpkg")

output_file_download <-subset_nhdplus(comids = flowline_UT$nhdplus_comid,
                                      output_file = output_geopkg_download,
                                      overwrite = TRUE, 
                                      nhdplus_data = "download",
                                      return_data = FALSE)

NHD_UT <- st_read(dsn = output_geopkg_download, layer = "NHDFlowline_Network", as_tibble=TRUE) %>% 
  dplyr::rename(geometry=geom)

# now grab the downstream mainstems (just return data, no geopackage)
output_DM <-subset_nhdplus(comids = flowline_DM$nhdplus_comid,
                           nhdplus_data = "download", 
                           return_data = TRUE)

# merge/bind layers together
NHDFlowline_Network <- do.call(rbind, list(output_DM$NHDFlowline_Network, NHD_UT))

# take this downloaded file and update existing geopackage
st_write(NHDFlowline_Network, layer = "NHDFlowline_Network", dsn = output_geopkg_download, delete_layer = TRUE)

st_layers(output_geopkg_download)
mapview(NHDFlowline_Network)

# view
plot(st_geometry(filter(NHDFlowline_Network, streamorde > 2)),
     lwd = 7, col = "darkgrey")
plot(sf::st_geometry(NHDFlowline_Network), 
     lwd = 3, col = "steelblue", add = TRUE)

# check for all gages downstream of location:
downstream_nwis <- navigate_nldi(nldi_feature,
                               mode = "DM",
                               data_source = "nwissite")
mapview(downstream_nwis) + mapview(NHDFlowline_Network, color="skyblue", lwd=1)

# all gages UPSTREAM
upstream_nwis <- navigate_nldi(nldi_feature,
                                 mode = "UT",
                                 data_source = "nwissite")


# plot!
plot(st_geometry(filter(NHDFlowline_Network, streamorde > 2)),
     lwd = 7, col = "darkgrey")
plot(sf::st_geometry(NHDFlowline_Network), 
     lwd = 1, col = "steelblue", add = TRUE)
plot(sf::st_geometry(downstream_nwis), 
     cex = 1, pch=21, bg = "orange", add = TRUE)
plot(sf::st_geometry(upstream_nwis), 
     cex = 1, pch=21, bg = "purple", add = TRUE)

# Getting Smaller Datasets ------------------------------------------------

# Make a plot based on a gage (automatically downloads)
(nfa <- plot_nhdplus("11427000"))

# add adjacent watershed
(nfa_mfa <- plot_nhdplus(list(
  list("nwissite", "USGS-11427000"),
  list("huc12pp", "180201280504"))))

# now plot specific range of data:
plot_nhdplus(list(list("comid", "14992581"),
                  list("nwissite", "USGS-11427000")),
             streamorder = 3,
             plot_config = list(basin = list(lwd = 2),
                                outlets = list(
                                  comid = list(col = "green"))),
             stoponlargerequest = FALSE)

# Download Data based on Point/COMID/Gage ---------------------------------

# use a starting point
start_point <- sf::st_as_sf(data.frame(x = -121.02230, y = 38.93572), 
                            coords = c("x", "y"), crs = 4326)

# use a USGS Gage
nwissite <- list(featureSource = "nwissite", 
                 featureID = "USGS-11427000")

# get all flowlines upstream of point/gage
flowline <- navigate_nldi(nwissite, 
                          mode = "upstreamTributaries", 
                          data_source = "")

# now download NHD+ data for COMIDS, save to geopackage
geopackname <- "nfa_11427000_nhdplus_us.gpkg"

nhdplus <- subset_nhdplus(comids = flowline$nhdplus_comid,
                          output_file = file.path(data_dir, geopackname),
                          nhdplus_data = "download",
                          overwrite = TRUE, return_data = FALSE)

nfa_flowline <- read_sf(nhdplus, "NHDFlowline_Network")

nfa_upstream_nwis <- navigate_nldi(nwissite,
                               mode = "upstreamTributaries",
                               data_source = "nwissite")

nfa_basin <- get_nldi_basin(nwissite)

# check layers in geopackage
st_layers(nhdplus)

mapview(read_sf(nhdplus, "NHDWaterbody")) + mapview(read_sf(nhdplus, "NHDFlowline_Network"), color="steelblue")


