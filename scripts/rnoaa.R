library(rnoaa)
library(dplyr)

# station_data <- ghcnd_stations() # Takes a while to run
# saveRDS(station_data, "data/NOAA_station_data.rds")

station_data<-readRDS("data/NOAA_station_data.rds")


# leaflet map
leaflet() %>%
  addTiles() %>% 
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addCircleMarkers(group = "NOAA", stroke = 0.8, radius = 1,opacity = 0.7, fillOpacity = 0.8, 
                   lng = ~longitude, lat = ~latitude,
                   data = station_data, weight=1, color="maroon") %>% 
  addLayersControl(
    baseGroups = c("ESRI Aerial", "Topo"),
    overlayGroups = c("NOAA"),
    options = layersControlOptions(collapsed = T))




lat_lon_df <- data.frame(id = c("davis", "san francisco"),
                         latitude = c(38.5449, 37.7749),
                         longitude = c(-121.7405, -122.4194))
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                          station_data = station_data, radius = 20)

davis <- data.frame(id="davis", latitude=38.5449, longitude=-121.7405)

# closest stations within 50 miles
meteo_nearby_stations(lat_lon_df = davis, station_data = station_data,
                      radius = 50, var = c("PRCP", "TMAX"),
                      year_min = 1992, year_max = 1992)

# closest ten stations:
meteo_nearby_stations(lat_lon_df = davis, station_data = station_data,
                      limit = 10, var = c("PRCP", "TMAX"),
                      year_min = 1992, year_max = 1992)


# GET ALL WEATHER DATA FROM DAVIS: USC00042294

# can pick the variables of interest (or "all"):
# (PRCP, TAVG, TMAX, TMIN, PSUN (daily percent of possible sunshine), AWDR (avg wind dir), AWND (avg wind speed in tenths m / s))
davis<-ghcnd_search(stationid = "USC00042294", var = c("prcp", "tavg", "tmax", "tmin","psun", "awdr","awnd","evap"))

library(ggplot2); library(viridis); library(lubridate)

tmax.D<-davis$tmax
head(tmax.D)

# dotplot
ggplot() + geom_point(data=tmax.D, aes(x=date, y=tmax/10, fill=tmax), pch=21, alpha=0.5) + scale_fill_viridis(option = "A")

ggplot() + 
  geom_jitter(data=tmax.D, aes(x=date, y=tmax/10, fill=tmax/10), pch=21, alpha=0.2) + scale_fill_viridis(option="A") +
geom_boxplot(data=tmax.D, aes(x=date, y=tmax/10, group=year(date)), alpha=0.8) 


pcp.D<-davis$prcp %>% filter(year(date)>1900 & year(date)<2017) %>% 
  mutate(year=year(date)) %>%
  group_by(id, year) %>% 
  summarize(mean_ppt = mean(prcp, na.rm=TRUE),
            ann_ppt = sum(prcp, na.rm=TRUE))

head(pcp.D)
ggplot() + geom_bar(data=pcp.D, aes(x=year, y=mean_ppt, fill=mean_ppt), alpha=0.5, stat="identity") + scale_fill_viridis()


library(ggrepel)
ggplot() + geom_point(data=pcp.D, aes(x=year, y=mean_ppt, fill=mean_ppt), pch=21, col="gray") + scale_fill_viridis(option = "D") + 
  geom_label_repel(data=pcp.D[pcp.D$mean_ppt<6 | pcp.D$mean_ppt>17,], aes(x=year, y=mean_ppt, label=year))
