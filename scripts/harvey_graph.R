# from this gist:
# https://gist.github.com/ldecicco-USGS/56262f3809f0807cb523d7105cb790a9


# LIBRARIES ---------------------------------------------------------------
library(tidyverse) #(ggplot, dplyr, tidyr)
library(maps)
library(rgdal)
library(geoknife)
library(dataRetrieval)
library(httr)

#devtools::install_github("dgrtwo/gganimate", force=TRUE)
library(gganimate)

# sf?
library(sf)

load("data_output/hurricane_data_20170826.rda")
storm_data<-readRDS("data_output/storm_data.rds")

# GET STATE COUNTY DATA ---------------------------------------------------

state_names <- c("texas","louisiana")

states <- map_data("state") %>%
  filter(region %in% state_names)

counties <- map_data("county",state_names) 

# GET HURRICANE/STORM TRACK -----------------------------------------------

nhc.url <- "http://www.nhc.noaa.gov/gis/best_track/%s%s%s_best_track.zip"
download.url <- sprintf(nhc.url, "al", "09", "2017")
#temp_path <- "data/hurricane"
temp_path <- tempdir()
download.file(download.url, destfile = paste0(temp_path,"/al092017_best_track.zip"))

unzip(zipfile = paste0(temp_path, "/al092017_best_track.zip"), exdir = temp_path)

# sf version CAN'T GET IT TO FILTER!!
lines <- st_read(dsn = temp_path, layer = "AL092017_lin")

radii <- st_read(dsn=temp_path, layer="AL092017_radii") %>% 
  mutate(datetime = lubridate::ymd_h(as.character(SYNOPTIME))) %>% 
  filter(as.character(SYNOPTIME)==max(as.character(SYNOPTIME)))
  
windswath <- st_read(dsn=temp_path, layer="AL092017_windswath") %>% 
  mutate(sttime_dtg = lubridate::ymd_h(as.character(STARTDTG)),
         entime_dtg = lubridate::ymd_h(as.character(ENDDTG))) %>% 
  filter(as.character(ENDDTG)==max(as.character(ENDDTG)))

devtools::install_github("environmentalinformatics-marburg/mapview", ref="develop")

library("mapview")
mapview(lines) + 
  #radii + 
  windswath

# orig version
lines <- readOGR(dsn=temp_path, layer="AL092017_lin")
lines_fortify <- fortify(lines) %>%
  filter(long < -88, lat >22)

radii <- readOGR(dsn=temp_path, layer="AL092017_radii")
radii_fortify <- fortify(radii) %>%
  filter(long < -88,
         lat >22) %>%
  mutate(id = as.numeric(id)) %>%
  filter(id == max(id)) %>%
  mutate(id = as.character(id))

windswath <- readOGR(dsn=temp_path, layer="AL092017_windswath")
windswath_fortify <- fortify(windswath) %>%
  filter(long < -88,
         lat >22) %>%
  mutate(id = as.numeric(id)) %>%
  filter(id == max(id)) %>%
  mutate(id = as.character(id))

# GEOKNIFE ----------------------------------------------------------------

devtools::install_github('USGS-R/geoknife')
library(geoknife)
# using geoknife to get spatial data (http://usgs-r.github.io/geoknife/)
stencil <- webgeom('HUC8::09020306,14060009') #can be state, ecoregion, HUC8
stencil

# see other potential values
HUC8s <- query(stencil, 'values')
# there are thousands of results, but head() will only display a few of them
head(HUC8s) 

# get data
fabric <- webdata('prism')
# display fabric:
fabric

# view variables and options to restrict to timeframe:
times(fabric) <- as.POSIXct(c('1990-01-01','2015-12-01'))
query(fabric, 'times')
query(fabric, 'variables')

job <- geoknife(stencil, fabric)
check(job)

data <- result(job)
plot(data[,1:2], ylab = variables(fabric), col="blue")


webdatasets = query('webdata')
webdatasets[100:120]

query(stencil, 'geoms')
query(stencil, 'attributes')
query(stencil, 'Ecoregions_Level_III')


# GET PRECIP --------------------------------------------------------------

getPrecip <- function(states, startDate, endDate){
 
  # use fips data from maps package
  counties_fips <- maps::county.fips %>% 
    mutate(statecounty=as.character(polyname)) %>% # character to split into state & county
    tidyr::separate(polyname, c('statename', 'county'), ',') %>%
    mutate(fips = sprintf('%05d', fips)) %>% # fips need 5 digits to join w/ geoknife result
    filter(statename %in% states) 
  
  stencil <- webgeom(geom = 'derivative:US_Counties',
                     attribute = 'FIPS',
                     values = counties_fips$fips)
  
  fabric <- webdata(url = 'http://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                    variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                    times = c(startDate, endDate))
  
  job <- geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
  check(job)
  precipData_result <- result(job, with.units=TRUE)
  precipData <- precipData_result %>% 
    select(-variable, -statistic, -units) %>% 
    gather(key = fips, value = precipVal, -DateTime) %>%
    left_join(counties_fips, by="fips") #join w/ counties data
  
  return(precipData)
  
}


start_time <- "2017-08-25 05:00:00"
end_time <- as.character(Sys.time())

precipData <- getPrecip(states = state_names, 
                        startDate = start_time, 
                        endDate = end_time)

precipData_sum <- precipData %>% 
    group_by(statename, statecounty, county) %>% 
    summarize(Precipitation = sum(precipVal)) 

counties <- left_join(counties, precipData_sum, by=c("region"="statename",
                                                        "subregion"="county"))

# save precip:
save(precipData, precipData_sum, counties, file = "data_output/hurricane_data_20170829.rda")


# GET RIVER HYDROGRAPHY ---------------------------------------------------

get_flowlines <- function(streamorder, mapRange){
  postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"
  
  filterXML <- paste0('<?xml version="1.0"?>',
                '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                  '<wfs:Query xmlns:feature="https://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
                    '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:And>',
                        '<ogc:PropertyIsGreaterThan>',
                          '<ogc:PropertyName>streamorde</ogc:PropertyName>',
                          '<ogc:Literal>',streamorder-1,'</ogc:Literal>',
                        '</ogc:PropertyIsGreaterThan>',
                        '<ogc:BBOX>',
                          '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                          '<gml:Envelope>',
                            '<gml:lowerCorner>',mapRange[3]," ",mapRange[1],'</gml:lowerCorner>',
                            '<gml:upperCorner>',mapRange[4]," ",mapRange[2],'</gml:upperCorner>',
                          '</gml:Envelope>',
                        '</ogc:BBOX>',
                      '</ogc:And>',
                    '</ogc:Filter>',
                  '</wfs:Query>',
                '</wfs:GetFeature>')

  destination = file.path(tempdir(),"nhdflowline_network.zip")
  file <- POST(postURL, body = filterXML, write_disk(destination, overwrite=T))

  filePath <- tempdir()
  print("unzipping...")
  unzip(destination, exdir = filePath)
  
  flowLines <- st_read(filePath, layer = 'nhdflowline_network')
  #flowLines = readOGR(filePath, layer='nhdflowline_network')
  
  return(flowLines)
}

mapRange <- c(range(counties$long),range(counties$lat))
rivers <- get_flowlines(5, mapRange) # orig map used order of 6

# save rivers
# saveRDS(rivers, file = "data_output/texas_rivers_nhd_order_4.rds")

# fortify if using non-sf package
# rivers_fortify <- fortify(rivers)
# saveRDS(rivers_fortify, file = "data_output/texas_rivers_fortify_nhd_order_4.rds")

# preview map?
library("mapview")
mapview(lines) + 
  rivers + 
  windswath


rivers_fortify <- readRDS("data_output/texas_rivers_fortify_nhd_order_4.rds")

states_abb <- c("TX","LA")


# GET NWIS FLOW DATA ------------------------------------------------------

#download each state individually
for(st in states_abb){

  stDV <- renameNWISColumns(readNWISdata(service="dv",
                                       parameterCd="00060",
                                       stateCd = st,
                                       startDate = "2017-08-24",
                                       endDate = Sys.Date()))
  if(st != states_abb[1]){
    storm.data <- full_join(storm.data,stDV)
    sites <- full_join(sites, attr(stDV, "siteInfo"))
  } else {
    storm.data <- stDV
    sites <- attr(stDV, "siteInfo")
  }
}

saveRDS(storm.data, file = "data_output/storm_data.rds")
saveRDS(sites, file="data_output/sites.rds")

sites <- readRDS("data_output/sites.rds")
storm.data <- readRDS("data_output/storm_data.rds")


# SUBSET BY DRAINAGE  -----------------------------------------------------

# Let's get the sites with the biggest drainage areas:
site_info <- readNWISsite(sites$site_no) %>%
  arrange(desc(contrib_drain_area_va))

site_info <- site_info[1:100,]

reqBks <- seq(1,nrow(sites),by=10)
statData <- data.frame()
for(i in reqBks) {
  getSites <- site_info$site_no[i:(i+9)]
  currentSites <- readNWISstat(siteNumbers = getSites,
                               parameterCd = "00060", 
                    statReportType="daily",
                    statType=c("p10","p25","p50","p75","p90","mean"))
  statData <- bind_rows(statData,currentSites)
}

statData$Date <- as.Date(paste("2017",
                                zeroPad(statData$month_nu, 2),
                                zeroPad(statData$day_nu, 2), 
                                sep = "-"))
today_stats <- statData %>%
  filter(Date == Sys.Date())

# discharge stats
discharge <- select(storm.data, site_no, Flow) %>%
  group_by(site_no) %>%
  summarise(Flow = max(Flow, na.rm = TRUE)) %>%
  filter(!is.na(Flow),
         site_no %in% site_info$site_no) %>%
  left_join(select(today_stats, site_no, p10_va, p25_va, p50_va, p75_va, p90_va), by="site_no") %>%
  left_join(select(sites, site_no, dec_lat_va, dec_lon_va), by="site_no")

discharge$class <- NA

discharge$class[discharge$Flow > discharge$p75_va] <- ">75%"
discharge$class[discharge$Flow < discharge$p25_va] <- "<25%"

discharge$class[discharge$Flow > discharge$p25_va & 
                  discharge$Flow <= discharge$p50_va] <- "25%-50%"
discharge$class[discharge$Flow > discharge$p50_va &
                  discharge$Flow <= discharge$p75_va] <- "50%-75%"

discharge <- filter(discharge, !is.na(class))
discharge$class <- as.factor(discharge$class)
levels(discharge$class) <- c(">75%","50%-75%","25%-50%","<25%")

hurricane_harvey <- ggplot() +
  geom_polygon(data = counties, aes(x = long, y=lat, group=group, fill = Precipitation), color = "white") +
  geom_path(data = states, aes(x = long, y=lat, group=group),
               color = "black") +
  geom_path(data = rivers_fortify, aes(x=long, y=lat, group=group), color = "lightblue", size = 0.1)+
  geom_point(data = discharge, aes(x = dec_lon_va, y = dec_lat_va, color = class)) +
  geom_polygon(data = windswath_fortify, aes(x=long, y=lat, group=group), alpha = 0.5) + 
  geom_polygon(data = radii_fortify, aes(x=long, y=lat, group=group)) +
  geom_path(data = lines_fortify, aes(x=long, y=lat), 
            color = "red", size = 2) +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "blue",name = "Precip [mm]") +
  scale_colour_manual(values = c(">75%"="red", "50%-75%"="blue", "25%-50%"="green", "<25%"="yellow"), name = "River Discharge") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

hurricane_harvey

ggsave(hurricane_harvey, filename = "docs/harvey20170826.pdf")
