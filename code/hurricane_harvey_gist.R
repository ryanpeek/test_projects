# harvey_graph_v2

library(maps)
library(ggplot2)
library(dplyr)
library(rgdal)
library(geoknife)
library(tidyr)
library(dataRetrieval)
library(httr)
library(gganimate)

state_names <- c("texas","louisiana")

states <- map_data("state") %>%
  filter(region %in% state_names)

counties <- map_data("county",state_names) 

nhc.url <- "http://www.nhc.noaa.gov/gis/best_track/%s%s%s_best_track.zip"
download.url <- sprintf(nhc.url, "al", "09", "2017")
temp_path <- tempdir()
download.file(download.url, destfile = paste0(temp_path,"/al092017_best_track.zip"))

unzip(zipfile = paste0(temp_path, '/al092017_best_track.zip'), exdir = temp_path)


lines <- readOGR(dsn=temp_path, layer="AL092017_lin")
lines@data$id <- rownames(lines@data)
lines_fortify <- fortify(lines) %>%
  filter(long < -88,
         lat >22) %>%
  left_join(lines@data, by="id")

radii <- readOGR(dsn=temp_path, layer="AL092017_radii")
radii@data$id <- rownames(radii@data)
radii_fortify <- fortify(radii) %>%
  filter(long < -88,
         lat >22) %>%
  left_join(radii@data, by = "id") %>%
  mutate(dateTime = as.POSIXct(strptime(as.character(SYNOPTIME), "%Y%m%d%H"),tz = "UTC"))
attr(radii_fortify$dateTime, "tzone") <- "America/Chicago"

windswath <- readOGR(dsn=temp_path, layer="AL092017_windswath")
windswath@data$id <- rownames(windswath@data)
windswath_fortify <- fortify(windswath) %>%
  filter(long < -88,
         lat >22) %>%
  left_join(windswath@data, by="id") %>%
  mutate(id = as.numeric(id)) %>%
  filter(id == max(id)) %>%
  mutate(id = as.character(id))

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

start_time <- min(radii_fortify$dateTime)
end_time <- Sys.time()
attr(end_time, "tzone") <- "America/Chicago"

precipData <- getPrecip(states = state_names, 
                        startDate = start_time, 
                        endDate = end_time)

# It seems to come back with the right date/time, wrong timezone...
attr(precipData$DateTime, "tzone") <- "America/Chicago"
precipData$DateTime <- as.POSIXct(precipData$DateTime + 5*60*60, tz = "America/Chicago")

cumulative_precip <- data.frame()
# timeStamps <- c(unique(radii_fortify$dateTime),max(precipData$DateTime))
timeStamps <- seq.POSIXt(from = unique(radii_fortify$dateTime)[1],
                         to = max(precipData$DateTime), by = "6 hours")

for(i in timeStamps){
  sub_precip <- filter(precipData, DateTime <= i) %>%
    group_by(statename, statecounty, county) %>% 
    summarize(Precipitation = sum(precipVal)) %>%
    mutate(dateTime = i)
  
  cumulative_precip <- bind_rows(cumulative_precip, sub_precip)
}

counties <- left_join(counties, cumulative_precip, by=c("region"="statename",
                                                        "subregion"="county"))
counties$dateTime <- as.POSIXct(counties$dateTime, 
                                origin = "1970-01-01", tz = "America/Chicago")

counties$precip_in <- counties$Precipitation * 0.0393701
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
  
  unzip(destination, exdir = filePath)
  flowLines = readOGR(filePath, layer='nhdflowline_network')
  
  return(flowLines)
}

mapRange <- c(range(counties$long),range(counties$lat))
# rivers <- get_flowlines(6, mapRange)
# rivers_fortify <- fortify(rivers)
# saveRDS(rivers_fortify, file = "texas_rivers.rds")
rivers_fortify <- readRDS("content/texas_rivers.rds")

states_abb <- c("TX","LA")
sites <- data.frame()

for(st in states_abb){
  sites_st <- whatNWISdata(stateCd = st,
                           parameterCd="00060") %>%
    filter(parm_cd == "00060",
           data_type_cd == "uv",
           end_date >= start_time) %>%
    arrange(desc(count_nu)) %>%
    filter(count_nu > 5000)
  
  sites <- bind_rows(sites, sites_st)
  
}

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

storm.data <- readNWISuv(siteNumbers = site_info$site_no,
                         parameterCd = "00060",
                         startDate = as.Date(start_time),
                         endDate = "", tz = "America/Chicago")
storm.data <- renameNWISColumns(storm.data)

discharge <- select(storm.data, site_no, dateTime, Flow_Inst) %>%
  rename(Flow=Flow_Inst) %>%
  filter(!is.na(Flow)) %>%
  mutate(Date = as.Date(dateTime)) %>%
  left_join(select(statData, site_no, Date, p10_va, p25_va, p50_va, p75_va, p90_va), 
            by=c("site_no","Date")) %>%
  left_join(select(sites, site_no, dec_lat_va, dec_long_va), by="site_no")

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

# Probably should do some smoothing here
discharge_sub <- discharge %>%
  filter(dateTime %in% timeStamps)

discharge_last <- discharge %>%
  group_by(site_no) %>%
  filter(dateTime == max(dateTime)) %>%
  mutate(dateTime = as.POSIXct(timeStamps[length(timeStamps)], tz = "America/Chicago"))

discharge_combo <- bind_rows(discharge_sub, 
                             discharge_last)

g1 <- ggplot() +
  geom_polygon(data = counties, 
               aes(x = long, y=lat, group=group, 
                   fill = precip_in, frame = dateTime), color = "white") +
  geom_path(data = states, aes(x = long, y=lat, group=group),
            color = "black") +
  geom_path(data = rivers_fortify,
            aes(x=long, y=lat, group=group),
            color = "lightblue", size = 0.1)+
  geom_point(data = discharge_combo, 
             aes(x = dec_long_va, y = dec_lat_va, 
                 color = class, frame = dateTime)) +
  geom_polygon(data = windswath_fortify, 
               aes(x=long, y=lat, group=group), 
               alpha = 0.5, color = "grey90") + 
  geom_polygon(data = radii_fortify, 
               aes(x=long, y=lat, group=group, 
                   frame = dateTime), 
               alpha = 0.5,fill="lightblue") +
  geom_path(data = lines_fortify, aes(x=long, y=lat), 
            color = "red", size = 2) +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "blue",
                      name = "Cumulative\nPrecip\n[in]") +
  scale_colour_manual(values = c(">75%"="red", "50%-75%"="blue", "25%-50%"="green", "<25%"="yellow"), 
                      name = "River Discharge\n% Based on Historical Record") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

gganimate(g1)