# weatheR package:
# see http://ecohen4.github.io/Cohen-McCreight/weatheR.html

#require(devtools)
#install_github("mpiccirilli/weatheR")
require(weatheR)
library(tidyverse)

# get all stations from NOAA
stations<- read_csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv") %>% 
  rename(station_name=`STATION NAME`,
         elev_m = `ELEV(M)`)

head(stations)

# cities of interest
cities.of.interest <- c("Davis", "Sacramento")

# great circle

deg2rad <- function(deg) return(deg*pi/180)

gcd.slc <- function(long1, lat1, long2, lat2) {
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  R <- 6371 # Earth mean radius in km
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

# get k-Nearest Stations
kNStations <- function(city.list, station.list, k = 5)
{
  # Find geocodes of each city
  coords <- suppressMessages(geocode(city.list))
  
  # Loop through each location, finding the k-nearest stations from the main list:
  kns.ix <- NULL # create a variable to track row index the stations
  kns.dist <- NULL # create a distance variable
  for(i in 1:length(city.list))
  {
    # Great cirle distance calculator:
    dist <- gcd.slc(coords$lon[i], coords$lat[i], station.list$LON, station.list$LAT)
    distSort <- sort(dist, ind=TRUE) 
    tmp.ix <- distSort$ix[1:k] # temporary index variable for the loop
    tmp.dist <- distSort$x[1:k] # temporary distance variable for the loop
    kns.ix <- c(kns.ix, tmp.ix) # append row index of stations for each location
    kns.dist <- c(kns.dist, tmp.dist) # append distances of stations for each location
  }
  st <- station.list[kns.ix,] # Subset the full list with k-nearest stations
  st$city <- rep(city.list, each=k) # Insert reference City
  st$Ref_Lat <- rep(coords$lat,each=k) # Insert reference Latitude
  st$Ref_Lon <- rep(coords$lon, each=k) # Insert reference Longitude
  
  st$kilo_distance <- kns.dist # Insert distance into result
  st <- st[with(st,order(city, kilo_distance)),]
  st$rank <- rep(1:k,length(city.list)) # Rank closest to farthest (1 to k)
  
  # Queries are made to NOAA database by year:
  st$BEGIN_Year <- as.numeric(substr(st$BEGIN,1,4)) # Start Year
  st$END_Year <- as.numeric(substr(st$END, 1, 4)) # End Year
  return(st)
}

# plot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL)
{
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout))
  {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1){
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plotStations <- function(city.list, station.list, k)
{
  kns <- kNStations(city.list, station.list, k)
  nc <- length(city.list)
  plots <- list()
  for (i in 1:nc)
  {
    map <- suppressMessages(get_map(location = city.list[i], zoom = 10))
    p1 <- suppressMessages(ggmap(map) +
                             geom_point(aes(x = LON, y = LAT),
                                        data = kns[kns$city==city.list[i],],
                                        colour="red", size=7, alpha=.5) +
                             geom_text(aes(x = LON, y = LAT, label=rank),
                                       data = kns[kns$city==city.list[i],]) +
                             geom_point(aes(x = lon, y = lat),
                                        data = geocode(city.list[i]),
                                        colour="black", size=7, alpha=.5)) +
      labs(title=city.list[i]) + theme(plot.margin=unit(c(0,0,0,0),"mm"))
    plots[[i]] <- p1
  }
  if (nc == 1) plot(p1) else multiplot(plotlist = plots, cols = round(sqrt(nc)))
}

# Run the function:
plotStations(cities.of.interest[1:2], stations, 7)



# DOWNLOAD BY FTP ---------------------------------------------------------

col.width <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1,
               1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1)

col.names <- c("CHARS", "USAFID", "WBAN", "YR", "M", "D", "HR", "MIN",
               "DATE.FLAG", "LAT", "LONG", "TYPE.CODE", "ELEV", "CALL.LETTER",
               "QLTY", "WIND.DIR", "WIND.DIR.QLTY", "WIND.CODE",
               "WIND.SPD", "WIND.SPD.QLTY", "CEILING.HEIGHT", "CEILING.HEIGHT.QLTY",
               "CEILING.HEIGHT.DETERM", "CEILING.HEIGHT.CAVOK", "VIS.DISTANCE",
               "VIS.DISTANCE.QLTY", "VIS.CODE", "VIS.CODE.QLTY",
               "TEMP", "TEMP.QLTY", "DEW.POINT", "DEW.POINT.QLTY",
               "ATM.PRES", "ATM.PRES.QLTY")


df <- read_fwf(file = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/2012/716380-99999-2012.gz", fwf_widths(col.width, col.names))



dlStationData <- function(kns, beg, end)
{
  base.url <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/"
  nstations <- nrow(kns)
  yrs <- seq(beg,end,1)
  nyrs <- end-beg+1
  usaf <- as.numeric(kns$USAF)
  wban <- as.numeric(kns$WBAN)
  # The following dataframe will be printed to show
  # which files were successfully downloaded
  status <- data.frame()
  temp <- as.data.frame(matrix(NA,nstations,5))
  names(temp) <- c("File","Status", "City", "rank", "kilo_distance")
  temp$City <- kns$city
  temp$rank <- kns$rank
  temp$kilo_distance <- kns$kilo_distance
  # Setup for the list of data
  temp.list <- df.list <- list()
  city.names <- unlist(lapply(strsplit(kns$city,", "), function(x) x[1])) # City Name
  df.names <- paste(city.names, kns$USAF, sep="_") # City Name_USAF#
  
  # Download the desired stations into a list (does not save to disk)
  for (i in 1:nyrs)
  {
    for (j in 1:nstations)
    {
      # Create file name
      temp[j,1] <- paste(usaf[j],"-",wban[j],"-", yrs[i], ".gz", sep = "")
      tryCatch({
        # Create connect to the .gz file
        gz.url <- paste(base.url, yrs[i], "/", temp[j, 1], sep="")
        con <- gzcon(url(gz.url))
        raw <- textConnection(readLines(con))
        # Read the .gz file directly into R without saving to disk
        temp.list[[j]] <- read.fwf(raw, col.width)
        close(con)
        # Some housekeeping:
        names(temp.list)[j] <- df.names[j]
        names(temp.list[[j]]) <- col.names
        temp.list[[j]]$LAT <- temp.list[[j]]$LAT/1000
        temp.list[[j]]$LONG <- temp.list[[j]]$LONG/1000
        temp.list[[j]]$WIND.SPD <- temp.list[[j]]$WIND.SPD/10
        temp.list[[j]]$TEMP <- temp.list[[j]]$TEMP/10
        temp.list[[j]]$DEW.POINT <- temp.list[[j]]$DEW.POINT/10
        temp.list[[j]]$ATM.PRES <- temp.list[[j]]$ATM.PRES/10
        temp.list[[j]]$city <- city.names[j]
        temp.list[[j]]$distance <- kns$kilo_distance[j]
        temp.list[[j]]$rank <- kns$rank[j]
        temp[j,2] <- "Success"
      },
      error=function(cond)
      {
        return(NA)
        next
      },
      finally={ # if any of the files didn't download successfully, label as such
        if(is.na(temp[j,2])=="TRUE") temp[j,2] <- "Failed"
      })
    }
    # Combine each year's status and list
    status <- bind_rows(status, temp)
    status <- status[order(status[,3], status[,4], status[,1]),]
    df.list <- append(df.list, temp.list)
  }
  output.list <- list(status, df.list)
  names(output.list) <- c("dl_status", "station_data")
  return(output.list)
}


# data compilation
combineWeatherDFs <- function(dfList)
{
  combined.list <- list()
  keys <- unique(names(dfList$station_data))
  keys <- keys[keys!=""]
  nkeys <- length(keys)
  for (i in 1:nkeys)
  {
    track <- which(names(dfList$station_data)==keys[i])
    combined.list[[i]] <- as.data.frame(bind_rows(dfList$station_data[track]))
    names(combined.list)[i] <- keys[i]
  }
  output.list <- list(dfList$dl_status, combined.list)
  names(output.list) <- c("dl_status", "station_data")
  return(output.list)
}


# put it together:
getStationsByCity <- function(city.list, station.list, k, begin, end)
{
  kns <- kNStations(city.list, station.list, k)
  weatherDFs <- dlStationData(kns, begin, end)
  combined.list <- combineWeatherDFs(weatherDFs)
  return(combined.list)
}


tst<- getStationsByCity("DAVIS, CA", station.list = stations, k = 5, begin = 2011, end = 2012)
tst[1]
head(tst$station_data[2])
#getFilteredStationsByCity(city.list = c("Davis, CA"), station.list = stations, k = 2, begin = 2011, end = 2017, distance = 10, hourly_interval = 12, tolerance = .05)


# FILTER ------------------------------------------------------------------

filterStationData <- function(comb.list, distance, hourly_interval, tolerance, begin, end)
{
  
  dlStatus <- comb.list$dl_status
  comb.list <- comb.list$station_data
  
  city.names <- unlist(lapply(comb.list, function(x) unique(x$city)))
  
  # 1) remove stations with little to no data at all
  rm.junk <- names(comb.list[which(sapply(comb.list, function(x) dim(x)[1] <= 10))])
  comb.list <- comb.list[which(sapply(comb.list, function(x) dim(x)[1] > 10))]
  
  # 2) remove stations that exceed maximum distance
  rm.dist <- names(comb.list[which(sapply(comb.list, function(x) max(x["distance"])) > distance)])
  comb.list <- comb.list[which(sapply(comb.list, function(x) max(x["distance"])) < distance)]
  
  # keep track of which stations have been removed
  rm.tmp <- unique(c(rm.junk, rm.dist))
  
  lapply(comb.list, names)
  # 3.a) remove stations that exceed threshold of missing data,
  # start with counting the 999s:
  cl <- c("TEMP", "DEW.POINT") # Additional columns can be added
  ix <- ix.tmp <- NULL
  ix.ct <- as.data.frame(matrix(nrow=length(comb.list), ncol=length(cl)))
  colnames(ix.ct) <- cl
  rownames(ix.ct) <- names(comb.list)
  for (L in 1:length(comb.list))
  {
    for (i in 1:length(cl))
    {
      ix.tmp <- which(comb.list[[L]][cl[i]]==999.9 | comb.list[[L]][cl[i]]==999 |
                        comb.list[[L]][cl[i]]==99.99)
      ix.ct[L,i] <- length(ix.tmp)
      ix <- union(ix,ix.tmp)
    }
    comb.list[[L]] <- comb.list[[L]][-ix,]
  }
  ix.ct$temp_pct <- ix.ct[,cl[1]]/unlist(lapply(comb.list,nrow))
  ix.ct$dew_pct <- ix.ct[,cl[2]]/unlist(lapply(comb.list,nrow))
  # print(ix.ct)
  
  # ms.obs <- (ix.ct$TEMP)+(ix.ct$DEW.POINT)
  
  # 3.b) set a minimum number of observations and remove stations that do not
  # meet requirement
  yrs <- seq(begin,end,1)
  nyrs <- end-begin+1
  min.obs <- (24/hourly_interval)*365*nyrs*(1-tolerance)
  obs.ix <- which(sapply(comb.list, nrow) < min.obs)
  rm.obs <- names(comb.list[which(sapply(comb.list, nrow) < min.obs)])
  if(length(rm.obs)==0) comb.list <- comb.list else comb.list <- comb.list[-obs.ix]
  
  # update removed stations
  rm.all <- unique(c(rm.tmp, rm.obs))
  
  # 4) All current stations are assumed to be adequet,
  #   we therefore will take the closest to each reference point
  kept.names <- substr(names(comb.list),1,nchar(names(comb.list))-7)
  kept.ranks <- unname(unlist(lapply(comb.list, function(x) x["rank"][1,1])))
  f.df <- data.frame(location=kept.names, ranks=kept.ranks)
  kp.ix <- as.numeric(rownames(f.df[which(ave(f.df$ranks,f.df$location,FUN=function(x) x==min(x))==1),]))
  final.list <- comb.list[kp.ix]
  
  # Show what was removed during the filtering process:
  kept <- names(comb.list)
  st.df <- data.frame(count(city.names))
  rm.df <- count(substr(rm.all, 1, nchar(rm.all)-7))
  kept.df <- count(substr(kept, 1, nchar(kept)-7))
  df.list <- list(st.df, rm.df, kept.df)
  mg.df <- Reduce(function(...) merge(..., by="x", all=T), df.list)
  suppressWarnings(mg.df[is.na(mg.df)] <- 0)
  colnames(mg.df) <- c("city", "stations", "removed", "kept")
  filterStatus <- mg.df
  
  # Show the stations that will be in the final output:
  finalStations <- names(final.list)
  
  # Create a list for output
  finalOutput <- list(dlStatus, filterStatus, finalStations, final.list)
  names(finalOutput) <- c("dl_status", "removed_rows", "station_names_final", "station_data")
  return(finalOutput)
}

##### WHAT??

# ftp://ftp.ncdc.noaa.gov/pub/data/noaa/2012/716380-99999-2012.gz
# ftp://ftp.ncdc.noaa.gov/pub/data/noaa/2016/007026-99999-2016.gz