## SCRIPT BY EJH: 2017
## Plot the GPX files from BSB

# LIBRARIES ---------------------------------------------------------------

library(sf)
library(maptools)
library(plotKML)
library(ggplot2)
library(maps)
library(lubridate)
library(ggmap)
library(cowplot)
library(viridis)
library(ggrepel)

# GET FILES ---------------------------------------------------------------

# List paths to GPX files
files <- list.files("~/Downloads/bsbcomparison/GPX", full.names = T)

#files <- list.files("~/Downloads/bsbcomparison/GPX/westernstates", full.names=T, pattern = "2014")

# Consolidate routes in one data.frame
index <- c()
lat <- c()
long <- c()
time <- c()
elev <- c()

for (i in 1:length(files)) {
  route <- readGPX(files[i])
  location <- route$tracks[[1]][[1]]
  index <- c(index, rep(i, dim(location)[1]))
  elev <- c(elev, location$ele)
  lat <- c(lat, location$lat)
  long <- c(long, location$lon)
  time <- c(time, location$time)
}

routes <- data.frame(cbind(index, lat, long, time, elev), stringsAsFactors = F)
head(routes)

# NOW FORMAT AND INDEX ----------------------------------------------------

# format the times
routes$datetime<-ymd_hms(routes$time, tz="America/Los_Angeles")

# add index in seconds (for looping through)
routes$seconds <- ifelse(routes$index == "1", routes$datetime - min(routes[routes$index == 1, "datetime"]),
                         ifelse(routes$index == "2", routes$datetime - min(routes[routes$index == 2, "datetime"]),
                                ifelse(routes$index == "3", routes$datetime - min(routes[routes$index == 3, "datetime"]),
                                       ifelse(routes$index == "4", routes$datetime - min(routes[routes$index == 4, "datetime"]), -9999))))

cols_to_numeric<- c("lat", "long", "elev")
routes[,cols_to_numeric] <- sapply(routes[,cols_to_numeric], as.numeric)

head(routes)

# MAKE PLOTS IN LOOP ------------------------------------------------------

jpeg("~/Downloads/bsbcomparison/plots/IMG_%04d.JPG", height = 5, width = 5, units = "in", res = 500)
for(i in seq(1, range(routes$seconds)[2], 10)){
  print(i/60)
  plot(lat ~ long, data = routes[routes$index == "1" & routes$seconds <= i,], col = "red", type = "l", 
       xlim = range(routes$long), ylim = range(routes$lat))
  points(lat ~ long, data = routes[routes$index == "1" & routes$seconds <= i & routes$seconds > i-5,], col = "red", pch = 19)
  lines(lat ~ long, data = routes[routes$index == "2" & routes$seconds <= i,], col = "blue")
  points(lat ~ long, data = routes[routes$index == "2" & routes$seconds <= i & routes$seconds > i-5,], col = "blue", pch = 19)
  lines(lat ~ long, data = routes[routes$index == "3" & routes$seconds <= i ,], col = "darkgreen")
  points(lat ~ long, data = routes[routes$index == "3" & routes$seconds <= i & routes$seconds > i-10,], col = "darkgreen", pch = 19)
  lines(lat ~ long, data = routes[routes$index == "4" & routes$seconds <= i ,], col = "orange")
  points(lat ~ long, data = routes[routes$index == "4" & routes$seconds <= i & routes$seconds > i-10,], col = "black", pch = 21, bg="orange")
  legend("bottomleft", legend = c("2015-EJH", "2016-EJH", "2017-EJH", "2017-RAP"), pch = 19, col = c("red", "blue", "darkgreen", "orange"), lty = 1)
}
dev.off()


# MAKE GGMAP PLOT ---------------------------------------------------------

# get a ggmap basemap background, use the extent from the data:
theme_set(theme_bw(12))
basemap <- get_map(location = c(max(routes$long),min(routes$lat),min(routes$long), max(routes$lat)),
                   #color="bw",
                   source="google", maptype="terrain", zoom=14, crop=TRUE) # "terrain"

gg <- ggmap(basemap, extent='panel',padding=0) + coord_cartesian() 


# TESTPLOTS ---------------------------------------------------------------

# mapplot<-gg + geom_path(data=routes[!routes$index=="5",], aes(x=long, y=lat, color=elev), show.legend = T,
#                         alpha=0.95, lwd=1) + viridis::scale_color_viridis() + 
#   geom_point(data=routes[!routes$index=="5" & routes$seconds >= 400 & 
#                                 routes$seconds <= 455, ], 
#                   aes(x=long, y=lat, fill=index), pch=21, size=4, show.legend = T,alpha=0.95) +
#   geom_text(data=routes[!routes$index=="5" & routes$seconds >= 400 & 
#                                  routes$seconds <= 455, ], 
#                    aes(x=long, y=lat, label=index), alpha=0.95, color="gray80",check_overlap=TRUE)
# mapplot

# elev plot:
# elevplot<-ggplot() + geom_line(data=routes[routes$index=="1" | routes$index=="2",], aes(x=seconds, y=elev, color=elev, group=index), lwd=1) +
#   scale_color_viridis()
# elevplot

# cowplot to stitch the two together
# plot_grid(mapplot, elevplot, align = "v",nrow = 2, rel_heights = c(2,.7))

# MAKE GGPLOT-MAP FUNCTION: MULTI TRACK -----------------------------------

running_map <- function(loopID){
  
  # first track
  mapplot<-gg + 
    geom_path(data=routes[routes$index == "1" & routes$seconds <= loopID,], 
              aes(x=long, y=lat, color=elev), show.legend = FALSE,
              alpha=0.95, lwd=1) + 
    geom_point(data=routes[routes$index == "1" & routes$seconds <= loopID & routes$seconds > loopID-5,],
               aes(x=long, y=lat), fill="#000004FF",
               alpha=0.95, pch = 21, size=3) +
    # geom_text(data=routes[routes$index == "1" & routes$seconds <= loopID & routes$seconds > loopID-5,],
    #           aes(x=long, y=lat, label=index), alpha=0.95, color="gray80", check_overlap=TRUE, show.legend = FALSE) +
    
    # second track
    geom_path(data=routes[routes$index == "2" & routes$seconds <= loopID,], 
              aes(x=long, y=lat, color=elev), show.legend = FALSE,
              alpha=0.95, lwd=1) + 
    geom_point(data=routes[routes$index == "2" & routes$seconds <= loopID & routes$seconds > loopID-5,],
               aes(x=long, y=lat), fill="#721F81FF",
               alpha=0.95, pch = 21, size=3) + 
    # geom_text(data=routes[routes$index == "2" & routes$seconds <= loopID & routes$seconds > loopID-5,],
    #           aes(x=long, y=lat, label=index), alpha=0.95, color="gray80", check_overlap=TRUE, show.legend = FALSE) +
    
    # third track
    geom_path(data=routes[routes$index == "3" & routes$seconds <= loopID,], 
              aes(x=long, y=lat, color=elev), show.legend = FALSE,
              alpha=0.95, lwd=1) + 
    geom_point(data=routes[routes$index == "3" & routes$seconds <= loopID & routes$seconds > loopID-5,],
               aes(x=long, y=lat), fill="#F1605DFF",
               alpha=0.95, pch = 21, size=3) + 
    # geom_text(data=routes[routes$index == "3" & routes$seconds <= loopID & routes$seconds > loopID-5,],
    #           aes(x=long, y=lat), label="17-ejh", alpha=0.95, color="black", check_overlap=TRUE, show.legend = FALSE) +
    # fourth track
    geom_path(data=routes[routes$index == "4" & routes$seconds <= loopID,], 
               aes(x=long, y=lat, color=elev), show.legend = FALSE,
               alpha=0.95, lwd=1) + 
    geom_point(data=routes[routes$index == "4" & routes$seconds <= loopID & routes$seconds > loopID-5,],
               aes(x=long, y=lat), fill="#FCFDBFFF",
               alpha=0.95, pch = 21, size=3) + scale_color_viridis()
    # geom_text(data=routes[routes$index == "4" & routes$seconds <= loopID & routes$seconds > loopID-5,],
    #           aes(x=long, y=lat  ), label="RAP", alpha=0.95, color="red3", check_overlap=TRUE, show.legend = FALSE) +
    # scale_color_viridis()
  
  # elevation plot
  elevplot<-ggplot() + 
    geom_line(data=routes[routes$index == "1" ,], aes(x=seconds, y=elev, color=elev)) +
    geom_line(data=routes[routes$index == "3" ,], aes(x=seconds, y=elev, color=elev), alpha=0.7) +
    geom_line(data=routes[routes$index == "4" ,], aes(x=seconds, y=elev, color=elev), alpha=0.7) +
    scale_color_viridis() +
    # now points
    geom_point(data=routes[routes$index == "1" & routes$seconds <= loopID & routes$seconds > loopID-5,],
               aes(x=seconds, y=elev), fill="#000004FF", show.legend = FALSE,
               alpha=0.95, pch = 21, size=4) + 
    geom_point(data=routes[routes$index == "2" & routes$seconds <= loopID & routes$seconds > loopID-5,],
               aes(x=seconds, y=elev), fill="#721F81FF", show.legend = FALSE,
               alpha=0.95, pch = 21, size=4) + 
    geom_point(data=routes[routes$index == "3" & routes$seconds <= loopID & routes$seconds > loopID-5,],
             aes(x=seconds, y=elev), fill="#F1605DFF", show.legend = FALSE,
             alpha=0.95, pch = 21, size=4) + 
    geom_point(data=routes[routes$index == "4" & routes$seconds <= loopID & routes$seconds > loopID-5,],
               aes(x=seconds, y=elev), fill="#FCFDBFFF", show.legend = FALSE,
               alpha=0.95, pch = 21, size=4)
    labs(xlab(""))
  
    # now plot together
    plot_grid(mapplot, elevplot, align = "v",nrow = 2, rel_heights = c(2,.7))
    # now save
    ggsave(filename=sprintf("~/Downloads/bsbcomparison/plots/IMG_%04d.jpg", loopID), height=5, width=5, units="in")
}


### RUN WITH LOOP
system.time(
  for(i in seq(1, range(routes$seconds)[2], 10)){
    print(i/60)
    running_map(i)
  }
)

# user  system elapsed 
# 774.042  24.565 829.018 

# MAKE GGPLOT-MAP FUNCTION: SINGLE TRACK -----------------------------------

running_map <- function(loopID){
  mapplot<-gg + 
    geom_point(data=routes[routes$index == "1" & routes$seconds <= loopID,], 
               aes(x=long, y=lat, color=elev), show.legend = FALSE,
               alpha=0.95, lwd=2) + 
    viridis::scale_color_viridis() +
    geom_point(data=routes[routes$index == "1" & routes$seconds <= loopID & routes$seconds > loopID-5,],
               aes(x=long, y=lat, fill=index), show.legend = FALSE,
               alpha=0.95, pch = 21, size=3)
  elevplot<-ggplot() + geom_line(data=routes[routes$index == "1" ,],
                                 aes(x=datetime, y=elev, color=elev)) + 
    geom_point(data=routes[routes$index == "1" & routes$seconds <= loopID & routes$seconds > loopID-5,],
               aes(x=datetime, y=elev, fill=index), show.legend = FALSE,
               alpha=0.95, pch = 21, size=4) + labs(xlab("")) +
    viridis::scale_color_viridis()
  plot_grid(mapplot, elevplot, align = "v",nrow = 2, rel_heights = c(2,.7))
  ggsave(filename=sprintf("~/Downloads/bsbcomparison/plots/IMG_%04d.jpg", loopID), height=5, width=5, units="in")
}

# RUN WITH LOOP

system.time(
  for(i in seq(1, range(routes$seconds)[2], 20)){
    print(i/60)
    running_map(i)
  }
)

# user  system elapsed 
# 21.966   0.946  23.773 

# NOW MAKE MOVIE ----------------------------------------------------------

#FFMPEG
setwd("~/Downloads/bsbcomparison/plots")

# make sure files are numerically ordered:
system(command = paste('ls *jpg | cat -n | while read n f; do mv "$f" `printf "IMG_%04d.jpg" $n`; done'))

# RUN ON MAC (use "system" instead of shell)
system(paste('ffmpeg -r 20 -start_number 0001 -i IMG_%4d.JPG -s 1280x720 ggbsb100.mp4'))

