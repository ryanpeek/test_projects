# plotly interp figure for Andres
# 2017 - R. Peek

# Packages ----------------------------------------------------------------

library(tidyverse)
library(plotly)
library(akima)
library(fields)
library(viridis)
library(raster)

# Data --------------------------------------------------------------------

idw <- read_csv(file = "data/IDW.csv")

glimpse(idw)
summary(idw)

# so mean height for this "layer" is ~ 6.0, we'll use this to re-assign height

# pull out variables
x <- idw$Longitude # longitude is X typically
y <- idw$Latitude
z <- idw$RH

# Linear Interpolation ----------------------------------------------------

# using the interp linear method:
s <- interp(x, y, z=idw$RH, linear = TRUE)

glimpse(s) # quick check of data

# Raster plot
image.plot(s, nlevel = 20, col = viridis(n=20))
points(x, y)

# Contour plot: Nice, but can't overlay points/layers
filled.contour(s$x,s$y,s$z, color.palette = viridis)

# Convert to Raster (to extract XY)
rh.l <- raster(s) # make a raster

# Extract Data to Dataframe from Raster
rh.linear <- as.data.frame(rasterToPoints(rh.l)) %>% 
  rename(RH=layer) %>% # rename to the RH variable
  mutate(height=mean(idw$Height))

# Plotly Plot
plot_ly(rh.linear, x=~x, y=~y, z=~height, marker=list(color=~RH, colorscale=viridis(n = 12), show.scale=TRUE)) %>%
  add_markers() 

# Spline Interpolation ----------------------------------------------------

# alternative method might be a better fit for data
test.spline <- Tps(data.frame(x,y), z, miles = F, lon.lat = T)

# make grid 90 x 90 to match orig number of points
fullRH.grid <- predictSurface(test.spline, nx = 90, ny = 90) 

# raster plot
image.plot(fullRH.grid, nlevel = 20, col = viridis(n=20), legend.args = list(text="RH", line=0.5), main=paste0("Relative Humidity at: ", round(mean(idw$Height), digits = 2), "m"))
points(x, y) # add points from orig dataset

# Contour plot: notice grid and contours
filled.contour(fullRH.grid, color.palette = viridis) 

# Convert to Raster (to extract XY)
rh.sp <- raster(fullRH.grid)
rh.spline <- as.data.frame(rasterToPoints(rh.sp)) %>% 
  rename(RH=layer) %>% # rename to the RH variable
  mutate(height=mean(idw$Height))

# Plotly Plot
plot_ly(rh.spline, x=~x, y=~y, z=~height, marker=list(color=~RH, colorscale=viridis(n = 12), show.scale=TRUE)) %>%
  add_markers() 

# Make A Stacked Plot -----------------------------------------------------

dat <- rh.spline # use the data layer rh.linear or rh.spline

# create the layers
dat.H1 <- dat # just copy over data from above
dat.H1$height <- 15 # set a different mean height

dat.H2 <- dat # just copy over data from above
dat.H2$height <- 21 # set a different mean height

# bind all data together
stacked.dat<- bind_rows(dat, dat.H1, dat.H2)
summary(stacked.dat) # final data frame

# Stacked plotly
plot_ly(stacked.dat, x=~x, y=~y, z=~height) %>%
  add_markers(color=~RH) %>% 
  #colorbar(title = "Relative Humidity") %>% 
  layout(scene = list(xaxis = list(title = 'Longitude'),
                      yaxis = list(title = 'Latitude'),
                      zaxis = list(title = 'Height')))


