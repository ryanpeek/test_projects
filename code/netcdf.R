# netcdf
# dealing with large netcdf files

# Reading in with ncdf4 ----------------------------------------------------
# https://cran.r-project.org/web/packages/futureheatwaves/vignettes/starting_from_netcdf.html
# see this demo https://www.r-bloggers.com/a-netcdf-4-in-r-cheatsheet/
library(ncdf4)
library(reshape2)
library(dplyr)

# retrieve a list of nc files in my data folder:
flist <- list.files(path = "data/", pattern = "^.*\\.(nc|NC|Nc|Nc)$")
flist

# Open a connection to the first file in our list
nc <- nc_open(paste0("data/", flist[1]))


# Save the print(nc) dump to a text file (same name as the nc file with a txt extension)
{
  sink(paste0("data/", flist[1], ".txt"))
  print(nc)
  sink()
}

# Get a list of the NetCDF's R attributes:
attributes(nc)$names

print(paste("The file has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"NetCDF attributes"))

# Get a list of the nc variable names.
attributes(nc$var)$names

# Take a look at the variable's nc attributes (units etc).
ncatt_get(nc, attributes(nc$var)$names[1])

# Retrieve a matrix of the data using the ncvar_get function:
var_mean <- ncvar_get(nc, attributes(nc$var)$names[1])

# Retrieve the latitude and longitude values.
attributes(nc$dim)$names

nc_lat <- ncvar_get( nc, attributes(nc$dim)$names[1])
nc_lon <- ncvar_get( nc, attributes(nc$dim)$names[2])

print(paste(dim(nc_lat), "latitudes and", dim(nc_lon), "longitudes"))

# Change the dimension names of our matrix to "lon" and "lat", 
# and the row and column names to the latitude and longitude values.
dimnames(var_mean) <- list(lon=nc_lon, lat=nc_lat)
var_mean[1:20, 1:10]

# lastly, you may want to transpose this matrix.
var_mean <- t(var_mean)
var_mean[245:247, 35:37]