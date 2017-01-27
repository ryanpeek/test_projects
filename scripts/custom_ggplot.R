# custom ggplot, via Noam Ross

library(png) #For reading in the image.  There's a similar package `jpeg`
library(ggplot2)
library(grid)  #For converting the image to something usable in ggplot2
library(dplyr)


# some data
fish2 = data.frame(habitat = c("mudflat", "channel", "marsh"), catch = c(3,2,6))

fish2 = fish2 %>%               # This pipeline create multiple points for each value
  group_by(habitat) %>%
  do(data_frame(habitat=.$habitat, catch=1:.$catch)) %>%
  group_by()

# get your picture
img <- readPNG(system.file("img", "Rlogo.png", package="png")) # Read in your fish
img <- readPNG(source = "./data/frog_drawing.png") # Read in your frog

# make grid object from image
g <- rasterGrob(img, interpolate=FALSE)  # Setting interpolate=TRUE will smooth it.

# Plot!
ggplot(fish2, aes(habitat, catch)) +   # Set up the plot
  mapply(function(xx, yy, ii) {        # Loop over your data frame
    g$name <- ii                       # Create an object for each point named for the row number
    
    # Add the image object, mess with the offsets to set its size and positioning
    annotation_custom(g, xmin=xx-1, xmax=xx+1, ymin=yy-0.3, ymax=yy+0.3)}, 
    # Loop over x value, y value, and row number. Note the need to convert the factor to a number.
    as.numeric(fish2$habitat), fish2$catch, seq_len(nrow(fish2)))   
