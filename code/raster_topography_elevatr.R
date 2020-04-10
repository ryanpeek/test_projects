# raster/topography

library(raster)
library(elevatr)
#devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(reshape2)
library(ggplot2)

data(lake)
x <- get_elev_point(lake, src = "epqs")
x_mat <- as.matrix(x)
x_rsh <- rayshader::rayshade(x_mat, 
                             anglebreaks = seq(55,65,10), 
                             sunangle = 315, 
                             maxsearch = 100)

ggplot(data = reshape2::melt(x_rsh)) + 
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_viridis_c() + 
  theme_void() + 
  theme(legend.position = "none")