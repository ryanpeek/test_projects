#pomo theme

devtools::install_github("gadenbuie/ggpomological")

library(ggpomological)
library(dplyr)

scales::show_col(ggpomological:::pomological_palette)

# Base plot
basic_iris_plot <- ggplot(iris) +
  aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
  geom_point(size = 2)

# Just your standard Iris plot
basic_iris_plot 

# With pomological colors
basic_iris_plot <- basic_iris_plot + scale_color_pomological()
basic_iris_plot

# With pomological theme
basic_iris_plot + theme_pomological()

# With transparent background
basic_iris_plot + theme_pomological_plain() 

# Or with "fancy" pomological settings

# see these scripts on Google and download?:
#https://statnmap.com/2018-04-18-draw-maps-like-paintings/
#Gaegu
#Nanum Pen Script
#Reenie Beanie


library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Gochi Hand", "gochi")
font_add_google("Schoolbell", "bell")
font_add_google("Covered By Your Grace", "grace")
font_add_google("Rock Salt", "rock")
font_add_google("Marck Script", "marck")
font_add_google("Mr Bedfort", "bedfort")
font_add_google("Mr De Haviland", "Mr De Haviland")
showtext_auto(enable = TRUE)

extrafont::font_import()
extrafont::fonttable()

pomological_iris <- basic_iris_plot + theme_pomological_fancy(base_family = "Lato")
pomological_iris
