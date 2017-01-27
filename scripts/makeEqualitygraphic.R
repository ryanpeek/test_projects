# MAKE EQUALITY SYMBOL ----------------------------------------------------


library(ggplot2)
library(grid)

bg <- data.frame(xmin = 0, xmax = 180, ymin = 0, ymax = 180)
bg_col <- "blue"

bars <- data.frame(xmin = c(35, 35), xmax = c(145, 145), ymin = c(45, 100), ymax = c(80, 135), groups = c("first", "second") )
bars_col <- "yellow"

p <- ggplot(bg, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(fill = bg_col)+
  geom_rect(data = bars, fill = bars_col, aes(group = groups))+
  coord_fixed()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+  
  theme(line = element_blank(),
        text = element_blank(),
        rect = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.ticks.length = unit(0, "cm"))

p

ggsave("equality.png", plot = p, width = 8, height = 8)