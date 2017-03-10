# trackeR

# install.packages("trackeR")
# install.packages("devtools")
devtools::install_github("hfrick/trackeR")

library(trackeR)

data(runs, package = "trackeR")
runsSummary <- summary(runs)
plot(runsSummary, group = c("total", "moving"),
     what = c("avgSpeed", "distance", "duration", "avgHeartRate"))


# READ IN DATA ------------------------------------------------------------

# best to use TCX format

filepath<-file.path("~/Downloads/TCX/2016-03-24-D5-Fernglen.tcx")
df <- readTCX(file = filepath)

str(df)
summary(df, session = 1, movingThreshold = 1)

library(ggplot2); library(viridis)
ggplot(data = df, aes(x=longitude, y=latitude, color=altitude)) + geom_point() +
  scale_color_viridis(option = "C")

data("runs", package = "trackeR")
plot(runs, session = 1:7)
plot(runs, session = 27, what = c("altitude", "pace"))
plotRoute(runs, session = 4, zoom = 13, source = "google")
leafletRoute(runs, session = 8:13)

summary(runs, session = 1, movingThreshold = 1)