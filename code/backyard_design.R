# Cubic Yards = Length (in feet) Width (in feet) Depth (in feet) ÷ 27

mulch_depth <- 2 # in inches
full_backyard <- (32*30*(mulch_depth/12))
driveway_side <- 11*6*(mulch_depth/12)

(mulch_yd3 <- (full_backyard + driveway_side)/27)


plot(c(0, 9, 18.5, 28, 32, 40.5, 46), c(0, 11, 11, 14, 20.5, 20.5, 42))
