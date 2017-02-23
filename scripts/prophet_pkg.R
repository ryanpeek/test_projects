# prophet package
# https://github.com/facebookincubator/prophet

# install.packages("prophet")

library(prophet)
library(readr)
library(dplyr)


# example: https://facebookincubator.github.io/prophet/docs/quick_start.html

# R
df <- read_csv('data/example_wp_peyton_manning.csv') %>%
  mutate(y = log(y))

# get into prophet
m <- prophet(df)

# R futures
future <- make_future_dataframe(m, periods = 365)
tail(future)

# R forecast
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# plot
plot(m, forecast)

prophet_plot_components(m, forecast)
