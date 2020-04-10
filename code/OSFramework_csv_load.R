# reading a large file from csv using OSFramework
# see this tweet: https://twitter.com/joshdeleeuw/status/950737761253945344

# upload Files to Public Project, then load in R:

library(httr)
library(magrittr)
data <- GET("osf.io/xxxxx/?action=...", progress()) %>% 
  content(as='parsed')

