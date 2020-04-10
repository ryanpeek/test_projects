# using Google Rmd (GMD)

# devtools::install_github("LFOD/GMD")

library(GMD)
library(dplyr) #We made it pipe friendly 

#grab an authentication token
token <- get_auth()

# get the URL for the google doc of interest:
url <- "https://docs.google.com/document/d/1jKuJ1pf3bF0M78xTjsplefEnKUM3REKwIsNQLv63tqo/edit"

# need to remove the weird "sharing bit at the end of url.

myDoc <- GMD(url, token, output_name = "docs/gRmd")


# run this and it will save a copy instantly to your working directory
myDoc()

# write code in google docs and it will show up locally:
speediness <- 1 #how many seconds between redownloads
myDoc %>% live_update(refresh_rate = speediness)
