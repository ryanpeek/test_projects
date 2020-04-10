# rvest 

#Script to scrape NARS website
library(rvest)
library(dplyr)
nars <- read_html("https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys")

links <- nars %>%
  html_nodes(".file-link, a") %>%
  html_attr("href") %>%
  tbl_df() %>% 
  filter(grepl("files",value))

  mutate(url = paste0("https://www.epa.gov",value),
         file_name = '')

for(i in seq_along(links2$url)){
  httr::GET(links2$url[i],httr::write_disk(basename(links2$value[i]),overwrite=T))
}

  
  
  
  
# RVEST EXAMPLES: ---------------------------------------------------------

# https://rpubs.com/Radcliffe/superbowl
  
library(rvest)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
  
url <- 'http://espn.go.com/nfl/superbowl/history/winners'
webpage <- read_html(url)

sb_table <- html_nodes(webpage, 'table')
sb <- html_table(sb_table)[[1]]
head(sb)


# Example 2:

# table of women's march info
url <- 'https://docs.google.com/spreadsheets/d/1xa0iLqYKz8x9Yc_rfhtmSOJQ2EGgeUVjvV4A8LsIaxY/htmlview?sle=true#'

webpage <- read_html(url, options = "COMPACT")

sb_table <- html_nodes(webpage, 'table')
sb <- html_table(sb_table)[[1]]

# remove first 6 rows
sb <- sb[-(1:6),-c(1,3,9,13:14)]
colnames(sb) <- sb[1,]
names(sb)[1] <- "Location"
sb <- sb[-(1:4), ]
sb[,4] <- as.numeric(sb[,4])

sb <- sb %>% 
  group_by(`State/Territory`) %>% 
  tally(`Estimate1 (low)`)





