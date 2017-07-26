# make 20 million line file of 4 words:

library(tidyverse)

# some data
words<-"dogs hate cats sometimes"

# make into tibble
dat <- tibble(X=rep(words, 20000000))

library(stringr)

# split out words:
dat2<-str_split_fixed(dat$X, pattern = " ", n=4) 

# write to RDS file with highest compression ("xz"):
write_rds(dat2, path = "string_example.rds", compress = "xz")


install.packages("devtools")  
devtools::install_github("hadley/multidplyr")  

# Load packages
library(dplyr)  
library(multidplyr)

# We will use built-in dataset - airquality
# dplyr
airquality %>% group_by(Month) %>% summarize(cnt = n())

# multidplyr
airquality %>% partition(Month) %>% summarize(cnt = n()) %>% collect()  