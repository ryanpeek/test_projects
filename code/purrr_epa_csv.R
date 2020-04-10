# purrr & EPA

library(purrr)
library(tidyverse)


# read in files and add filename col to each
epa <- dir(path="data/epa/tst", pattern="*.csv$", full.names = T) %>% 
  set_names() %>% 
  map(read.csv) %>% 
  imap(~mutate(.x, filename=.y))
  #imap(~transform(.x, filename = .y))

# view first few rows of data
head(epa[[1]])
# view how many records in rows, cols
dim(epa[[1]])

# view dim and summary all at once
dplyr::glimpse(epa[[1]])

# convert list to data frame
epa <- epa %>% bind_rows()

# walk returns input invisibly so good for side effects, like writing files out.
  
# imap allows you to map simultaneously over x and its indices (names or positions)


# alternatively:
epa_dat <- data_frame(filename = basename(dir(path="data/epa/tst", pattern="*.csv$", full.names = T))) %>% # create dataframe with column of the filenames
  mutate(file_contents = map(filename, # read files in
                             ~read.csv(file.path("data/epa/tst", .)))) %>% 
  unnest # this unlists all the list of dataframe
