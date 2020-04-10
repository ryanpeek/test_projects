install.packages("fs")

library(fs)


# list files in curr dir
dir_ls()

# create a new directory
tmp <- dir_create(file_temp())
tmp

# create new files in that directory
file_create(path(tmp, "my-file.txt"))
dir_ls(tmp)

# remove files from the directory
file_delete(path(tmp, "my-file.txt"))
dir_ls(tmp)

# remove the directory
dir_delete(tmp)

# create mult dir with pipe
library(magrittr)

paths <- file_temp() %>%
  dir_create() %>%
  path(letters[1:5]) %>%
  file_create()
paths

paths %>% file_delete()

suppressMessages(
  library(tidyverse))

dir_info(recursive = TRUE) %>%
  filter(type == "file", permissions == "u+r", size > "10KB") %>%
  arrange(desc(size)) %>%
  select(path, permissions, size, modification_time)


dir_info(recursive = TRUE) %>%
  group_by(directory = path_dir(path)) %>%
  tally(wt = size, sort = TRUE)



# Create separate files for each species
iris %>%
  split(.$Species) %>%
  map(select, -Species) %>%
  iwalk(~ write_tsv(.x, paste0(.y, ".tsv")))

# Show the files
iris_files <- dir_ls(glob = "*.tsv")
iris_files

# Read the data into a single table, including the filenames
iris_files %>%
  map_df(read_tsv, .id = "file", col_types = cols(), n_max = 2)

file_delete(iris_files)


