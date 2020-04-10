library(tidyverse)

messydata <- read_csv(file = "https://raw.githubusercontent.com/datacarpentry/organization-genomics/gh-pages/files/SampleSheet_Example_messy.csv")

# write out as csv:
write_csv(messydata, path="data/messy_data_genomics.csv")

# importantly: tidy data is one observation per row, one variable per column

# cleaned csv:
cleaned_data <- read_csv(file= "https://raw.githubusercontent.com/datacarpentry/organization-genomics/gh-pages/files/SampleSheet_Example_clean.csv")
