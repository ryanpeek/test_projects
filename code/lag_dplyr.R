
# Read in Data ------------------------------------------------------------

library(tidyverse)
dat <- read_delim("data/iso_year_data.tsv", delim = " ")



# Arrange and Group -------------------------------------------------------

# this is grouped by ISO, so only comparing differences WITHIN ISO3
dat_iso_diff <- dat %>% group_by(ISO3) %>% arrange(year) %>% 
  mutate(DV_diff = DV - lag(DV),
         emp_diff = emp - lag(emp),
         fam_diff = fam - lag(fam),
         edu_diff = edu - lag(edu))


# this is not grouped, but arranged by year
# so comparing differences between rows (ISO or year combos)
dat_diff <- dat %>% arrange(year, ISO3) %>% 
  mutate(DV_diff = DV - lag(DV),
         emp_diff = emp - lag(emp),
         fam_diff = fam - lag(fam),
         edu_diff = edu - lag(edu))
