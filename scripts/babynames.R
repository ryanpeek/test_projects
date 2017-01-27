## BABY NAMES

library(dplyr)     # provides data manipulating functions.
library(magrittr)  # ceci n'est pas un pipe
library(ggplot2)   # for graphics
library(stringr)   # for string detections
library(babynames)

babynamess <- babynames

# filter to Males and create a "decade" column for grouping, sample 5 per decade

samps<-babynamess %>%
  filter(sex=='M') %>%
  mutate(decade = plyr::round_any(year, accuracy = 10, f = floor)) %>%
  group_by(decade) %>%
  sample_n(5) %>% arrange(year)

as.data.frame(samps)


# filter to specific name and summarize 
babynames %>%
  filter(stringr::str_detect(name, 'Clar')) %>%
  group_by(year, sex) %>%
  summarize(total = sum(n)) %>% 
  ggplot(., aes(year, total, color = sex)) + geom_line() + 
  ggtitle(label = 'Names starting with "Cla"') + 
  theme(plot.title = element_text(hjust = 0.5)) 
