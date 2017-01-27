library(tidyverse)
library(broom)

datatst <- read.table(textConnection("num  Site_id Spcod xvar    yvar
                                     137   A     S1    0   0.0000000
                                     21    A     S1   10   6.5146580
                                     22    A     S1   20  16.2866450
                                     23    A     S1   30  22.8013029
                                     24    A     S1   40  29.3159609
                                     25    A     S1   50  42.3452769
                                     26    A     S1   60  55.3745928
                                     27    A     S1   70  64.8208469
                                     28    A     S1   80  77.5244300
                                     29    A     S1   90  86.9706840
                                     30    A     S1  100 100.0000000
                                     138   B     S2    0   0.0000000
                                     31    B     S2   10  11.9047619
                                     32    B     S2   20  19.4444444
                                     33    B     S2   30  26.9841270
                                     34    B     S2   40  34.5238095
                                     35    B     S2   50  42.0634921
                                     36    B     S2   60  49.6031746
                                     37    B     S2   70  61.1111111
                                     38    B     S2   80  73.0158730
                                     39    B     S2   90  88.4920635
                                     40    B     S2  100 100.0000000"), header = TRUE)

df_model <- datatst %>%
  dplyr::group_by(Site_id, Spcod) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(yvar ~ xvar, data=.)),
         # model quality measurements
         glance = map(model, broom::glance),
         # contains parameters from model
         tidy = map(model, broom::tidy),
         # estimates and errors
         augment = map(model, broom::augment))

full_join(unnest(df_model, glance) %>% select(Site_id, Spcod, r.squared),
          unnest(df_model, tidy) %>% select(Site_id, Spcod, term, estimate)) %>% 
  spread(term, estimate) 


unnest(df_model, tidy)

unnest(df_model, glance)

unnest(df_model, augment)
