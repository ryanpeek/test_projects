# tidyr

library(dplyr)
library(tidyr)

# dummy data
Ind <- c("Bee1","Bee1","Bee2","Bee2","Bee3")
SNP1 <- c("AA","TT","AT","TT","TT")
SNP2 <- c("GC","GG","GG","CC","GC")
SNP3 <- c("GG","GC","GG","CC","GC")
df <- tibble(Ind,SNP1,SNP2,SNP3)
df
df_filt <- distinct(df, Ind, .keep_all=TRUE)

df_filt

# use tidyr to gather/extract data
df_filt %>% gather(snp, value, -Ind) %>% 
  extract(value, into=c('h1', 'h2'),  '([[A-G]])([[A-G]])')

df_filt %>% gather(snp, value, -Ind) %>% 
  extract(value, into=c('h1', 'h2'),  '([[A-Z]])([[A-Z]])')

df_filt %>% gather(snp, value, -Ind) %>% 
  extract(value, into=c('h1', 'h2'),  '(.)(.)')


# or try this option:
# (when numeric, sep is interpreted as a position in the string)
df_filt %>% gather(snp, value, -Ind) %>% 
  separate(col = value, into = c("h1","h2"), sep=1)
