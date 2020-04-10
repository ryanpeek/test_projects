# documenting police shootings nationwide

# see here: https://rud.is/b/2017/10/08/tragic-documentation/


## INSTALL hrbrthemes roboto on macosx: see here: https://github.com/hrbrmstr/hrbrthemes/issues/18
# library(hrbrthemes)
# d <- read.csv(extrafont:::fonttable_file(), stringsAsFactors = FALSE)
# d[grepl("Light", d$FontName),]$FamilyName <- font_rc_light
# write.csv(d,extrafont:::fonttable_file(), row.names = FALSE)
extrafont::loadfonts()

library(httr)
library(rvest)
library(stringi)
library(hrbrthemes)
library(tidyverse)

read.table(sep=":", stringsAsFactors=FALSE, header=TRUE, 
           text="race:description
           W:White, non-Hispanic
           B:Black, non-Hispanic
           H:Hispanic
           N:Native American
           A:Asian
           None:Other/Unknown
           O:Other") -> rdf

wapo_data_url <- "https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv"
shootings_file <- basename(wapo_data_url)
if (!file.exists(shootings_file)) download.file(wapo_data_url, shootings_file)

cols(
  id = col_integer(),
  name = col_character(),
  date = col_date(format = ""),
  manner_of_death = col_character(),
  armed = col_character(),
  age = col_integer(),
  gender = col_character(),
  race = col_character(),
  city = col_character(),
  state = col_character(),
  signs_of_mental_illness = col_character(),
  threat_level = col_character(),
  flee = col_character(),
  body_camera = col_character()
) -> shootings_cols

read_csv(shootings_file, col_types = shootings_cols) %>% 
  mutate(yr = lubridate::year(date), wk = lubridate::week(date)) %>% 
  filter(yr >= 2017) %>% 
  mutate(race = ifelse(is.na(race), "None", race)) %>% 
  mutate(race = ifelse(race=="O", "None", race)) %>% 
  count(race, wk) %>% 
  left_join(rdf, by="race") %>% 
  mutate(description = factor(description, levels=rdf$description)) -> xdf

lod_url <- "https://www.odmp.org/search/year/2017?ref=sidebar"
lod_rds <- "officer_lod.rds"
if (!file.exists(lod_rds)) {
  res <- httr::GET(lod_url)
  write_rds(res, lod_rds)
} else {
  res <- read_rds(lod_rds)
}
pg <- httr::content(res, as="parsed", encoding = "UTF-8")

html_nodes(pg, xpath=".//table[contains(., 'Detective Chad William Parque')]") %>% 
  html_nodes(xpath=".//td[contains(., 'EOW')]") %>% 
  html_text() %>% 
  stri_extract_all_regex("(EOW:[[:space:]]+(.*)\n|Cause of Death:[[:space:]]+(.*)\n)", simplify = TRUE) %>% 
  as_data_frame() %>% 
  mutate_all(~{
    stri_replace_first_regex(.x, "^[[:alpha:][:space:]]+: ", "") %>% 
      stri_trim_both() 
  }
  ) %>% 
  as_data_frame() %>%  
  set_names(c("day", "cause")) %>% 
  mutate(day = as.Date(day, "%A, %B %e, %Y"), wk = lubridate::week(day))%>% 
  count(wk, cause) -> odf 

ggplot(xdf, aes(wk, n)) +
  geom_segment(aes(xend=wk, yend=0)) +
  scale_y_comma(limits=c(0,20)) +
  facet_wrap(~description, scales="free_x") +
  labs(x="2017 Week #", y="# Deaths",
       title="Weekly Fatal Police Shootings in 2017",
       subtitle=sprintf("2017 total: %s", scales::comma(sum(xdf$n))),
       caption="Source: https://www.washingtonpost.com/graphics/national/police-shootings-2017/") +
  theme_ipsum_rc(grid="Y")

count(odf, cause, wt=n, sort=TRUE) -> ordr

mutate(odf, cause = factor(cause, levels=ordr$cause)) %>% 
  ggplot(aes(wk, n)) +
  geom_segment(aes(xend=wk, yend=0)) +
  scale_x_continuous(limits=c(0, 40)) +
  scale_y_comma(limits=c(0,15)) +
  facet_wrap(~cause, ncol=3, scales="free_x") +
  labs(x="2017 Week #", y="# Deaths",
       title="Weekly Officer Line of Duty Deaths in 2017",
       subtitle=sprintf("2017 total: %s", scales::comma(sum(odf$n))),
       caption="Source: https://www.odmp.org/search/year/2017") +
  theme_ipsum_rc(grid="Y")
