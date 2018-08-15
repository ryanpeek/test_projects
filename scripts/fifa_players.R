# FIFA

# from blog post:
#https://rviews.rstudio.com/2018/06/14/player-data-for-the-2018-fifa-world-cup/?mkt_tok=eyJpIjoiT0RFek5qYzJOekJtTUdJNCIsInQiOiI4WDdVQ2p0dkdUUUY4bGNGUnk0aEpSZWtmSXNrRE96MnFlaFczVGdCQVI1MnJ4WFplNFJhVXdUdU45SHJPVkF2VkZzcG5JU1F2djdMamhQS0F5MHAxU1BXWHhtczVGQVcyM0Eyb00yOWlqdzB6VWJxUmNZaWdcLzJVMWhlUXRnU3cifQ%3D%3D

suppressMessages(library(tidyverse))
library(stringr)
suppressMessages(library(lubridate))
suppressMessages(library(cowplot))

# issues with RJavaTools required following these instructions to install:
# https://gist.github.com/tomsing1/1da54d3f720ed96fbbb5a3f075bd2a56
#install_github("ropenscilabs/tabulizer")
library(tabulizer)
url <- "https://github.com/davidkane9/wc18/raw/master/fifa_player_list_1.pdf"
out <- extract_tables(url, output = "data.frame")

# Note how bind_rows() makes it very easy to combine a list of compatible
# dataframes.

pdf_data <- bind_rows(out) %>% 
  as_tibble() %>% 
  
  # Make the variable names more tidy-like.
  
  rename(team = Team,
         number = X.,
         position = Pos.,
         name = FIFA.Popular.Name,
         birth_date = Birth.Date,
         shirt_name = Shirt.Name,
         club = Club,
         height = Height,
         weight = Weight) %>% 
  
  # Country names are contentious issues. I modify two names because I will
  # later need to merge this tibble with data from Wikipedia, which uses
  # different names.
  
  mutate(team = case_when(
    team == "Korea Republic" ~ "South Korea",
    team == "IR Iran" ~ "Iran",
    TRUE ~ team)) %>% 
  
  # league and club should be separate variables. We also want birth_date to be
  # a date and to have an age variable already calculated.
  
  mutate(birth_date = dmy(birth_date),
         league = str_sub(club, -4, -2),
         club = str_sub(club, end = -7),
         age = interval(birth_date, "2018-06-14") / years(1))


# ERROR CHECK -------------------------------------------------------------


stopifnot(length(unique(pdf_data$team)) == 32)      # There are 32 teams.
stopifnot(all(range(table(pdf_data$team)) == 23))   # Each team has 23 players.
stopifnot(pdf_data %>% 
            filter(position == "GK") %>% 
            group_by(team) %>% 
            tally() %>% 
            filter(n != 3) %>% 
            nrow() == 0)                     # All teams have 3 goal keepers.
stopifnot(all(pdf_data$position %in% 
                c("GK", "DF", "MF", "FW")))  # All players assigned to 1 of 4 positions.


# WIKIPEDIA ---------------------------------------------------------------

suppressMessages(library(rvest))
html <- read_html("https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_squads")

# Once we have read in all the html, we need to identify the location of the
# data we want. The rvest vignette provides guidance, but the key trick is the
# use of SelectorGadget to find the correct CSS node.

# First, we need the country and the shirt number of each player so that we can
# merge this data with that from the PDF.

country <- html_nodes(html, ".mw-headline") %>% 
  html_text() %>%
  as_tibble() %>% 
  filter(! str_detect(value, "Group")) %>% 
  slice(1:32)

number <- html_nodes(html, ".plainrowheaders td:nth-child(1)") %>% 
  html_text()

# We don't need the name of each player but I like to grab it, both because I
# prefer the Wikipedia formatting and to use this as a cross-check on the
# accuracy of our country/number merge.

name <- html_nodes(html, "th a") %>% 
  html_text() %>% 
  as_tibble() %>% 
  filter(! str_detect(value, "^captain$")) %>% 
  slice(1:736)

# cap is the variable we care about, but Wikipedia page also includes the number
# of goals that each player has scored for the national team. Try adding that
# information on your own.

caps <- html_nodes(html, ".plainrowheaders td:nth-child(5)") %>% 
  html_text()

# Create a tibble. Note that we are relying on all the vectors being in the
# correct order.

wiki_data <- tibble(
  number = as.numeric(number),
  name = name$value,
  team = rep(country$value, each = 23),
  caps = as.numeric(caps))

# I prefer the name from Wikipedia. Exercise for the reader: How might we use
# name (from Wikipedia) and shirt_name (from the PDF) to confirm that we have
# lined up the data correctly?

x <- left_join(select(pdf_data, -name), wiki_data, by = c("team", "number"))


x %>% 
  group_by(league) %>% 
  tally() %>% 
  arrange(desc(n))

x %>% 
  group_by(team) %>% 
  summarise(elite = mean(league %in% 
                           c("ENG", "ESP", "GER", "ITA", "FRA"))) %>%
  arrange(desc(elite)) %>% 
  slice(c(1:8, 29:32))
