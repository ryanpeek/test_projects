# Tutorial on working with APIs
# Some packages to install

install.packages('spocc')
install.packages('taxize')
install.packages('mapr')

# Taxize
# ------------------------------------------------------------

library(taxize)
temp <- gnr_resolve(names = c("Helianthos annus", "Homo saapiens"))
head(temp)

# In this example, we provide a list of species names, some of which are misspelled, and we'll call the API with the tnrs function.

mynames <- c("Helianthus annuus",
             "Pinus contort",
             "Poa anua",
             "Abis magnifica",
             "Rosa california",
             "Festuca arundinace",
             "Sorbus occidentalos",
             "Madia sateva")
tnrs(query = mynames, source = "iPlant_TNRS")[ , -c(5:7)]

# Another common use case is when there are many synonyms for a species. In this
# example, we have three synonyms of the currently accepted name for a species.


mynames <- c("Helianthus annuus ssp. jaegeri",
             "Helianthus annuus ssp. lenticularis",
             "Helianthus annuus ssp. texanus")
(tsn <- get_tsn(mynames, accepted = FALSE))
lapply(tsn, itis_acceptname)


# Another task biologists often face is getting higher taxonomic names for a
# taxa list. Having the higher taxonomy allows you to put into context the
# relationships of your species list.

specieslist <- c("Abies procera",
                 "Pinus contorta")
classification(specieslist, db = 'itis')


# spocc
# ------------------------------------------------------------
library(spocc)
# sopcc stands for species occurrences

# queries the following (yes we know there are overlaps)
# - GBIF
# - Ecoengine
# - iNaturalist
# - Vernet
# - BISON
# - eBird
# - AntWeb
# - iDigBio
# - OBIS


library('spocc')
(df <- occ(query = 'Accipiter striatus',
           from = 'gbif'))

# To view the data
View(df$gbif$data)


df2 <- occ(query = 'Accipiter striatus',
          from = c('gbif', 'ecoengine'),
          limit = 25)

head(df2$gbif$data$Accipiter_striatus)[1:6, 1:10]

# The function occ2df takes a return value from occ
# and makes it into a nice data frame that can used to map points.

head(occ2df(df2))



# mapr
# ------------------------------------------------------------

# install this with install.packages("mapr")

library(mapr)
library(spocc)
spp <- c('Danaus plexippus', 'Accipiter striatus', 'Pinus contorta')
dat <- occ(query = spp, from = 'gbif', has_coords = TRUE)
map_leaflet(dat, dest = ".")

# A ggmap
library(ggmap)
x <- occ(query = 'Lynx rufus californicus', from = 'gbif', limit = 100)
map_ggmap(df2)
map_ggplot(x, "usa")

# ------------------------------------------------------------
