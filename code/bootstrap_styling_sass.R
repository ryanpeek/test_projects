# new shiny formattings

# SASS: compile Sass to CSS from R

# Bootstraplib: R bindings for bootstrap
# bundles with Rmd, Shiny, etc

library(sass)
#remotes::install_github("rstudio/bootstraplib")
library(bootstraplib)


# Shiny Meta --------------------------------------------------------------

#remotes::install_github("rstudio/shinymeta")
library(shinymeta)
library(shiny)

# need UI
# need server
## Identify domain logic: where is core domain logic to provide to user