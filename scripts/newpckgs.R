# new packages:

# "trackeR"
# "broom"
# "weatherAlert"
# "weatherAlertAreas"
# "ezknitr"
# "rprojroot"
# "MetaLandSim"
# "CatterPlots"
# "ggraph"
# "hrbrthemes"

library(devtools)
install_github("Gibbsdavidl/CatterPlots")
library(CatterPlots)
x <- -10:10
y <- -x^2 + 10
purr <- catplot(xs=x, ys=y, cat=3, catcolor=c(0,1,1,1))
cats(purr, -x, -y, cat=4, catcolor=c(1,0,1,1))
x <- rnorm(n = 25)
y <- rnorm(n = 25)

purr <- catplot(xs=x, ys=y, cat=1)


# Markdown table generator...amazing!
# http://www.tablesgenerator.com/markdown_tables

#Script to scrape NARS website
library(rvest)
library(dplyr)
nars <- read_html("https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys")

links <- nars %>%
	html_nodes(".file-link, a") %>%
	html_attr("href") %>%
	tbl_df() %>%
	filter(grepl("files",value)) %>%
	mutate(url = paste0("https://www.epa.gov",value),
				 file_name = )

for(i in seq_along(links$url)){
	httr::GET(links$url[i],httr::write_disk(basename(links$value[i]),overwrite=T))
}