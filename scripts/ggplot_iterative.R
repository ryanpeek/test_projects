# Intro to plotting with ggplot2: building plots iteratively


# Getting started ---------------------------------------------------------

# First let's download a data set called "portal_data_joined.csv"
# We'll put it in our data folder inside of our project directory.
# If you don't already have a data directory, you can create one in 
# the files pane. Alternatively, you can download the data by hand:
# https://tinyurl.com/y36xgftg

download.file(url = "https://tinyurl.com/y36xgftg", 
              destfile = "data/portal_data_joined.csv")

install.packages("tidyverse")
library(tidyverse)

#surveys <- read_csv("data/portal_data_joined.csv")
surveys <- read_csv("https://tinyurl.com/y36xgftg")
str(surveys)
View(surveys)

ggplot(data=surveys)


library(hexbin)
surveys_plot +
  geom_hex()


# tmaptools::palette_explorer()

library(dpylr)
# Let's see the NAs
survey_weight_nas <- surveys %>%
  filter(is.na(weight))

# Let's filter the NAs
surveys_complete <- surveys %>%
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))

# now we no longer get a warning message because we filtered
# all of the NAs out of our data.
ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

# let's continue plotting with surveys

surveys_plot <- ggplot(data = surveys,
                       mapping = aes(x = weight, y = hindfoot_length))

surveys_plot + geom_point()

# let's build on our plot iteratively to make it better

ggplot(data = surveys, 
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1)

ggplot(data = surveys,
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")


ggplot(data = surveys,
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id))

ggplot(data = surveys,
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id)) +
  scale_color_brewer(palette = "Paired")

# a pallete with 20 colors is Tableau20. 
# I think it's in the ggthemes package

# to add specific colors for your data, add a layer
ggplot(data = iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(color = Species)) +
  scale_color_manual(values = c(setosa = "red", virginica = "blue", versicolor ="green"))

ggplot(surveys, aes(x = species_id, y = weight)) +
  geom_boxplot()

ggplot(surveys, aes(x = species_id, y = weight)) +
  geom_boxplot() +
  geom_jitter(alpha = .3, color = "tomato")

ggplot(surveys, aes(x = species_id, y = weight)) +
  geom_jitter(alpha = .1, color = "tomato") +
  geom_boxplot()

ggplot(surveys, aes(x = species_id, y = weight)) +
  geom_violin()
