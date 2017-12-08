# infer package
#https://github.com/andrewpbray/infer

devtools::install_github("andrewpbray/infer")

library(infer)
library(tidyverse)

mtcars <- as.data.frame(mtcars) %>%
  mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))

mtcars %>%
  specify(response = mpg) %>% # alt: mpg ~ NULL (or mpg ~ 1)
  hypothesize(null = "point", mu = 25) %>% 
  generate(reps = 100, type = "bootstrap") %>% 
  calculate(stat = "mean")

# confidence intervals
mtcars %>%
  specify(response = mpg) %>% # alt: mpg ~ NULL (or mpg ~ 1)
  generate(reps = 100, type = "bootstrap") %>% 
  calculate(stat = "mean")



mtcars %>%
  specify(response = mpg) %>% # alt: mpg ~ NULL (or mpg ~ 1)
  hypothesize(null = "point", med = 26) %>% 
  generate(reps = 100, type = "bootstrap") %>% 
  calculate(stat = "median")


mtcars %>%
  specify(response = mpg) %>% # alt: mpg ~ NULL (or mpg ~ 1)
  hypothesize(null = "point", sigma = 5) %>% 
  generate(reps = 100, type = "bootstrap") %>% 
  calculate(stat = "sd")


mtcars %>%
  specify(response = am) %>% # alt: am ~ NULL (or am ~ 1)
  hypothesize(null = "point", p = c("1" = .25)) %>% 
  generate(reps = 100, type = "simulate") %>% 
  calculate(stat = "prop")

mtcars %>%
  specify(am ~ vs) %>% # alt: response = am, explanatory = vs
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in props")


mtcars %>%
  specify(cyl ~ NULL) %>% # alt: response = cyl
  hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25)) %>%
  generate(reps = 100, type = "simulate") %>%
  calculate(stat = "Chisq")


mtcars %>%
  specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "Chisq")


mtcars %>%
  specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in means")


mtcars %>%
  specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in medians")


mtcars %>%
  specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "F")


mtcars %>%
  specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "slope")
