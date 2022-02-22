######################################################################
# BCCDC Biostats - Efficient Data Import in R
# Michael Otterstatter, Jan 18, 2019
#
# Much of this material is based on the book 'R for Data Science' by
# Garrett Grolemund and Hadley Wickham
# Freely available online at: https://r4ds.had.co.nz/data-import.html
#
# This script was written to work on the BCCDC Analytics Platform
#
######################################################################

proj.path <- "C:/Users/Michael/Google Drive/presentations/tidy data/scripts"
setwd("proj.path")


library(checkpoint)
checkpoint("2019-01-17")


library(tidyverse)

# look at one of R's built-in datasetss: 'iris'
iris


# create 'tibble' version of mtcars, which is a more modern dataset form 
#  that makes data work easier
# see https://r4ds.had.co.nz/tibbles.html
iris.t <- as_tibble(iris)

iris.t

summary(iris.t)



# subset rows of data using filter()
# see https://r4ds.had.co.nz/transform.html#filter-rows-with-filter
filter(iris.t, Species == 'virginica')

filter(iris.t, Species %in% c('setosa', 'virginica'))

filter(iris.t, Petal.Length > 6.0 & Petal.Width < 2)



# subset columns of data using select()
# see: https://r4ds.had.co.nz/transform.html#select
select(iris.t, Species, Petal.Width)

select(iris.t, -Petal.Width, -Petal.Length)

select(iris.t, contains("Length"))



# add new variables (columns) using mutate()
# see https://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate
mutate(iris.t, Sepal.Area = Sepal.Length * Sepal.Width)

mutate(iris.t, Sepal.Petal.Diff = Sepal.Length - Petal.Length)

mutate(iris.t, Avg.Petal.Length = mean(Petal.Length),
       Dev.from.Avg = Petal.Length - Avg.Petal.Length)



# generate overall and group-level summaries with group_by() and summarise()
# see: https://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise
summarise(iris.t, Avg.Petal.Length = mean(Petal.Length))

iris.grouped <- group_by(iris.t, Species)
summarise(iris.grouped, Avg.Petal.Length = mean(Petal.Length))

iris.grouped <- group_by(iris.t, Species)
filter(iris.grouped, rank(desc(Petal.Length)) < 3)



# Note that operations can be linked together using 
#  the 'piping' operator '%>%'
# see: https://r4ds.had.co.nz/pipes.html
iris.t %>%
  mutate(Dev.from.Average = Petal.Length - mean(Petal.Length)) %>%
  group_by(Species) %>%
  summarise(Avg.Dev = mean(Dev.from.Average))



# reshaping data: gather columns in key-value pairs (wide-to-long) using gather()
iris.t %>%
  select(Species, Sepal.Length, Sepal.Width) %>%
  gather(key = "Sepal.Attribute", value = "Measurement",
         Sepal.Length, Sepal.Width)

iris.t %>%
  gather(key = "Attribute", value = "Measurement",
         Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)



# or the opposite: spread key-value columns apart (long-to-wide) using spread()
iris.gathered <- iris.t %>%
  gather(key = "Attribute", value = "Measurement",
         Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

iris.gathered %>%
  filter(Species == 'setosa' & Attribute == 'Sepal.Length') %>%
  spread(Attribute, Measurement)



# joining datasets using _join() 
# first split example dataset into two parts, as if the petal data and sepal data
#  had been collected separately.  Note that we add a new 'ID' variable (simply the row
#  number) to allow information for the same individual to be joined.  Just for fun, we
#  arrange the new datasets by petal or sepal length to mix up the observations
petal.data <- iris.t %>%
  mutate(ID = row_number()) %>% 
  select(ID, Species, contains("Petal")) %>%
  arrange(Petal.Length)

sepal.data <- iris.t %>%
  mutate(ID = row_number()) %>% 
  select(ID, Species, contains("Sepal")) %>%
  arrange(Sepal.Length)

petal.data
sepal.data


# join the two datasets back together by species and ID so that the
# original information is recovered
left_join(petal.data, sepal.data, by = c("Species", "ID"))

          